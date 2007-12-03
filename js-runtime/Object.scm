(module jsre-Object
   (include "macros.sch")
   (import jsre-object
	   jsre-Function ;; recursive dependency :(
	   jsre-Date
	   jsre-String
	   jsre-Number
	   jsre-Bool
	   jsre-natives
	   jsre-exceptions
	   jsre-primitives
	   jsre-conversion
	   jsre-global-object
	   jsre-scope-object
	   jsre-globals-tmp
	   )
   (export *js-Object* ;; can be modified by user -> can't be ::Js-Object
	   *js-Object-prototype*::Js-Object
	   (js-object-prototype::Js-Object)
	   (Object-init)
	   (js-object-literal properties::pair-nil)
	   (object-for-in-attributes o)))

(define *js-Object* (tmp-js-object))
(define *js-Object-prototype* (tmp-js-object))

(define-method (js-object->string::bstring o::Js-Object)
   "Object")

(define *object-prototype-initialized?* #f)
(define (js-object-prototype)
   (unless *object-prototype-initialized?*
      (let ((proto (instantiate::Js-Object
		      (props (make-props-hashtable))
		      (proto (js-null)))))
	 (set! *js-Object-prototype* proto)
	 (set! *object-prototype-initialized?* #t)))
   *js-Object-prototype*)

(define (Object-init)
   (set! *js-Object* (Object-lambda))
   (register-function-object! *js-Object*
			      (Object-new)
			      Object-construct
			      (js-function-prototype)
			      1 ;; TODO
			      "TODO [native]")
   (globals-tmp-add! (lambda () (global-runtime-add! 'Object *js-Object*)))

   (let ((prototype (js-object-prototype)))
      ;; no need to safe the prototype in *js-object-prototype*. that's already
      ;; done.

      (js-property-generic-set! (procedure-object *js-Object*)
				"prototype"
				prototype
				(prototype-attributes))
      (js-property-generic-set! prototype
				"toString"
				(toString)
				(built-in-attributes))
      (js-property-generic-set! prototype
				"valueOf"
				(valueOf)
				(built-in-attributes))))

(define (Object-lambda)
   (js-fun-lambda #f
		  this-callee
		  #f
		  (first-arg)
		  (if (or (eq? first-arg *js-Undefined*)
			  (eq? first-arg *js-Null*))
		      (js-new this-callee)
		      (any->object first-arg))))

(define (Object-new)
   (lambda (this f . L)
      'do-nothing))

(define (Object-construct c . L)
   (create-empty-object-lambda c))

(define (js-object-literal properties)
   (let ((o (js-new *js-Object*)))
      (for-each (lambda (prop)
		   (let ((name (car prop))
			 (val (cadr prop)))
		      ;; TODO: js-object-literal can be optimized
		      (js-property-safe-set! o name val)))
		properties)
      o))

(define (object-for-in-attributes o)
   ;: TODO: (mostly?) wrong object-for-in
   (let ((real-o (any->object o)))
      (filter (lambda (prop) prop)
	      (hashtable-map (Js-Object-props real-o)
			     (lambda (name prop)
				(let ((attr (Property-entry-attr prop)))
				   (and (Attributes-enumerable attr)
					name)))))))

;; Properties
;; ===================================
(define (valueOf)
   (js-fun this #f #f ()
	   this))

(define (toString)
   (js-fun this #f #f ()
	   (string-append "[object "
			  (js-object->string this)
			  "]")))
