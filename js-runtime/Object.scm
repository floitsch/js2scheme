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

(define *object-prototype-initialized?* #f)
(define (js-object-prototype)
   (if (not *object-prototype-initialized?*)
       (let ((proto (instantiate::Js-Object
		       (props (make-props-hashtable))
		       (proto *js-Null*))))
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
   (globals-tmp-add! (lambda () (global-add! 'Object *js-Object*)))

   ;; TODO: add other properties (like prototype) ?
   (js-property-direct-set! (procedure-object *js-Object*)
			    "prototype"
			    (instantiate::Property-entry
			       (val (js-object-prototype))
			       (attr (prototype-attribute))))
   (js-property-direct-set! *js-Object-prototype*
			    "toString"
			    (instantiate::Property-entry
			       (val (toString))
			       (attr (built-in-attribute))))
   (js-property-direct-set! *js-Object-prototype*
			    "valueOf"
			    (instantiate::Property-entry
			       (val (valueOf))
			       (attr (built-in-attribute)))))

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
   ;: TODO: completely wrong object-for-in
   (let ((real-o (any->object o)))
      (hashtable-key-list (Js-Object-props real-o))))

;; Properties
;; ===================================
(define (valueOf)
   (js-fun this #f #f ()
	   this))

(define (toString)
   (js-fun this #f #f ()
	   (string-append "["
			  (js-object->string this)
			  "]")))
