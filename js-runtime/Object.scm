(module jsre-Object
   (include "macros.sch")
   (import jsre-object
	   jsre-Function ;; recursive dependency :(
	   jsre-Date
	   jsre-String
	   jsre-Number
	   jsre-Bool
	   jsre-natives
	   jsre-Error
	   jsre-primitives
	   jsre-conversion
	   jsre-global-object
	   jsre-scope-object
	   jsre-globals-tmp
	   )
   (export *js-Object* ;; can be modified by user -> can't be ::Js-Object
	   (js-object-prototype::Js-Object)
	   (Object-init)
	   (js-object-literal properties::pair-nil)))

(define *js-Object* #unspecified)
(define *js-Object-prototype*::Js-Object (js-undeclared))

(define *object-prototype-initialized?* #f)
(define (js-object-prototype)
   (unless *object-prototype-initialized?*
      (let ((proto (instantiate::Js-Object
		      (props (make-props-hashtable))
		      (proto (js-null)))))
	 (set! *js-Object-prototype* proto)
	 (set! *object-prototype-initialized?* #t)))
   *js-Object-prototype*)

(define-method (js-object->string::bstring o::Js-Object)
   "Object")

(define (Object-init)
   (set! *js-Object* (Object-lambda))
   (globals-tmp-add! (lambda () (global-runtime-add! 'Object *js-Object*)))

   (let* ((text-repr "function(v) {/* native Object */ throw 'native'; }")
	  (proc-object (create-function-object *js-Object*
					       (Object-new)
					       Object-construct
					       text-repr))
	  (prototype (js-object-prototype)))

      ;; no need to safe the prototype in *js-object-prototype*. that's already
      ;; done.

      (js-property-generic-set! proc-object ;; 15.2.3
				"length"
				1.0
				(length-attributes))
      (js-property-generic-set! proc-object ;; 15.2.3.1
				"prototype"
				prototype
				(prototype-attributes))
      
      (js-property-generic-set! prototype  ;; 15.2.4.1
				"constructor"
				*js-Object*
				(constructor-attributes))
      (js-property-generic-set! prototype  ;; 15.2.4.2
				"toString"
				(toString)
				(built-in-attributes))
      (js-property-generic-set! prototype  ;; 15.2.4.3
				"toLocalString"
				(toLocalString)
				(built-in-attributes))
      (js-property-generic-set! prototype  ;; 15.2.4.4
				"valueOf"
				(valueOf)
				(built-in-attributes))
      (js-property-generic-set! prototype  ;; 15.2.4.5
				"hasOwnProperty"
				(hasOwnProperty)
				(built-in-attributes))
      (js-property-generic-set! prototype  ;; 15.2.4.6
				"isPrototypeOf"
				(isPrototypeOf)
				(built-in-attributes))
      (js-property-generic-set! prototype  ;; 15.2.4.7
				"propertyIsEnumerable"
				(propertyIsEnumerable)
				(built-in-attributes))))

(define (Object-lambda) ;; 15.2.1.1
   (js-fun-lambda #f
		  this-callee
		  #f
		  (first-arg)
		  (if (or (js-undefined? first-arg)
			  (js-null? first-arg))
		      (js-new this-callee)
		      (any->object first-arg))))

(define (Object-new)  ;; 15.2.2.1
   (js-fun-lambda this
		  #f
		  #f
		  (first-arg)
		  (if (or (js-undefined? first-arg)
			  (js-null? first-arg))
		      this
		      (any->object first-arg))))

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

;; Properties
;; ===================================
(define (toString)      ;; 15.2.4.2
   (js-fun this #f #f "Object.toString"
	   ()
	   (string-append "[object "
			  (js-object->string this)
			  "]")))


(define (toLocalString) ;; 15.2.4.3
   (js-fun this #f #f "Object.toLocalString"
	   ()
	   (js-call (js-property-safe-get this "toString")
		    this)))

(define (valueOf)       ;; 15.2.4.4
   (js-fun this #f #f "Object.valueOf"
	   ()
	   this))

(define (hasOwnProperty) ;; 15.2.4.5
   (js-fun this #f #f "Object.hasOwnProperty"
	   (prop)
	   (let ((s (any->string prop)))
	      (js-property-one-level-contains? this s))))

(define (isPrototypeOf) ;; 15.2.4.6
   (js-fun this #f #f "Object.isPrototypeOf"
	   (other)
	   (cond
	      ((js-object other)
	       => (lambda (o)
		     (let loop ((o o))
			(let ((prototype (Js-Object-proto o)))
			   (cond
			      ((js-null? prototype)
			       #f)
			      ((eq? prototype this)
			       #t)
			      (else
			       (loop prototype)))))))
	      (else
	       #f))))

(define (propertyIsEnumerable) ;; 15.2.4.7
   (js-fun this #f #f "Object.propertyIsEnumerable"
	   (prop)
	   (let ((s (any->string prop)))
	      (js-property-is-enumerable? this s))))
