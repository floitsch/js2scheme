(module jsre-Object
   (import jsre-base-object
	   jsre-ht-object
	   jsre-property-entry
	   jsre-base-string
	   jsre-undefined)
   (use jsre-Function
	jsre-Date
	jsre-String
	jsre-Number
	jsre-Bool
	jsre-Error
	jsre-conversion
	jsre-global-object
	jsre-scope-object
	)
   (export (final-class NatO-Object::Js-HT-Object)
	   (create-empty-NatO-Object::NatO-Object proto)
	   (new-Object) ;; shortcut for (js-new (global-read *jsg-Object*))
	   *jsg-Object*
	   (natO-object-prototype::NatO-Object)
	   (Object-init)
	   (natO-object-literal properties::pair-nil)))

(define (create-empty-NatO-Object proto)
   (instantiate::NatO-Object
      (props (make-props-hashtable))
      (proto proto)))

(define *jsg-Object* #unspecified)
(define *js-orig-Object* (lambda () (js-null))) ;; going to be replaced soon.

(define *natO-object-prototype*::NatO-Object (NatO-Object-nil))

(define *object-prototype-initialized?* #f)
(define (natO-object-prototype)
   (unless *object-prototype-initialized?*
      (let ((proto (instantiate::NatO-Object
		      (props (make-props-hashtable))
		      (proto (js-null)))))
	 (set! *natO-object-prototype* proto)
	 (set! *object-prototype-initialized?* #t)))
   *natO-object-prototype*)

(define-method (js-class-name::js-string o::NatO-Object)
   (STR "Object"))

(define (Object-init)
   (set! *jsg-Object* (create-runtime-global (STR "Object") (Object-lambda)))
   (set! *js-orig-Object* *jsg-Object*)

   (let* ((text-repr (STR "function(v) {/*native Object*/ throw 'native'; }"))
	  (proc-object (create-function-object (global-read *jsg-Object*)
					       (Object-new)
					       Object-construct
					       text-repr))
	  (prototype (natO-object-prototype)))

      ;; no need to safe the prototype in *natO-object-prototype*. that's already
      ;; done.

      (js-property-generic-set! proc-object ;; 15.2.3
				(STR "length")
				1.0
				(length-attributes))
      (js-property-generic-set! proc-object ;; 15.2.3.1
				(STR "prototype")
				prototype
				(get-Attributes dont-enum
						dont-delete read-only))
      
      (js-property-generic-set! prototype  ;; 15.2.4.1
				(STR "constructor")
				(global-read *jsg-Object*)
				(constructor-attributes))
      (js-property-generic-set! prototype  ;; 15.2.4.2
				(STR "toString")
				(toString)
				(built-in-attributes))
      (js-property-generic-set! prototype  ;; 15.2.4.3
				(STR "toLocalString")
				(toLocalString)
				(built-in-attributes))
      (js-property-generic-set! prototype  ;; 15.2.4.4
				(STR "valueOf")
				(valueOf)
				(built-in-attributes))
      (js-property-generic-set! prototype  ;; 15.2.4.5
				(STR "hasOwnProperty")
				(hasOwnProperty)
				(built-in-attributes))
      (js-property-generic-set! prototype  ;; 15.2.4.6
				(STR "isPrototypeOf")
				(isPrototypeOf)
				(built-in-attributes))
      (js-property-generic-set! prototype  ;; 15.2.4.7
				(STR "propertyIsEnumerable")
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

(define (Object-construct c)
   (create-empty-object-lambda c))

;; simply a shortcut to create an empty Object, as if done by 'new Object'
(define (new-Object)
   (let ((gobj (global-read *jsg-Object*)))
      (if (eq? gobj *js-orig-Object*)
	  (Object-construct (procedure-object *js-orig-Object*))
	  (js-new gobj))))

(define (natO-object-literal properties)
   (let ((o (new-Object)))
      (for-each (lambda (prop)
		   (let ((name (any->js-string (car prop)))
			 (val (cadr prop)))
		      ;; TODO: natO-object-literal can be optimized
		      (js-property-set! o name val)))
		properties)
      o))

;; Properties
;; ===================================
(define (toString)      ;; 15.2.4.2
   (js-fun this #f #f (STR "Object.prototype.toString")
	   ()
	   (js-string-append (STR "[object ")
			     (js-class-name (safe-js-object this))
			     (STR "]"))))


(define (toLocalString) ;; 15.2.4.3
   (js-fun this #f #f (STR "Object.prototype.toLocalString")
	   ()
	   (js-call (js-property-get this (STR "toString"))
		    this)))

(define (valueOf)       ;; 15.2.4.4
   (js-fun this #f #f (STR "Object.prototype.valueOf")
	   ()
	   this))

(define (hasOwnProperty) ;; 15.2.4.5
   (js-fun this #f #f (STR "Object.prototype.hasOwnProperty")
	   (prop)
	   (let ((s (any->js-string prop)))
	      (js-property-one-level-contains? this s))))

(define (isPrototypeOf) ;; 15.2.4.6
   (js-fun this #f #f (STR "Object.prototype.isPrototypeOf")
	   (other)
	   (cond
	      ((js-object other)
	       => (lambda (o)
		     (let loop ((o o))
			(let ((prototype (NatO-Object-proto o)))
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
   (js-fun this #f #f (STR "Object.prototype.propertyIsEnumerable")
	   (prop)
	   (let ((s (any->js-string prop)))
	      (js-property-is-enumerable? this s))))
