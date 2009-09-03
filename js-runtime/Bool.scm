(module jsre-Bool
   (import jsre-base-object
	   jsre-ht-object
	   jsre-property-entry
	   jsre-base-string)
   (use jsre-undefined
	jsre-Object
	jsre-Date
	jsre-Function
	jsre-String
	jsre-Number
	jsre-Error
	jsre-conversion
	jsre-global-object
	jsre-scope-object
	)
   (export *jsg-Bool*
	   *js-Bool-orig* ; ::procedure ;; Bigloo recently had bugs with types
	   (final-class NatO-Bool::Js-HT-Object
	      val::bool)
	   (Bool-init)))

(define *jsg-Bool* #unspecified)
(define *js-Bool-orig* (lambda () 'to-be-replaced))
(define *js-Bool-prototype*::Js-Object (js-null))

(define-method (js-class-name::js-string o::NatO-Bool)
   (STR "Boolean"))

(define (Bool-init)
   (set! *js-Bool-orig* (Bool-lambda))
   (set! *jsg-Bool* (create-runtime-global (STR "Boolean") *js-Bool-orig*))

   (let* ((text-repr (STR "function(v) { /*native Boolean*/ throw 'native'; }"))
	  (bool-object (create-function-object *js-Bool-orig*
					       (Bool-new)
					       Bool-construct
					       text-repr))
	  (prototype (instantiate::NatO-Bool ;; 15.6.4
		       (props (make-props-hashtable))
		       (proto (natO-object-prototype))
		       (val #f))))
      
      (set! *js-Bool-prototype* prototype)

      (js-property-generic-set! bool-object ;; 15.6.3
				(STR "length")
				1.0
				(length-attributes))
      (js-property-generic-set! bool-object ;; 15.6.3.1
				(STR "prototype")
				prototype
				(get-Attributes dont-enum
						dont-delete read-only))

      (js-property-generic-set! prototype    ;; 15.6.4.1
				(STR "constructor")
				*js-Bool-orig*
				(constructor-attributes))
      (js-property-generic-set! prototype    ;; 15.6.4.2
				(STR "toString")
				(toString)
				(built-in-attributes))
      (js-property-generic-set! prototype    ;; 15.6.4.3
				(STR "valueOf")
				(valueOf)
				(built-in-attributes))))

(define (Bool-lambda)
   ;; 15.6.1.1
   (js-fun-lambda #f #f #f
    (value)
    (any->bool value)))

(define (Bool-new)
   ;; 15.6.2.1
   (js-fun-lambda #f #f #f
    (value)
    (instantiate::NatO-Bool
       (props (make-props-hashtable))
       (proto *js-Bool-prototype*)
       (val (any->bool value)))))

(define (Bool-construct f-o::NatO-Function)
   #f)

(define (toString)
   ;; 15.6.4.1
   (js-fun this #f #f (STR "Boolean.prototype.toString")
	   ()
	   (if (not (NatO-Bool? this))
	       (type-error (STR "Bool-toString applied to") this)
	       (let ((val (NatO-Bool-val this)))
		  (if val (STR "true") (STR "false"))))))

(define (valueOf)
   ;; 15.6.4.3
   (js-fun this #f #f (STR "Boolean.prototype.valueOf")
	   ()
	   (if (not (NatO-Bool? this))
	       (type-error (STR "Bool-valueOf applied to") this)
	       (NatO-Bool-val this))))
