(module jsre-Bool
   (include "macros.sch")
   (import jsre-natives)
   (use jsre-object
	jsre-Object
	jsre-Date
	jsre-Function
	jsre-String
	jsre-Number
	jsre-Error
	jsre-primitives
	jsre-conversion
	jsre-global-object
	jsre-scope-object
	)
   (export *jsg-Bool*
	   *js-Bool-orig* ; ::procedure ;; Bigloo recently had bugs with types
	   (class Js-Bool::Js-Object
	      val::bool)
	   (Bool-init)))

(define *jsg-Bool* #unspecified)
(define *js-Bool-orig* (lambda () 'to-be-replaced))
(define *js-Bool-prototype*::Js-Object (js-null))

(define-method (js-class-name::bstring o::Js-Bool)
   "Boolean")

(define (Bool-init)
   (set! *js-Bool-orig* (Bool-lambda))
   (set! *jsg-Bool* (create-runtime-global "Boolean" *js-Bool-orig*))

   (let* ((text-repr "function(v) { /* native Boolean */ throw 'native'; }")
	  (bool-object (create-function-object *js-Bool-orig*
					       (Bool-new)
					       Bool-construct
					       text-repr))
	  (prototype (instantiate::Js-Bool ;; 15.6.4
		       (props (make-props-hashtable))
		       (proto (js-object-prototype))
		       (val #f))))
      
      (set! *js-Bool-prototype* prototype)

      (js-property-generic-set! bool-object ;; 15.6.3
				"length"
				1.0
				(length-attributes))
      (js-property-generic-set! bool-object ;; 15.6.3.1
				"prototype"
				prototype
				(get-Attributes dont-enum
						dont-delete read-only))

      (js-property-generic-set! prototype    ;; 15.6.4.1
				"constructor"
				*js-Bool-orig*
				(constructor-attributes))
      (js-property-generic-set! prototype    ;; 15.6.4.2
				"toString"
				(toString)
				(built-in-attributes))
      (js-property-generic-set! prototype    ;; 15.6.4.3
				"valueOf"
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
    (instantiate::Js-Bool
       (props (make-props-hashtable))
       (proto *js-Bool-prototype*)
       (val (any->bool value)))))

(define (Bool-construct f-o::Js-Function)
   #f)

(define (toString)
   ;; 15.6.4.1
   (js-fun this #f #f "Boolean.toString"
	   ()
	   (if (not (Js-Bool? this))
	       (type-error "Bool-toString applied to" this)
	       (let ((val (Js-Bool-val this)))
		  (if val "true" "false")))))

(define (valueOf)
   ;; 15.6.4.3
   (js-fun this #f #f "Boolean.valueOf"
	   ()
	   (if (not (Js-Bool? this))
	       (type-error "Bool-valueOf applied to" this)
	       (Js-Bool-val this))))
