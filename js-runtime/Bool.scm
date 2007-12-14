(module jsre-Bool
   (include "macros.sch")
   (import jsre-object
	   jsre-Object
	   jsre-Date
	   jsre-Function
	   jsre-String
	   jsre-Number
	   jsre-natives
	   jsre-Error
	   jsre-primitives
	   jsre-conversion
	   jsre-global-object
	   jsre-scope-object
	   jsre-globals-tmp
	   )
   (export *js-Bool* ;; can be modified by user -> can't be ::procedure
	   *js-Bool-orig*::procedure
	   (class Js-Bool::Js-Object
	      val::bool)
	   (Bool-init)))

(define *js-Bool* #unspecified)
(define *js-Bool-orig* (lambda () #f))
(define *js-Bool-prototype* (tmp-js-object))

(define-method (js-object->string::bstring o::Js-Bool)
   "Boolean")

(define (Bool-init)
   (set! *js-Bool* (Bool-lambda))
   (set! *js-Bool-orig* *js-Bool*)
   (register-function-object! *js-Bool*
			      (Bool-new)
			      Bool-construct
			      (js-function-prototype) ;; TODO: what's the proto?
			      1
			      "TODO [native]")
   (globals-tmp-add! (lambda () (global-runtime-add! 'Boolean *js-Bool*)))
   (let ((bool-object (procedure-object *js-Bool*))
	 (prototype (instantiate::Js-Bool
		       (props (make-props-hashtable))
		       (proto (js-object-prototype))
		       (val #f))))
      (set! *js-Bool-prototype* prototype)
      (js-property-generic-set! bool-object
			       "prototype"
			       prototype
			       (prototype-attributes))
      (js-property-generic-set! prototype
			       "constructor"
			       *js-Bool*
			       (built-in-attributes))
      (js-property-generic-set! prototype
			       "toString"
			       (toString)
			       (built-in-attributes))
      (js-property-generic-set! prototype
			       "valueOf"
			       (valueOf)
			       (built-in-attributes))))

(define (Bool-lambda)
   (js-fun-lambda
    #f
    #f
    #f
    (value)
    (any->bool value)))

(define (Bool-new)
   (js-fun-lambda
    #f
    #f
    #f
    (value)
    (instantiate::Js-Bool
       (props (make-props-hashtable))
       (proto *js-Bool-prototype*)
       (val (any->bool value)))))

(define (Bool-construct f-o::Js-Function)
   #f)

(define (toString)
   (js-fun this #f #f ()
	   (if (not (Js-Bool? this))
	       (type-error (string-append
			    "Bool-toString applied to "
			    (any->safe-string this)))
	       (let ((val (Js-Bool-val this)))
		  (if val "true" "false")))))

(define (valueOf)
   (js-fun this #f #f ()
	   (if (not (Js-Bool? this))
	       (type-error (string-append
			    "Bool-valueOf applied to "
			    (any->safe-string this)))
	       (Js-Bool-val this))))
