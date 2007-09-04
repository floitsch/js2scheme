(module jsre-Bool
   (include "macros.sch")
   (import jsre-object
	   jsre-Object
	   jsre-Date
	   jsre-Function
	   jsre-String
	   jsre-Number
	   jsre-natives
	   jsre-exceptions
	   jsre-primitives
	   jsre-conversion
	   jsre-global-object
	   jsre-globals-tmp
	   )
   (export *js-Bool* ;; can be modified by user -> can't be ::procedure
	   *js-Bool-orig*::procedure
	   *js-Bool-prototype*::Js-Object
	   (class Js-Bool::Js-Object
	      val::bool)
	   (Bool-init)))

(define *js-Bool* #unspecified)
(define *js-Bool-orig* (lambda () #f))
(define *js-Bool-prototype* (tmp-js-object))

(define (Bool-init)
   (set! *js-Bool* (Bool-lambda))
   (set! *js-Bool-orig* *js-Bool*)
   (register-function-object! *js-Bool*
			      (Bool-new)
			      Bool-construct
			      (js-function-prototype) ;; TODO: what's the proto?
			      1
			      "TODO [native]")
   (globals-tmp-add! (lambda () (global-add! 'Boolean *js-Bool*)))
   (let ((bool-object (procedure-object *js-Bool*))
	 (prototype (instantiate::Js-Bool
		       (props (make-props-hashtable))
		       (proto *js-Object-prototype*)
		       (val #f))))
      (set! *js-Bool-prototype* prototype)
      (js-property-direct-set! bool-object
			       "prototype"
			       (instantiate::Property-entry
				  (val prototype)
				  (attr (prototype-attribute))))
      (js-property-direct-set! prototype
			       "toString"
			       (instantiate::Property-entry
				  (val (toString))
				  (attr (built-in-attribute))))
      (js-property-direct-set! prototype
			       "valueOf"
			       (instantiate::Property-entry
				  (val (valueOf))
				  (attr (built-in-attribute))))))

(define (Bool-lambda)
   (js-fun-lambda
    #f
    #f
    #f
    (value)
    (any->bool value)))

(define (Bool-new)
   (js-fun-lambda
    this
    #f
    #f
    (value)
    (Js-Bool-val-set! this (any->bool value))))

(define (Bool-construct::Js-Bool f-o::Js-Function)
   (instantiate::Js-Bool
      (props (make-props-hashtable))
      ;; Bool-prototype can not be changed.
      (proto *js-Bool-prototype*)
      (val #f)))

(define (toString)
   (js-fun this #f #f ()
	   (if (not (Js-Bool? this))
	       (type-error (with-output-to-string
			      (lambda () (display-circle this))))
	       (let ((val (Js-Bool-val this)))
		  (if val "true" "false")))))

(define (valueOf)
   (js-fun this #f #f ()
	   (if (not (Js-Bool? this))
	       (type-error "TODO")
	       (Js-Bool-val this))))
