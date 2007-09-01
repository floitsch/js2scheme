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
   (export *js-Bool* ;; can be modified by user -> can't be ::Js-Object
	   *js-Bool-orig*::Js-Object
	   *js-Bool-prototype*::Js-Object
	   (class Js-Bool::Js-Object
	      val::bool)
	   (Bool-init)))

(define *js-Bool* (tmp-js-object))
(define *js-Bool-orig* (tmp-js-object))
(define *js-Bool-prototype* (tmp-js-object))

(define (Bool-init)
   (set! *js-Bool* Bool-lambda)
   (set! *js-Bool-orig* *js-Bool*)
   (register-function-object! Bool-lambda
			      Bool-new
			      Bool-construct
			      (js-function-prototype) ;; TODO: what's the proto?
			      1
			      "TODO [native]")
   (globals-tmp-add! (lambda () (global-add! 'Boolean *js-Bool*)))
   (let ((bool-object (procedure-object *js-Bool*)))
      (set! *js-Bool-prototype* (instantiate::Js-Bool
				   (props (make-props-hashtable))
				   (proto *js-Object-prototype*)
				   (val #f)))
      (js-property-direct-set! bool-object
			       "prototype"
			       (instantiate::Property-entry
				  (val *js-Bool-prototype*)
				  (attr (DontEnum-DontDelete-ReadOnly-attribute))))))

(define Bool-lambda
   (js-fun-lambda
    #f
    #f
    #f
    (value)
    (any->bool value)))

(define Bool-new
   (js-fun-lambda
    this
    #f
    #f
    (value)
    (Js-Bool-val-set! this (any->bool value))))

(define (Bool-construct::Js-Bool f-o::Js-Function)
   (instantiate::Js-Bool
      (props (make-props-hashtable))
      (proto *js-Bool-prototype*)
      (val #f)))
