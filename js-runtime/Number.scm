(module jsre-Number
   (include "macros.sch")
   (import jsre-object
	   jsre-exceptions
	   jsre-Object
	   jsre-Date
	   jsre-Function
	   jsre-String
	   jsre-Bool
	   jsre-natives
	   jsre-primitives
	   jsre-conversion
	   jsre-global-object
	   jsre-globals-tmp
	   )
   (export (class Js-Number::Js-Object
	      (value::double read-only))
	   *js-Number* ;; can be modified by user -> can't be ::procedure
	   *js-Number-orig*::procedure
	   *js-Number-prototype*::Js-Object
	   (Number-init)))

(define *js-Number* #unspecified)
(define *js-Number-orig* (lambda () #f))
(define *js-Number-prototype* (tmp-js-object))

(define (Number-init)
   (set! *js-Number* (Number-lambda))
   (register-function-object! *js-Number*
			      (Number-new)
			      Number-construct
			      (js-function-prototype)
			      1
			      "TODO [native]")
   (globals-tmp-add! (lambda () (global-add! 'Number *js-Number*)))
   ;; TODO: add other properties (like prototype...) ?
   (let ((o (js-object *js-Number*)))
      (js-property-safe-set! o
			     "POSITIVE_INFINITY"
			     (+infinity))))

(define (Number-lambda)
   (js-fun-lambda
    #f
    #f
    (nb-args get-arg)
    ()
    (if (= nb-args 0)
	0.0 ;; TODO should be +0.0
	(any->number (get-arg 0)))))

;; maybe 'new' should construct the object, so 'value' can be constant.
(define (Number-new)
   (js-fun-lambda
    #f
    #f
    (nb-args get-arg)
    ()
    (instantiate::Js-Number
       (props (make-props-hashtable))
       (proto *js-Number-prototype*)
       (value (if (= nb-args 0)
		  0.0 ;; TODO should be +0.0
		  (get-arg 0))))))
   
(define (Number-construct f-o::Js-Function)
   ;; Number-new always returns an Object.
   ;; so we can ignore this one.
   #f)

