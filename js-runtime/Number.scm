(module jsre-Number
   (include "macros.sch")
   (import jsre-object
	   jsre-Error
	   jsre-Object
	   jsre-Date
	   jsre-Function
	   jsre-String
	   jsre-Bool
	   jsre-natives
	   jsre-primitives
	   jsre-conversion
	   jsre-global-object
	   jsre-scope-object
	   jsre-globals-tmp
	   )
   (export (class Js-Number::Js-Object
	      (value::double read-only))
	   *js-Number* ;; can be modified by user -> can't be ::procedure
	   *js-Number-orig*::procedure
	   (Number-init)))

(define *js-Number* #unspecified)
(define *js-Number-orig* (lambda () #f))
(define *js-Number-prototype* (tmp-js-object))

(define-method (js-object->string::bstring o::Js-Number)
   "Number")

(define (Number-init)
   (set! *js-Number* (Number-lambda))
   (globals-tmp-add! (lambda () (global-runtime-add! 'Number *js-Number*)))
   
   (let* ((number-object (create-function-object *js-Number*
						 (Number-new)
						 Number-construct
						 "TODO [native]"))
	  (prototype (instantiate::Js-Number       ;; 15.7.4
			(props (make-props-hashtable))
			(proto (js-object-prototype))
			(value 0.0)))) ;; TODO: +0.0
      (set! *js-Number-prototype* prototype)

      (js-property-generic-set! number-object ;; 15.7.3
				"length"
				1.0
				(length-attributes))
      (js-property-generic-set! number-object ;; 15.7.3.1
				"prototype"
				prototype
				(prototype-attributes))
      ;; TODO: MAX_VALUE MIN_VALUE
      (js-property-generic-set! number-object ;; 15.7.3.4
				"NaN"
				(NaN)
				(prototype-attributes))
      (js-property-generic-set! number-object ;; 15.7.3.5
				"NEGATIVE_INFINITY"
				(-infinity)
				(prototype-attributes))
      (js-property-generic-set! number-object ;; 15.7.3.6
				"POSITIVE_INFINITY"
				(+infinity)
				(prototype-attributes))
      
      (js-property-generic-set! prototype     ;; 15.7.4.4
				"valueOf"
				(valueOf)
				(built-in-attributes))))

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

(define (valueOf)
   ;; 15.7.4.4
   (js-fun this #f #f "Number.valueOf"
	   ()
	   (if (not (Js-Number? this))
	       (type-error "Number.valueOf applied to" this)
	       (Js-Number-value this))))
