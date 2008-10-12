(module jsre-Math
   (include "macros.sch")
   (import jsre-object
	   jsre-Error
	   jsre-Object
	   jsre-Date
	   jsre-Function
	   jsre-String
	   jsre-Number
	   jsre-Bool
	   jsre-natives
	   jsre-primitives
	   jsre-conversion
	   jsre-global-object
	   jsre-scope-object
	   jsre-globals-tmp
	   )
   (export (class Js-Math::Js-Object)
	   *js-Math* ;; can be modified by user -> can't be ::Js-Object
	   (Math-init)))

;; 15.8 Math Object

(define *js-Math* #unspecified)

(define-method (js-object->string::bstring o::Js-Math)
   "Math")

(define (Math-init)
   (set! *js-Math* (instantiate::Js-Math
		      (props (make-props-hashtable))
		      (proto (js-object-prototype))))
   
   (globals-tmp-add! (lambda () (global-runtime-add! 'Math *js-Math*)))
   (js-property-generic-set! *js-Math* ;; 15.8.1.1
			     "E"
			     2.71828182845904523536
			     (prototype-attributes)) ;; TODO bad name
   (js-property-generic-set! *js-Math* ;; 15.8.1.2
			     "LN10"
			     2.3025850929940459011
			     (prototype-attributes)) ;; TODO bad name
   (js-property-generic-set! *js-Math* ;; 15.8.1.3
			     "LN2"
			     0.69314718055994528623
			     (prototype-attributes)) ;; TODO bad name
   (js-property-generic-set! *js-Math* ;; 15.8.1.4
			     "LOG2E"
			     1.442695040888963387
			     (prototype-attributes)) ;; TODO bad name
   (js-property-generic-set! *js-Math* ;; 15.8.1.5
			     "LOG10E"
			     0.43429448190325181667
			     (prototype-attributes)) ;; TODO bad name
   (js-property-generic-set! *js-Math* ;; 15.8.1.6
			     "PI"
			     3.14159265358979323846264338327950288419716939937510
			     (prototype-attributes)) ;; TODO bad name
   (js-property-generic-set! *js-Math* ;; 15.8.1.7
			     "SQRT1_2"
			     0.70710678118654757274
			     (prototype-attributes)) ;; TODO bad name
   (js-property-generic-set! *js-Math* ;; 15.8.1.8
			     "SQRT2"
			     1.4142135623730951455
			     (prototype-attributes)) ;; TODO bad name
   (js-property-generic-set! *js-Math*    ;; 15.8.2.1
			     "abs"
			     (abs)
			     (built-in-attributes))
   (js-property-safe-set! *js-Math*
			  "floor"
			  (js-fun #f #f #f "Math.floor"
				  (val)
				  (floor val)))
   (js-property-safe-set! *js-Math*
			  "pow"
			  (js-fun #f #f #f "Math.pow"
				  (x power)
				  (expt x power)))
   (js-property-safe-set! *js-Math*
			  "min"
			  (js-fun #f #f #f "Math.min"
				  (x y)
				  (if (<fl x y)
				      x
				      y)))
   (js-property-safe-set! *js-Math*
			  "max"
			  (js-fun #f #f #f "Math.min"
				  (x y)
				  (if (>fl x y)
				      x
				      y))))

(define (abs)
   (js-fun #f #f #f "Math.abs"
	   (val)
	   (absfl (any->number val))))
