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

(define *js-Math* (tmp-js-object))

(define (Math-init)
   (set! *js-Math* (instantiate::Js-Math
		      (props (make-props-hashtable))
		      (proto (js-object-prototype))))
   
   (globals-tmp-add! (lambda () (global-runtime-add! 'Math *js-Math*)))
   (js-property-safe-set! *js-Math*
			  "abs"
			  (js-fun #f #f #f (val)
				  (abs val)))
   (js-property-safe-set! *js-Math*
			  "floor"
			  (js-fun #f #f #f (val)
				  (floor val)))
   (js-property-safe-set! *js-Math*
			  "pow"
			  (js-fun #f #f #f (x power)
				  (expt x power))))
