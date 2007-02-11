(module jsre-Math
   (include "macros.sch")
   (import jsre-object
	   jsre-exceptions
	   jsre-Object
	   jsre-Function
	   jsre-String
	   jsre-Number
	   jsre-Bool
	   jsre-natives
	   jsre-primitives
	   jsre-conversion
	   )
   (export *js-Math* ;; can be modified by user -> can't be ::Js-Object
	   (Math-init)))

(define *js-Math* (tmp-js-object))
(define *js-Math-prototype* (tmp-js-object))

(define (Math-init)
   ;; TODO
   'TODO
   )
