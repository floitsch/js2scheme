(module jsre-Number
   (include "macros.sch")
   (import jsre-object
	   jsre-exceptions
	   jsre-Object
	   jsre-Function
	   jsre-String
	   jsre-Bool
	   jsre-natives
	   jsre-primitives
	   jsre-conversion
	   )
   (export *js-Number* ;; can be modified by user -> can't be ::Js-Object
	   *js-Number-prototype*::Js-Object
	   (Number-init)))

(define *js-Number* (tmp-js-object))
(define *js-Number-prototype* (tmp-js-object))

(define (Number-init)
   ;; TODO
   'TODO
   )
