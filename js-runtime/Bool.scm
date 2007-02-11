(module jsre-Bool
   (include "macros.sch")
   (import jsre-object
	   jsre-Object
	   jsre-Function
	   jsre-String
	   jsre-Number
	   jsre-natives
	   jsre-exceptions
	   jsre-primitives
	   jsre-conversion
	   )
   (export *js-Bool* ;; can be modified by user -> can't be ::Js-Object
	   *js-Bool-prototype*::Js-Object
	   (Bool-init)))

(define *js-Bool* (tmp-js-object))
(define *js-Bool-prototype* (tmp-js-object))

(define (Bool-init)
   ;; TODO
   'TODO
   )
