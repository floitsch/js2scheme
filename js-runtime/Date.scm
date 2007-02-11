(module jsre-Date
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
   (export *js-Date* ;; can be modified by user -> can't be ::Js-Object
	   *js-Date-prototype*::Js-Object
	   (Date-init)))

(define *js-Date* (tmp-js-object))
(define *js-Date-prototype* (tmp-js-object))

(define (Date-init)
   ;; TODO
   'TODO
   )
