(module jsre-String
   (include "macros.sch")
   (import jsre-object
	   jsre-Object
	   jsre-Date
	   jsre-Function
	   jsre-Number
	   jsre-Bool
	   jsre-natives
	   jsre-exceptions
	   jsre-primitives
	   jsre-conversion
	   )
   (export *js-String* ;; can be modified by user -> can't be ::procedure
	   *js-String-orig*::procedure
	   *js-String-prototype*::Js-Object
	   (String-init)))

(define *js-String* #unspecified)
(define *js-String-orig* (lambda () #f))
(define *js-String-prototype* (tmp-js-object))

(define (String-init)
   ;; TODO
   'TODO
   )
