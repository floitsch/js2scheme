(module jsre-primitive-functions
   (include "macros.sch")
   (import jsre-object
	   jsre-natives ;; undefined, null, ...
	   jsre-exceptions
	   jsre-primitives)
   (export js-print))

(define js-print (js-fun (#f #f to-print) (print to-print)))
