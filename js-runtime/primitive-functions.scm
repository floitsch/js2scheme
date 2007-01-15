(module jsre-primitive-functions
   (include "macros.sch")
   (import jsre-object
	   jsre-natives ;; undefined, null, ...
	   jsre-exceptions
	   jsre-primitives
	   jsre-Object
	   jsre-Function
	   jsre-String
	   jsre-Number
	   jsre-Bool
	   jsre-conversion)
   (export js-print
	   (init-js-print)))

(define js-print #f)

(define (init-js-print)
   (print "initting")
   (set! js-print (js-fun (#f #f to-print) (print to-print)))
   (print "done"))
