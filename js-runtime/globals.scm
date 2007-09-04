(module jsre-globals
   (include "macros.sch")
;   (library js2scheme-comp)
   (import jsre-object
	   jsre-natives ;; undefined, null, ...
	   jsre-primitives
	   jsre-exceptions
	   jsre-Object
	   jsre-Date
	   jsre-Function
	   jsre-String
	   jsre-Number
	   jsre-Bool
	   jsre-conversion
	   jsre-global-object
	   jsre-globals-tmp
	   ;js2scheme-comp
	   )
   (export jsg-print
	   jsg-scmprint
	   jsg-eval))

(define js-print #unspecified)

(define-globals
   (define (print to-print)
      (print (any->string to-print)))
   (define (scmprint to-print)
      (write-circle to-print)
      (print))
   (define (eval prog)
      (let ((scm-prog (js2scheme (open-input-string prog))))
	 (eval scm-prog))))
