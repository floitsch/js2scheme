(module jsre-globals
   (include "macros.sch")
;   (library js2scheme-comp)
   (import jsre-object
	   jsre-natives ;; undefined, null, ...
	   jsre-primitives
	   jsre-exceptions
	   jsre-Object
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
	   jsg-eval))

(define js-print #unspecified)

(define-globals
   (define (print to-print)
      (print to-print))
   (define (eval prog)
      ;(print prog)
      (let ((scm-prog (js2scheme (open-input-string prog))))
	 (print prog)
	 (print scm-prog)
	 (eval scm-prog))))
