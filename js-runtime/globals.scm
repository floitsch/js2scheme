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
   (export jsg-NaN
	   jsg-Infinity
	   jsg-undefined
	   jsg-print
	   jsg-scmprint
	   jsg-eval
	   ))

(define jsg-NaN (NaN))
(global-special-add! 'NaN
		     jsg-NaN
		     (dont-enum-dont-delete-attributes))
(define jsg-Infinity (+infinity))
(global-special-add! 'Infinity
		     jsg-Infinity
		     (dont-enum-dont-delete-attributes))
(define jsg-undefined (js-undefined))
(global-special-add! 'undefined
		     jsg-undefined
		     (dont-enum-dont-delete-attributes))

(define-runtime-globals
   (define (print to-print)
      (print (any->string to-print)))
   (define (scmprint to-print)
      (write-circle to-print)
      (print))
   (define (eval prog)
      (let* ((p (open-input-string prog))
	     (scm-prog (js2scheme p))
	     (res (eval scm-prog)))
	 (print scm-prog)
	 (close-input-port p)
	 res)))
