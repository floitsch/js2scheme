(module jsre-operators
   (include "macros.sch")
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
	   jsre-globals-tmp)
   (export (jsop-+ v1 v2)))

(define-macro (define-operator formals . L)
   (let* ((id (car formals))
	  (exported-id (symbol-append 'jsop- id))
	  (exported-formals (cons exported-id (cdr formals))))
      `(define ,exported-formals ,@L)))

(define-operator (+ v1 v2)
   (+ v1 v2))

; (define *runtime-vars* '(Object
; 			 Function
; 			 Array
; 			 String
; 			 Boolean
; 			 Number
; 			 Math
; 			 Date
; 			 RegExp
; 			 Error
; 			 OR && BIT_OR ^ & == != === !== < > <= >= instanceof in
; 			 << >> >>> + - * / % ++ --))
