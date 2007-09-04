(module jsre-exceptions
   (include "macros.sch")
   (export (error-fun msg)
	   (range-error val)
	   (type-error val)
	   (undeclared-error id)
	   (syntax-error msg)))

(define (error-fun msg)
   (lambda L (error #f msg #f)))

(define (range-error val)
   (error #f "range-error" val))

(define (type-error val)
   (error #f "type-error" val))

(define (undeclared-error id)
   (error #f "variable undeclared" id))

(define (syntax-error msg)
   (error #f "syntax-error" msg))
