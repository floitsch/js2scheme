(module jsre-exceptions
   (include "macros.sch")
   (export (error-fun msg)
	   (range-error val)
	   (type-error val)
	   (undeclared-error id)
	   (syntax-error msg)
	   (eval-error msg)
	   (delete-error msg)))

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

(define (eval-error msg)
   (error #f "eval can't be copied ..." msg))

(define (delete-error msg)
   (error #f "delete error ..." msg))
