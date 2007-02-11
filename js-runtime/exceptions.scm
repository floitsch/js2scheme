(module jsre-exceptions
   (include "macros.sch")
   (export (error-fun msg)
	   (range-error val)
	   (type-error val)))

(define (error-fun msg)
   (lambda L (error #f msg #f)))

(define (range-error val)
   (error #f "range-error" val))

(define (type-error val)
   (error #f "type-error" val))
