(module jsre-operators
   (include "macros.sch")
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
	   jsre-globals-tmp)
   (export (inline jsop-+ v1 v2)
	   (inline jsop-- v1 v2)
	   (inline jsop-unary-- v)
	   (inline jsop-unary-+ v)
	   (inline jsop-/ v1 v2)
	   (inline jsop-* v1 v2)
	   (inline jsop-% v1 v2)
	   (inline jsop-=== v1 v2)
	   (inline jsop-== v1 v2)
	   (inline jsop-!= v1 v2)
	   (inline jsop-< v1 v2)
	   (inline jsop-> v1 v2)
	   (inline jsop->= v1 v2)
	   (inline jsop-<= v1 v2)
	   (inline jsop-! v)
	   (inline jsop-&& v1 v2)
	   (inline jsop-OR v1 v2)
	   (inline jsop-typeof v)))

;; TODO: operators are not yet spec-conform

(define-inline (jsop-+ v1 v2)
   (if (string? v1)
       (if (string? v2)
	   (string-append v1 v2)
	   (string-append v1 (any->string v2)))
       (if (string? v2)
	   (string-append (any->string v1) v2)
	   (+ v1 v2))))

(define-inline (jsop-- v1 v2)
   (- v1 v2))

(define-inline (jsop-/ v1 v2)
   (/ v1 v2))

(define-inline (jsop-* v1 v2)
   (* v1 v2))

(define-inline (jsop-% v1 v2)
   (let ((tmp (inexact->exact (/ v1 v2))))
      (- v1 (* tmp v2))))

(define-inline (jsop-unary-- v)
   (- v))

(define-inline (jsop-unary-+ v)
   v)

(define-inline (jsop-=== v1 v2)
   (eq? v1 v2))

(define-inline (jsop-== v1 v2)
   (eq? v1 v2))

(define-inline (jsop-!= v1 v2)
   (not (any->bool (eq? v1 v2))))

(define-inline (jsop-< v1 v2)
   (< v1 v2))

(define-inline (jsop-> v1 v2)
   (> v1 v2))

(define-inline (jsop-<= v1 v2)
   (<= v1 v2))

(define-inline (jsop->= v1 v2)
   (>= v1 v2))

(define-inline (jsop-! v)
   (not (any->bool v)))

(define-inline (jsop-&& v1 v2)
   (and (any->bool v1) v2))

(define-inline (jsop-OR v1 v2)
   (or (any->bool v1) v2))

(define-inline (jsop-typeof v)
   (cond
      ((string? v) "string")
      ((number? v) "number")
      ((boolean? v) "boolean")
      ((procedure? v) "function")
      ((eq? *js-Undefined* v) "undefined")
      ((eq? *js-Null* v) "object")
      ((eq? *js-Undeclared* v) "undefined")
      ((Js-Object? v) "object")
      (else (error "jsop-typeof" "missed type " v))))

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
