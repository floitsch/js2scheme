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
	   (inline jsop-typeof v)
	   (inline jsop-delete base field)
	   (inline jsop-delete-global v id)
	   (inline jsop-any->object expr)
	   (js-op-with-delete objs id implicit-global)))

;; TODO: operators are not yet spec-conform

(define-inline (jsop-+ v1 v2)
   (cond
      ((string? v1)
       (if (string? v2)
	   (string-append v1 v2)
	   (string-append v1 (any->string v2))))
      ((string? v2)
       (string-append (any->string v1) v2))
      (else
       (+fl (any->number v1) (any->number v2)))))

(define-inline (jsop-- v1 v2)
   (-fl (any->number v1) (any->number v2)))

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
   (equal? v1 v2))

(define-inline (jsop-!= v1 v2)
   ;; TODO
   (not (equal? v1 v2)))

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
      ((js-undefined? v) "undefined")
      ((js-null? v) "object")
      ((js-undeclared? v) "undefined")
      ((Js-Object? v) "object")
      (else
       (print)
       (display "-*")
       (write v)
       (print "*-")
       (error "jsop-typeof" "missed type " v))))

;; base must not be undefined or null (which can only happen for
;; undeclared variables anyways.
(define-inline (jsop-delete base prop)
   ;; mostly similar to js-property-get
   (let ((o-typed (any->object base))
	 (prop-typed (any->string prop)))
      (js-property-safe-delete! o-typed prop-typed)))

(define-inline (jsop-delete-global v id)
   (if (not (js-property-contains *js-global-object* id))
       ;; TODO
       (type-error "can't delete from null")
       (js-property-safe-delete! *js-global-object* id)))

(define-inline (jsop-any->object expr)
   (any->object expr))

;; suppose the following scenario:
;;
;; var x;
;; with (o1) {
;;  with (o2) {
;;    with (o3) {
;;       delete y;
;;       delete x;
;;    }}}
;;
;; in this case y has not been declared, and there exists hence a implicit-y
;; variable. x, on the other hand, has been declared.
;;
;; in this case js-op-with-delete is called as follows:
;;
;;   (js-op-with-delete `(,o3 ,o2 ,o1) "y" implicit-y)
;;   (js-op-with-delete `(,o3 ,o2 ,o1) "x" #f)
(define (js-op-with-delete objs id global)
   (let loop ((objs objs))
      (cond
	 ((and (null? objs)
	       global)
	  (jsop-delete-global global id))
	 ((null? objs)
	  #f) ;; no implicit-global, but a declared variable
	 (else
	  (if (js-property-contains (car objs) id)
	      (jsop-delete (car objs) id)
	      (loop (cdr objs)))))))
   
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
