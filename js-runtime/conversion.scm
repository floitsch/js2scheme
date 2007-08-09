(module jsre-conversion
   (include "macros.sch")
   (import jsre-object
	   jsre-natives ;; undefined, null, ...
	   jsre-exceptions
	   jsre-primitives
	   jsre-Object
	   jsre-Date
	   jsre-Function
	   jsre-String
	   jsre-Number
	   jsre-Bool)
   (export (inline js-boolify::bool any)
	   (inline any->bool::bool any)
	   (inline any->number any)
	   (inline any->primitive any hint)
	   (inline any->integer any)
	   (inline any->int32 any)
	   (inline any->uint32 any)
	   (inline any->uint16 any)
	   (inline any->string::bstring any)
	   (inline any->object::Js-Object any)
	   (js-object any) ;; TODO we really need a better name...
	   (js-object->primitive o::Js-Object hint::symbol)))

;; return #f if any is not a javascript object.
;; otherwise the Js-Object
(define (js-object any)
   (cond
      ((or (eq? any *js-Null*)
	   (eq? any *js-Undefined*))
       #f)
      ((Js-Object? any) any)
      ((procedure? any) (procedure-object any))
      (else
       #f)))
   
(define-inline (js-boolify::bool any)
   (cond
      ((boolean? any) any)
      ((eq? any *js-Undefined*) #f)
      ((eq? any *js-Null*) #f)
      ((string? any) (not (string=? any "")))
      ((number? any) ;; TODO
       (and (not (=fl any 0.0))
	    (not (=fl any *NaN*))))
      (else #t)))

(define-inline (any->bool::bool any)
   (js-boolify any))

(define-inline (any->number any)
   (cond
      ((number? any) (if (exact? any) (exact->inexact any) any)) ;; TODO (numbers)
      ((boolean? any) (if any 1.0 0.0)) ;; TODO +0.0
      ((eq? any *js-Undefined*) *NaN*)
      ((eq? any *js-Null*) 0.0)
      ((string? any) (string->number any)) ;; TODO
      (else (any->number (any->primitive any 'number)))))

(define (js-object->primitive o::Js-Object hint::symbol)
   (define (toString)
      (let ((toString-prop (js-property-contains o "toString")))
	 (if (procedure? toString-prop)
	     (js-call toString-prop o))))
   (define (valueOf)
      (let ((valueOf-prop (js-property-contains o "valueOf")))
	 (if (procedure? valueOf-prop)
	     (js-call valueOf-prop o))))
   (case hint
      ((number)
       (let ((valueOf-prim (valueOf)))
	  (if (primitive? valueOf-prim)
	      valueOf-prim
	      (let ((toString-prim (toString)))
		 (if (primitive? toString-prim)
		     toString-prim
		     (type-error "TODO"))))))
      ((string)
       (let ((toString-prim (toString)))
	  (if (primitive? toString-prim)
	      toString-prim
	      (let ((valueOf-prim (valueOf)))
		 (if (primitive? valueOf-prim)
		     valueOf-prim
		     (type-error "TODO"))))))))

;; hint may be either #f, 'string or 'number
(define-inline (any->primitive any hint)
   (cond
      ((js-object any)
       =>
       (lambda (o)
	  (if (and (not hint)
		   (Js-Date? o))
	      (js-object->primitive o 'string)
	      (js-object->primitive o (or hint 'number)))))
      (else any)))

(define-inline (any->integer any)
   (define (sign n)
      (if (>=fl n 0.0)
	  1.0
	  -1.0))
   
   (let ((nb (any->number any)))
      (cond
	 ((=fl *NaN* nb) 0.0)
	 ((or (=fl *+infinity* nb)
	      (=fl *-infinity* nb)
	      (=fl 0.0 nb))
	  nb)
	 (else
	  (*fl (sign nb) (floor nb))))))

(define-inline (any->int32 any)
   (let ((nb (any->number any)))
      (cond
	 ((or (=fl *NaN* nb)
	      (=fl *+infinity* nb)
	      (=fl *-infinity* nb)
	      (=fl 0.0 nb))
	  0)
	 (else
	  ;; TODO
	  (inexact->exact nb)))))

(define-inline (any->uint32 any)
   (let ((nb (any->number any)))
      (cond
	 ((or (=fl *NaN* nb)
	      (=fl *+infinity* nb)
	      (=fl *-infinity* nb)
	      (=fl 0.0 nb))
	  0)
	 (else
	  ;; TODO
	  (inexact->exact nb)))))
   
(define-inline (any->uint16 any)
   (let ((nb (any->number any)))
      (cond
	 ((or (=fl *NaN* nb)
	      (=fl *+infinity* nb)
	      (=fl *-infinity* nb)
	      (=fl 0.0 nb))
	  0)
	 (else
	  ;; TODO
	  0))))

(define-inline (any->string::bstring any)
   (cond
      ((string? any) any)
      ((eq? any *js-Null*) "null")
      ((eq? any *js-Undefined*) "undefined")
      ((boolean? any) (if any
		       "true"
		       "false"))

      ;; TODO: not correct!
      ((number? any) (number->string any))
      ;; any->primitive is supposed to call o.toString or o.toValue
      (else
       (any->string (any->primitive any 'string)))))

(define-inline (any->object::Js-Object any)
   (cond
      ((or (eq? any *js-Null*)
	   (eq? any *js-Undefined*))
       (type-error any))
      ((Js-Object? any) any)
      ((string? any) (js-new *js-String* any))
      ((number? any) (js-new *js-Number* any))
      ((procedure? any) (procedure-object any))
      ((boolean? any) (js-new *js-Bool* any))
      (else
       (type-error any))))
