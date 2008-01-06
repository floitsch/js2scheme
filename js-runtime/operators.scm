(module jsre-operators
   (include "macros.sch")
   (import jsre-object
	   jsre-natives ;; undefined, null, ...
	   jsre-primitives
	   jsre-Error
	   jsre-Object
	   jsre-Date
	   jsre-Function
	   jsre-String
	   jsre-Number
	   jsre-Bool
	   jsre-conversion
	   jsre-global-object
	   jsre-scope-object
	   jsre-globals-tmp)
   (export
	   (inline jsop-property-delete! obj prop)
	   (inline jsop-typeof v)
	   (inline jsop-unary-- v)
	   (inline jsop-unary-+ v)
	   (inline jsop-~ v)
	   (inline jsop-! v)
	   (inline jsop-* v1 v2)
	   (inline jsop-/ v1 v2)
	   (inline jsop-% v1 v2)
	   (inline jsop-+ v1 v2)
	   (inline jsop-- v1 v2)
	   (inline jsop-<< v1 v2)
	   (inline jsop->> v1 v2)
	   (inline jsop->>> v1 v2)
	   (jsop-< v1 v2)
	   (jsop-> v1 v2)
	   (jsop-<= v1 v2)
	   (jsop->= v1 v2)
	   (jsop-instanceof v1 v2)
	   (jsop-in v1 v2)
	   (inline jsop-!= v1 v2)
	   (inline jsop-== v1 v2)
	   (inline jsop-!== v1 v2)
	   (inline jsop-=== v1 v2)
	   (inline jsop-& v1 v2)
	   (inline jsop-^ v1 v2)
	   (inline jsop-BIT_OR v1 v2)
	   
	   ;; && and || are in macros.sch
	   ;; (macro jsop-&& e1 e2)
	   ;; (macro jsop-OR e1 e2)

	   ;; dummy functions for extract-globals.
	   (jsop-++ dummy)
	   (jsop--- dummy)

	   (inline jsop-any->object expr)
	   (inline jsop-any->number expr)
	   ))

;; base must not be undefined or null (which can only happen for
;; undeclared variables anyways.
(define-inline (jsop-property-delete! base prop)
   ;; mostly similar to js-property-get
   (let ((o-typed (any->object base))
	 (prop-typed (any->string prop)))
      (js-property-safe-delete! o-typed prop-typed)))

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
       (write-circle v)
       (print "*-")
       (error "jsop-typeof" "missed type " v))))

(define-inline (jsop-unary-- v)
   (-fl 0.0 (any->number v)))

(define-inline (jsop-unary-+ v)
   (any->number v))

(define-inline (jsop-~ v)
   (let* ((f (any->int32 v))
          (i (flonum->elong f)))
      (elong->flonum (bit-notelong i))))

(define-inline (jsop-! v)
   (not (any->bool v)))

(define-inline (jsop-* v1 v2)
   (let* ((n1 (any->number v1))
	  (n2 (any->number v2)))
      (*fl n1 n2)))

(define-inline (jsop-/ v1 v2)
   (let* ((n1 (any->number v1))
	  (n2 (any->number v2)))
      (/fl n1 n2)))

(define-inline (jsop-% v1 v2)
   (let* ((n1 (any->number v1))
	  (n2 (any->number v2)))
      (cond
	 ((or (NaN? n1)
	      (NaN? n2)
	      (+infinity? n1)
	      (-infinity? n1)
	      ;; TODO: numbers (+- 0)
	      (=fl n2 0.0))
	  (NaN))
	 ((=fl n1 0.0) ;; TODO: or (-0)
	  n1)
	 (else
	  (let ((tmp (inexact->exact (/fl n1 n2))))
	     (- n1 (* tmp n2)))))))

(define-inline (jsop-+ v1 v2)
   (let* ((lhs (any->primitive v1 #f))
	  (rhs (any->primitive v2 #f)))
      (cond
	 ((string? lhs)
	  (if (string? rhs)
	      (string-append lhs rhs)
	      (string-append lhs (any->string rhs))))
	 ((string? rhs)
	  (string-append (any->string lhs) rhs))
	 (else
	  (let* ((n1 (any->number lhs))
		 (n2 (any->number rhs)))
	     ;; TODO: numbers +- 0
	     (+fl n1 n2))))))

(define-inline (jsop-- v1 v2)
   (let* ((n1 (any->number v1))
	  (n2 (any->number v2)))
      (-fl n1 n2)))

(define-inline (jsop-<< v1 v2)
   ;; 11.7.1
   (let* ((n1 (flonum->elong (any->int32 v1)))
	  (n2 (any->uint32 v2))
	  (by (bit-and #x1F (flonum->fixnum n2))))
      (elong->flonum (bit-lshelong n1 by))))

(define-inline (jsop->> v1 v2)
   ;; 11.7.2
   (let* ((n1 (flonum->elong (any->int32 v1)))
	  (n2 (any->uint32 v2))
	  (by (bit-and #x1F (flonum->fixnum n2))))
      (elong->flonum (bit-rshelong n1 by))))

(define-inline (jsop->>> v1 v2)
   ;; 11.7.3
   (let* ((v1_32 (any->uint32 v1))
	  (n1 (flonum->elong v1_32))
	  (n2 (any->uint32 v2))
	  (by (bit-and #x1F (flonum->fixnum n2))))
      (if (zero? by)
	  v1_32
	  (let* ((tmp (bit-rshelong n1 1))
		 ;; clear bit31 (in case nb was negative)
		 (tmp2 (bit-andelong tmp #ex7FFFFFFF)))
	     (elong->flonum (bit-rshelong tmp2 (-fx by 1)))))))

(define (abstract-rel v1 v2 when-NaN)
   ;; 11.8.5
   (let* ((p1 (any->primitive v1 'number))
	  (p2 (any->primitive v2 'number)))
      (if (and (string? p1) (string? p2))
	  (string<? p1 p2)
	  (let* ((n1 (any->number p1))
		 (n2 (any->number p2)))
	     (cond
		((or (NaN? n1) (NaN? n2))
		 when-NaN)
		(else
		 (<fl n1 n2)))))))

(define (jsop-< v1 v2)
   ;; 11.8.1
   (abstract-rel v1 v2 #f))

(define (jsop-> v1 v2)
   ;; 11.8.2
   (abstract-rel v2 v1 #f))

(define (jsop-<= v1 v2)
   ;; 11.8.3
   (not (abstract-rel v2 v1 #t)))

(define (jsop->= v1 v2)
   ;; 11.8.4
   (not (abstract-rel v1 v2 #t)))

(define (jsop-instanceof v1 v2)
   (unless (procedure? v2)
      (type-error "not a procedure" v2))
   (let ((obj1 (js-object v1)))
      (unless obj1
	 (type-error "not an object" v1))
      (let* ((prototype (js-property-safe-get (js-object v2) "prototype"))
	     (prototype-obj (js-object prototype)))
	 (unless prototype-obj
	    (type-error "prototype is not an object" prototype))
	 (let loop ((obj1 obj1))
	    (let ((proto (Js-Object-proto obj1)))
	       (cond
		  ((js-null? proto)
		   #f)
		  ((eq? proto prototype-obj)
		   #t)
		  (else
		   (loop proto))))))))

(define (jsop-in v1 v2)
   (let ((obj2 (js-object v2)))
      (unless obj2
	 (type-error "not an object" v2))
      (let ((str1 (any->string v1)))
	 (and (js-property-contains obj2 str1)
	      #t))))

(define-inline (jsop-== v1 v2)
   (cond
      ((NaN? v1) #f)
      ((NaN? v2) #f)
      ;; TODO:  -0, +0
      ((eq? v1 v2)
       ;; shortcuts undefined, null, some numbers, some strings, booleans,
       ;; functions
       #t)

      ;; same types:
      ((and (real? v1)         (real? v2))         (=fl v1 v2))
      ((and (string? v1)       (string? v2))       (string=? v1 v2))
      ((and (boolean? v1)      (boolean? v2))      #f) ;; eq? covered other case
      ((and (procedure? v1)    (procedure? v2))    #f) ;; eq? covered other case

      ;; different types:
      ((and (js-null? v1)      (js-undefined? v2)) #t)
      ((and (js-undefined? v1) (js-null? v2))      #t)
      ((and (string? v1)       (real? v2))     (=fl (js-string->number v1) v2))
      ((and (real? v1)         (string? v2))   (=fl v1 (js-string->number v2)))

      ((boolean? v1)
       (if v1 (jsop-== 1.0 v2) (jsop-== 0.0 v2)))
      ((boolean? v2)
       (if v2 (jsop-== v1 1.0) (jsop-== v1 0.0)))

      ((and (or (string? v1) (real? v1))
	    (js-object v2))
       => (lambda (obj)
	     (jsop-== v1
		      (js-object->primitive v2 (if (Js-Date? obj)
						   'string
						   'number)))))
      ((and (or (string? v2) (real? v2))
	    (js-object v1))
       => (lambda (obj)
	     (jsop-== (js-object->primitive v1 (if (Js-Date? obj)
						   'string
						   'number))
		      v2)))
      (else #f)))

(define-inline (jsop-!= v1 v2)
   (not (jsop-== v1 v2)))

(define-inline (jsop-=== v1 v2)
   (cond
      ((eq? v1 v2) #t)
      ((string? v1)
       (and (string? v2)
	    (string=? v1 v2)))
      ((real? v1)
       (and (real? v2)
	    (equal? v1 v2))) ;; TODO: verify. shouldn't eqv? be sufficient
      ;; TODO: handle +-0
      (else
       #f)))

(define-inline (jsop-!== v1 v2)
   (not (jsop-=== v1 v2)))

(define-inline (jsop-& v1 v2)
   ;; 11.10
   (let* ((n1 (flonum->elong (any->int32 v1)))
	  (n2 (flonum->elong (any->int32 v2))))
      (elong->flonum (bit-andelong n1 n2))))

(define-inline (jsop-^ v1 v2)
   ;; 11.10
   (let* ((n1 (flonum->elong (any->int32 v1)))
	  (n2 (flonum->elong (any->int32 v2))))
      (elong->flonum (bit-xorelong n1 n2))))

(define-inline (jsop-BIT_OR v1 v2)
   ;; 11.10
   (let* ((n1 (flonum->elong (any->int32 v1)))
	  (n2 (flonum->elong (any->int32 v2))))
      (elong->flonum (bit-orelong n1 n2))))

;; jsop-++ and jsop--- can't be used directly. We don't have macros either.
;; the whole purpose of the existence of both procedures here is to add them to
;; the runtime-variables.sch file, so that the symbol-pass works as expected.
;; They have to be expanded into x = (any->number x) +/- 1
(define (jsop-++ dummy)
   (error "operators"
	  "++ prefix/postfix must be expanded in compiler."
	  #f))
(define (jsop--- dummy)
   (error "operators"
	  "-- prefix/postfix must be expanded in compiler."
	  #f))

(define-inline (jsop-any->object expr)
   (any->object expr))

(define-inline (jsop-any->number expr)
   (any->number expr))
