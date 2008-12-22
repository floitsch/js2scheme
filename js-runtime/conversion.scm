(module jsre-conversion
   (include "macros.sch")
   (use jsre-object
	jsre-natives ;; undefined, null, ...
	jsre-Error
	jsre-primitives
	jsre-Object
	jsre-Date
	jsre-Function
	jsre-String
	jsre-Number
	jsre-Bool)
   (import jsre-double)
;    (export (inline js-boolify::bool any)
; 	   (inline any->bool::bool any)
; 	   (inline js-string->number str)
; 	   (inline any->number any)
; 	   (inline any->primitive any hint)
; 	   (inline any->integer any)
; 	   (inline any->int32 any)
; 	   (inline any->uint32 any)
; 	   (inline any->uint16 any)
; 	   (inline any->string::bstring any)
; 	   (inline any->object::Js-Object any)
; 	   (js-object any) ;; TODO we really need a better name...
; 	   (js-object->primitive o::Js-Object hint::symbol)))
   (export (js-boolify::bool any)
	   (any->bool::bool any)
	   (js-string->number::double str::bstring)
	   (any->number any)
	   (any->primitive any hint)
	   (finite->integer::double n::double)
	   (any->integer::double any)
	   (any->int32::double any)
	   (any->uint32::double any)
	   (any->uint16::double any)
	   (any->string::bstring any)
	   (any->object any)
	   (js-object any) ;; TODO we really need a better name...
	   (safe-js-object::Js-Object any) ;; TODO we really need a better name...
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

(define (safe-js-object any)
   (if (Js-Object? any)
       any
       (procedure-object any)))

(define (js-boolify::bool any)
   (cond
      ((boolean? any) any)
      ((js-undefined? any) #f)
      ((js-null? any) #f)
      ((string? any) (not (string=? any "")))
      ((flonum? any) ;; TODO
       (and (not (=fl any 0.0))
	    (not (NaN? any))))
      (else #t)))

(define (any->bool::bool any)
   (js-boolify any))

(define (js-string->number str)
   (define whitespace? char-whitespace?)   ;; TODO not spec-conform (white-space)

   (cond-expand
      (bigloo-c (define hex-string->real string->real))
      (else
       (define (hex-string->real str)
	  ;; TODO hex-string->real could be much faster.
	  ;;      In theory we should just take the 53 first bits, and adjust
	  ;;      the exponent accordingly.
	  (let ((str-len (string-length str))
		(val0 (char->integer #\0))
		(val9 (char->integer #\9))
		(vala (char->integer #\a))
		(valf (char->integer #\f))
		(valA (char->integer #\A))
		(valF (char->integer #\F)))
	     (let ((res 0.0)
		   (i 0))
		(if (>= i str-len)
		    res
		    (let* ((c (string-ref str i))
			   (v (char->integer c)))
		       ((and (>=fx v val0) (<= v val9))
			(+fl (*fl res 16.0) (fixnum->flonum (-fx v val0))))
		       ((and (>=fx v vala) (<= v valf))
			(+fl (*fl res 16) (fixnum->flonum (+fx 10 (-fx v vala)))))
		       ((and (>=fx v valA) (<= v valF))
			(+fl (*fl res 16 (fixnum->flonum (+fx 10 (-fx v valA)))))))))))))
		
	  
   
   ;; remove whitespaces
   (define (strip str)
      (let ((str-len (string-length str)))
	 (let loop ((start 0))
	    (cond
	       ((>= start str-len)
		"")
	       ((whitespace? (string-ref str start))
		(loop (+fx start 1)))
	       (else
		(let luup ((end (-fx str-len 1)))
		   (cond
		      ((whitespace? (string-ref str end))
		       (luup (-fx end 1)))
		      ((and (=fx start 0)
			    (=fx end (-fx str-len 1)))
		       str)
		      (else
		       (substring str start (+fx end 1))))))))))

   (define (valid-real? str)
      (let ((str-len (string-length str))
	    (val0 (char->integer #\0))
	    (val9 (char->integer #\9)))
	 (let loop ((i 0)
		    (allow-sign? #t)
		    (allow-dot? #t)
		    (allow-e? #t)
		    (need-digit? #t))
	    (cond
	       ((and (>=fx i str-len)
		     need-digit?)
		#f)
	       ((>=fx i str-len)
		#t)
	       ((and (>=fx (char->integer (string-ref str i)) val0)
		     (<=fx (char->integer (string-ref str i)) val9))
		(loop (+fx i 1)
		      #f
		      allow-dot?
		      allow-e?
		      #f))
	       ((and allow-dot?
		     (char=? #\. (string-ref str i)))
		(loop (+fx i 1)
		      #f
		      #f
		      allow-e?
		      need-digit?))
	       ((and allow-sign?
		     (or (char=? #\+ (string-ref str i))
			 (char=? #\- (string-ref str i))))
		(loop (+fx i 1)
		      #f
		      allow-dot?
		      allow-e?
		      need-digit?))
	       ((and allow-e?
		     (not need-digit?)
		     (or (char=? #\e (string-ref str i))
			 (char=? #\E (string-ref str i))))
		(loop (+fx i 1)
		      #t
		      #f
		      #f
		      #t))
	       (else
		#f)))))

   ;; simplified test: length at least >= 3 and all chars have to be 0-f0-F
   (define (valid-hex-string? str)
      (let ((str-len (string-length str))
	    (val0 (char->integer #\0))
	    (val9 (char->integer #\9))
	    (vala (char->integer #\a))
	    (valf (char->integer #\f))
	    (valA (char->integer #\A))
	    (valF (char->integer #\F)))
	 (and (>fx str-len 2)
	      (char=? #\0 (string-ref str 0))
	      (or (char=? #\x (string-ref str 1))
		  (char=? #\X (string-ref str 1)))
	      (let loop ((i 2))
		 (if (>= i str-len)
		     #t
		     (let ((cval (char->integer (string-ref str i))))
			(if (or (and (>=fx cval val0)
				     (<=fx cval val9))
				(and (>=fx cval vala)
				     (<=fx cval valf))
				(and (>=fx cval valA)
				     (<=fx cval valF)))
			    (loop (+fx i 1))
			    #f)))))))

   (let ((stripped (strip str)))
      (if (string-null? stripped)
	  0.0
	  (cond
	     ((string-null? stripped) 0.0)
	     ((valid-real? stripped)
	      (string->real stripped))
	     ((string=? stripped "+Infinity") +inf.0)
	     ((string=? stripped "-Infinity") -inf.0)
	     ((string=? stripped "Infinity") +inf.0)
	     ((valid-hex-string? stripped)
	      (hex-string->real stripped))
	     (else
	      +nan.0)))))

(define (any->number any)
   (cond
      ((flonum? any) any)
      ((boolean? any) (if any 1.0 0.0)) ;; TODO +0.0
      ((js-undefined? any) +nan.0)
      ((js-null? any) 0.0)
      ((string? any) (js-string->number any))
      ((number? any)
       (warning "exact number in any->number. should not happen" any)
       (if (exact? any) (exact->inexact any) any)) ;; TODO (numbers)
      (else (any->number (any->primitive any 'number)))))

(define (js-object->primitive o::Js-Object hint::symbol)
   (define (toString)
      (let ((toString-prop (js-property-contains o "toString")))
	 (if (procedure? toString-prop)
	     (js-call toString-prop o)
	     o)))
   (define (valueOf)
      (let ((valueOf-prop (js-property-contains o "valueOf")))
	 (if (procedure? valueOf-prop)
	     (js-call valueOf-prop o)
	     o)))
   (case hint
      ((number)
       (let ((valueOf-prim (valueOf)))
	  (if (primitive? valueOf-prim)
	      valueOf-prim
	      (let ((toString-prim (toString)))
		 (if (primitive? toString-prim)
		     toString-prim
		     (type-error "could not convert to primitive type: "
				 o))))))
      ((string)
       (let ((toString-prim (toString)))
	  (if (primitive? toString-prim)
	      toString-prim
	      (let ((valueOf-prim (valueOf)))
		 (if (primitive? valueOf-prim)
		     valueOf-prim
		     (type-error "could not convert to primitive type"
				 o))))))))

;; hint may be either #f, 'string or 'number
(define (any->primitive any hint)
   (cond
      ((js-object any)
       =>
       (lambda (o)
	  (if (and (not hint)
		   (Js-Date? o))
	      (js-object->primitive o 'string)
	      (js-object->primitive o (or hint 'number)))))
      (else any)))

(define (finite->integer n::double)
   (truncatefl n))
   
(define (any->integer any)
   (let ((nb (any->number any)))
      (cond
	 ((NaN? nb) +0.0)
	 ((or (+infinity? nb)
	      (-infinity? nb)
	      (=fl 0.0 nb)) ;; could be -0.0, too.
	  nb)
	 (else
	  (finite->integer nb)))))

(define (any->int32 any)
   (let ((nb (any->number any)))
      (cond
	 ((or (NaN? nb)
	      (+infinity? nb)
	      (-infinity? nb)
	      (=fl 0.0 nb)) ;; could be -0.0, too.
	  +0.0)
	 (else
	  (let* ((tmp (flonum->llong nb))
		 (uint32 (bit-andllong tmp #lxffffffff))) ;; 32 bits
	     (if (>=llong uint32 #lx80000000) ;; 2^31
		 (llong->flonum (-llong uint32 #lx100000000)) ;; 2^32
		 (llong->flonum uint32)))))))

(define (any->uint32 any)
   (let ((nb (any->number any)))
      (cond
	 ((or (NaN? nb)
	      (+infinity? nb)
	      (-infinity? nb)
	      (=fl 0.0 nb)) ;; could be -0.0, too
	  +0.0)
	 (else
	  (let ((tmp (flonum->llong nb)))
	     (llong->flonum
	      (bit-andllong tmp #lxffffffff))))))) ;; 32 bits
   
(define (any->uint16 any)
   (let ((nb (any->number any)))
      (cond
	 ((or (NaN? nb)
	      (+infinity? nb)
	      (-infinity? nb)
	      (=fl 0.0 nb)) ;; could be -0.0, too
	  +0.0)
	 (else
	  (let ((tmp (flonum->llong nb)))
	     (llong->flonum
	      (bit-andllong tmp #lxffff))))))) ;; 16 bits

(define (any->string::bstring any)
   (define (any->string2::bstring any)
      (cond
	 ((string? any) any)
	 ((eq? any *js-Null*) "null")
	 ((eq? any *js-Undefined*) "undefined")
	 ((boolean? any) (if any "true" "false"))
	 ((flonum? any) (double->string any 'shortest 0))
	 (else
	  (with-output-to-string (lambda ()
				    (write-circle any))))))

   (cond
      ((string? any) any)
      ((eq? any *js-Null*) "null")
      ((eq? any *js-Undefined*) "undefined")
      ((boolean? any) (if any "true" "false"))
      ((flonum? any) (double->string any 'shortest 0))
      (else
       (any->string2 (any->primitive any 'string)))))

;; converts 'any' to a JS-object. -> might return a procedure too!
(define (any->object any)
   (cond
      ((or (eq? any *js-Null*)
	   (eq? any *js-Undefined*))
       (type-error "can't convert to object" any))
      ((Js-Object? any) any)
      ((string? any) (js-new *js-String-orig* any))
      ((flonum? any) (js-new *js-Number-orig* any))
      ((procedure? any) any)
      ((boolean? any) (js-new *js-Bool-orig* any))
      ((number? any)
       (warning "exact number in any->object. should not happen" any)
       (if (exact? any) ;; TODO (numbers)
	   (js-new *js-Number-orig* (exact->inexact any))
	   (js-new *js-Number-orig* any)))
      (else
       (type-error "could not convert to object" any))))
