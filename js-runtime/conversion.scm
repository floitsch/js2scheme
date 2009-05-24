(module jsre-conversion
   (import jsre-double
	   jsre-base-string)
   (use jsre-base-object
	jsre-natives ;; undefined
	jsre-Error
	jsre-primitives
	jsre-Object
	jsre-Date
	jsre-Function
	jsre-String
	jsre-Number
	jsre-Bool)
;    (export (inline js-boolify::bool any)
; 	   (inline any->bool::bool any)
; 	   (inline js-string->number str)
; 	   (inline any->number any)
; 	   (inline any->primitive any hint)
; 	   (inline any->integer any)
; 	   (inline any->int32 any)
; 	   (inline any->uint32 any)
; 	   (inline any->uint16 any)
; 	   (inline any->js-string::bstring any)
; 	   (inline any->object::Js-Object any)
; 	   (js-object any) ;; TODO we really need a better name...
; 	   (js-object->primitive o::Js-Object hint::symbol)))
   (export (js-boolify::bool any)
	   (any->bool::bool any)
	   (js-string->number::double str::Js-Base-String)
	   (any->number any)
	   (any->primitive any hint)
	   (finite->integer::double n::double)
	   (any->integer::double any)
	   (any->int32::double any)
	   (any->uint32::double any)

	   ;; char/string conversions
	   (any->uint10FFFF-fx::long any)
	   (any->uint16fx::ucs2 any)
	   (any->uint8fx::char any)

	   (any->js-string::Js-Base-String any)
	   (any->object any)
	   (js-object any) ;; TODO we really need a better name...
	   (safe-js-object::Js-Object any) ;; TODO we really need a better name...
	   (js-object->primitive o::Js-Object hint::symbol)))

;; return #f if any is not a javascript object.
;; otherwise the Js-Object
(define (js-object any)
   (cond
      ((or (js-null? any)
	   (js-undefined? any))
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
      ((js-string? any) (not (js-string-null? any)))
      ((flonum? any)
       (and (not (=fl any 0.0))
	    (not (nanfl? any))))
      (else #t)))

(define (any->bool::bool any)
   (js-boolify any))

(define (js-string->number str)
   (define whitespace? js-char-whitespace?)   ;; TODO not spec-conform (white-space)

   ;; TODO: in C-mode we could theoretically use 'string->real' if the
   ;; base-string has a fitting representation.
   ;; TODO hex-string->real could be much faster.
   ;;      In theory we should just take the 53 first bits, and adjust
   ;;      the exponent accordingly.
   (define (hex-string->real str)
      (let ((str-len (js-string-length str))
	    (val0 (char->integer #\0))
	    (val9 (char->integer #\9))
	    (vala (char->integer #\a))
	    (valf (char->integer #\f))
	    (valA (char->integer #\A))
	    (valF (char->integer #\F)))
	 (let loop ((i 2) ;; skip the "0x"
		    (res 0.0))
	    (if (>= i str-len)
		res
		(let* ((c (js-string-ref str i))
		       (v (js-char->integer c)))
		   (cond
		      ((and (>=fx v val0) (<=fx v val9))
		       (loop (+fx i 1)
			     (+fl (*fl res 16.0)
				  (fixnum->flonum (-fx v val0)))))
		      ((and (>=fx v vala) (<=fx v valf))
		       (loop (+fx i 1)
			     (+fl (*fl res 16.0)
				  (fixnum->flonum (+fx 10 (-fx v vala))))))
		      ((and (>=fx v valA) (<=fx v valF))
		       (loop (+fx i 1)
			     (+fl (*fl res 16.0)
				  (fixnum->flonum (+fx 10 (-fx v valA))))))
		      (else
		       (error 'conversion
			      "hex-string->real received non-hex string"
			      str))))))))
   
   ;; remove whitespaces
   (define (strip str)
      (let ((str-len (js-string-length str)))
	 (let loop ((start 0))
	    (cond
	       ((>= start str-len)
		(STR ""))
	       ((whitespace? (js-string-ref str start))
		(loop (+fx start 1)))
	       (else
		(let luup ((end (-fx str-len 1)))
		   (cond
		      ((whitespace? (js-string-ref str end))
		       (luup (-fx end 1)))
		      ((and (=fx start 0)
			    (=fx end (-fx str-len 1)))
		       str)
		      (else
		       (js-substring str start (+fx end 1))))))))))

   (define (valid-real? str)
      (let ((str-len (js-string-length str))
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
	       ((and (>=fx (js-char->integer (js-string-ref str i)) val0)
		     (<=fx (js-char->integer (js-string-ref str i)) val9))
		(loop (+fx i 1)
		      #f
		      allow-dot?
		      allow-e?
		      #f))
	       ((and allow-dot?
		     (char=js-char? #\. (js-string-ref str i)))
		(loop (+fx i 1)
		      #f
		      #f
		      allow-e?
		      need-digit?))
	       ((and allow-sign?
		     (or (char=js-char? #\+ (js-string-ref str i))
			 (char=js-char? #\- (js-string-ref str i))))
		(loop (+fx i 1)
		      #f
		      allow-dot?
		      allow-e?
		      need-digit?))
	       ((and allow-e?
		     (not need-digit?)
		     (or (char=js-char? #\e (js-string-ref str i))
			 (char=js-char? #\E (js-string-ref str i))))
		(loop (+fx i 1)
		      #t
		      #f
		      #f
		      #t))
	       (else
		#f)))))

   ;; simplified test: length at least >= 3 and all chars have to be 0-f0-F
   (define (valid-hex-string? str)
      (let ((str-len (js-string-length str))
	    (val0 (char->integer #\0))
	    (val9 (char->integer #\9))
	    (vala (char->integer #\a))
	    (valf (char->integer #\f))
	    (valA (char->integer #\A))
	    (valF (char->integer #\F)))
	 (and (>fx str-len 2)
	      (char=js-char? #\0 (js-string-ref str 0))
	      (or (char=js-char? #\x (js-string-ref str 1))
		  (char=js-char? #\X (js-string-ref str 1)))
	      (let loop ((i 2))
		 (if (>= i str-len)
		     #t
		     (let ((cval (js-char->integer (js-string-ref str i))))
			(if (or (and (>=fx cval val0)
				     (<=fx cval val9))
				(and (>=fx cval vala)
				     (<=fx cval valf))
				(and (>=fx cval valA)
				     (<=fx cval valF)))
			    (loop (+fx i 1))
			    #f)))))))

   (let* ((stripped (strip str)))
      (cond
	 ((js-string-null? stripped) 0.0)
	 ((valid-real? stripped)
	  (js-string->real stripped))
	 ((js-string=utf8? stripped "+Infinity") +inf.0)
	 ((js-string=utf8? stripped "-Infinity") -inf.0)
	 ((js-string=utf8? stripped "Infinity") +inf.0)
	 ((valid-hex-string? stripped)
	  (hex-string->real stripped))
	 (else
	  +nan.0))))

(define (any->number any)
   (cond
      ((flonum? any) any)
      ((boolean? any) (if any 1.0 0.0)) ;; TODO +0.0
      ((js-undefined? any) +nan.0)
      ((js-null? any) 0.0)
      ((js-string? any) (js-string->number any))
      ((number? any)
       (warning "exact number in any->number. should not happen" any)
       (if (exact? any) (exact->inexact any) any)) ;; TODO (numbers)
      (else (any->number (any->primitive any 'number)))))

(define (js-object->primitive o::Js-Object hint::symbol)
   (define (primitive? v) (not (Js-Object? v)))

   (define (toString)
      (let ((toString-prop (js-property-contains o (STR "toString"))))
	 (if (procedure? toString-prop)
	     (js-call toString-prop o)
	     o)))
   (define (valueOf)
      (let ((valueOf-prop (js-property-contains o (STR "valueOf"))))
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
		     (type-error (STR "could not convert to primitive type")
				 o))))))
      ((string)
       (let ((toString-prim (toString)))
	  (if (primitive? toString-prim)
	      toString-prim
	      (let ((valueOf-prim (valueOf)))
		 (if (primitive? valueOf-prim)
		     valueOf-prim
		     (type-error (STR "could not convert to primitive type")
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
	 ((nanfl? nb) +0.0)
	 ((or (infinitefl? nb)
	      (=fl 0.0 nb)) ;; could be -0.0, too.
	  nb)
	 (else
	  (finite->integer nb)))))

;; nb must not be nan, 0.0 or infinite.
(define (safe->uint32 nb::float)
   (let* ((tmp (truncatefl nb))
	  (two^32 4294967296.0)
	  (rem (remainderfl tmp two^32)))
      ;; remainderfl returns negative values for negative input.
      ;; fix this.
      (if (<fl rem 0.0)
	  (+fl rem two^32)
	  rem)))
   
(define (any->int32 any)
   ;; TODO: inefficient?
   (let ((nb (any->number any)))
      (cond
	 ((or (nanfl? nb)
	      (infinitefl? nb)
	      (=fl 0.0 nb)) ;; could be -0.0, too.
	  +0.0)
	 (else
	  (let ((tmp (safe->uint32 nb))
		(two^32 4294967296.0)
		(two^31 2147483648.0))
	     (if (>=fl nb two^31)
		 (-fl nb two^32)
		 nb))))))

(define (any->uint32 any)
   ;; TODO: inefficient
   (let ((nb (any->number any)))
      (cond
	 ((or (nanfl? nb)
	      (infinitefl? nb)
	      (=fl 0.0 nb)) ;; could be -0.0, too
	  +0.0)
	 (else
	  (safe->uint32 nb)))))

;; might return a negative number.
;; nb must not be nan, infinite or 0.0 (although that probably does not matter)
;; if nb does not fit into nb, then it is cut by an arbitrary limit. However,
;;    we assure that the cut-off is at least 2^24 and on a bit-boundary.
;;    In other words: the least important bits are untouched.
(define (flonum->truncated-fixnum nb)
   (define limit 16777216.0)
   [assert (limit) (> (maxvalfx) limit)]

   (if (=fl nb (fixnum->flonum (flonum->fixnum nb)))
       ;; shortcut: a valid integer-number.
       (flonum->fixnum nb)
       (flonum->fixnum (remainderfl (truncatefl nb) limit))))

;; the following functions are (should) only be used for character creations.
;; they return fixnums (bints/longs)
;; The ECMA spec requires characters to be 16 bit long. The "real" conversion
;; function would hence be 'any->uint16fx'. The others are used by "incorrect"
;; string implementations.
(define (any->uint8fx::char any)
   (let ((nb (any->number any)))
      (cond
	 ((or (nanfl? nb)
	      (infinitefl? nb)
	      (=fl 0.0 nb)) ;; covers -0.0 too.
	  #a000)
	 (else
	  (integer->char (bit-and #xFF (flonum->truncated-fixnum nb)))))))

(define (any->uint16fx::ucs2 any)
   (let ((nb (any->number any)))
      (cond
	 ((or (nanfl? nb)
	      (infinitefl? nb)
	      (=fl 0.0 nb)) ;; covers -0.0 too.
	  #u0000)
	 (else
	  (integer->ucs2 (bit-and #xFFFF (flonum->truncated-fixnum nb)))))))

;; this one should be used for utf32 (or similar implementations)
;; it returns the unicode-value of the given number.
;; 
(define (any->uint10FFFF-fx::long any)
   (let ((nb (any->number any)))
      (cond
	 ((or (nanfl? nb)
	      (infinitefl? nb)
	      (=fl 0.0 nb)) ;; covers -0.0 too.
	  #u0000)
	 (else
	  (let ((tr (flonum->truncated-fixnum nb)))
	     ;; most of the time tr will be < #x10FFFF anyways.
	     (modulofx (bit-and #xFFFFFF tr) #x10FFFF))))))

(define (any->js-string::Js-Base-String any)
   (define (any->string2::Js-Base-String any)
      (cond
	 ((js-string? any) any)
	 ((js-null? any) (STR "null"))
	 ((js-undefined? any) (STR "undefined"))
	 ((boolean? any) (if any (STR "true") (STR "false")))
	 ((flonum? any) (utf8->js-string
			 (double->string any 'shortest 0)))
	 ((string? any)
	  (warning "any->js-string on utf8-string" (utf8->js-string any)))
	 (else
	  (utf8->js-string
	   (with-output-to-string (lambda ()
				     (write-circle any)))))))

   (cond
      ((js-string? any) any)
      ((js-null? any) (STR "null"))
      ((js-undefined? any) (STR "undefined"))
      ((boolean? any) (if any (STR "true") (STR "false")))
      ((flonum? any) (utf8->js-string (double->string any 'shortest 0)))
      ((string? any)
	  (warning "any->js-string on utf8-string" (utf8->js-string any)))
      (else
       (any->string2 (any->primitive any 'string)))))

;; converts 'any' to a JS-object. -> might return a procedure too!
(define (any->object any)
   (cond
      ((or (js-null? any)
	   (js-undefined? any))
       (type-error (STR "can't convert to object") any))
      ((Js-Object? any) any)
      ((js-string? any) (js-new *js-String-orig* any))
      ((flonum? any) (js-new *js-Number-orig* any))
      ((procedure? any) any)
      ((boolean? any) (js-new *js-Bool-orig* any))
      ((number? any)
       (warning "exact number in any->object. should not happen" any)
       (if (exact? any) ;; TODO (numbers)
	   (js-new *js-Number-orig* (exact->inexact any))
	   (js-new *js-Number-orig* any)))
      (else
       (type-error (STR "could not convert to object") any))))
