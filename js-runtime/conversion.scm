(module jsre-conversion
   (include "macros.sch")
   (import jsre-object
	   jsre-natives ;; undefined, null, ...
	   jsre-Error
	   jsre-primitives
	   jsre-Object
	   jsre-Date
	   jsre-Function
	   jsre-String
	   jsre-Number
	   jsre-Bool
	   jsre-global-object
	   jsre-scope-object)
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
	   (js-string->number str)
	   (any->number any)
	   (any->primitive any hint)
	   (any->integer::real any)
	   (any->int32::real any)
	   (any->uint32::real any)
	   (any->uint16::real any)
	   (any->string::bstring any)
	   (any->object::Js-Object any)
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
   
(define (js-boolify::bool any)
   (cond
      ((boolean? any) any)
      ((js-undefined? any) #f)
      ((js-null? any) #f)
      ((string? any) (not (string=? any "")))
      ((real? any) ;; TODO
       (and (not (=fl any 0.0))
	    (not (NaN? any))))
      (else #t)))

(define (any->bool::bool any)
   (js-boolify any))

(define (js-string->number str) ;; TODO: number
   ;; still not completely compliant.

   ;; remove whitespaces
   (define (strip str)
      (let ((str-len (string-length str)))
	 (let loop ((start 0))
	    (cond
	       ((>= start str-len)
		"")
	       ((char-whitespace? (string-ref str start))
		(loop (+fx start 1)))
	       (else
		(let luup ((end (-fx str-len 1)))
		   (cond
		      ((char-whitespace? (string-ref str end))
		       (luup (-fx end 1)))
		      (else
		       (substring str start (+fx end 1))))))))))

   ;; simplified test: length at least >= 3 and all chars have to be a-fA-F
   (define (valid-hex-string? str)
      (let ((str-len (string-length str))
	    (val0 (char->integer #\0))
	    (val9 (char->integer #\9))
	    (vala (char->integer #\a))
	    (valf (char->integer #\f))
	    (valA (char->integer #\A))
	    (valF (char->integer #\F)))
	 (and (>fx str-len 2)
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
	     ((or (string-prefix? "0x" stripped)
		  (string-prefix? "0X" stripped))
	      (if (valid-hex-string? stripped)
		  (string->real stripped)
		  (NaN)))
	     ((or (string=? stripped "+Infinity")
		  (string=? stripped "Infinity"))
	      (+infinity))
	     ((string=? stripped "-Infinity")
	      (-infinity))
	     (else
	      (let ((t (string->number stripped)))
		 (cond
		    ((not t)    (NaN))
		    ((exact? t) (exact->inexact t))
		    (else       t))))))))

(define (any->number any)
   (cond
      ((real? any) any)
      ((boolean? any) (if any 1.0 0.0)) ;; TODO +0.0
      ((js-undefined? any) (NaN))
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

(define (any->integer any)
   (define (sign n)
      (if (>=fl n 0.0)
	  1.0
	  -1.0))

   (let ((nb (any->number any)))
      (cond
	 ((NaN? nb) +0.0)
	 ((or (+infinity? nb)
	      (-infinity? nb)
	      (=fl -0.0 nb)
	      (=fl +0.0 nb))
	  nb)
	 (else
	  (*fl (sign nb) (floor nb))))))

(define (any->int32 any)
   (let ((nb (any->number any)))
      (cond
	 ((or (NaN? nb)
	      (+infinity? nb)
	      (-infinity? nb)
	      (=fl -0.0 nb)
	      (=fl +0.0 nb))
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
	      (=fl -0.0 nb)
	      (=fl +0.0 nb))
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
	      (=fl -0.0 nb)
	      (=fl +0.0 nb))
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
	 ((boolean? any) (if any
			     "true"
			     "false"))
	 ((flonum? any) (double->string any))
	 (else
	  (with-output-to-string (lambda ()
				    (write-circle any))))))

   ;; TODO: not correct!
   (define (double->string::bstring v::double)
      (define (add-e+ str)
	 (let ((pos (string-contains str "e")))
	    (if (and pos
		     (not (char=? (string-ref str (+fx pos 1))
				  #\-)))
		(string-append (substring str 0 (+fx pos 1))
			       "+"
			       (substring str (+fx pos 1)
					  (string-length str)))
		str)))
      (cond
	 ((NaN? v) "NaN")
	 ((<fl v 0.0)
	  (string-append "-" (double->string (-fl 0.0 v))))
	 ((+infinity? v) "Infinity")
	 ((=fl (floorfl v) v)
	  (let* ((str (real->string v))
		 (pos (string-contains str ".0")))
	     (if (not pos)
		 (begin
		    (warning "double->string"
			     "could not find .0"
			     str)
		    str)
		 (add-e+ (string-append (substring str 0 pos)
					(substring str
						   (+fx pos 2)
						   (string-length str)))))))
	 (else
	  (add-e+ (number->string v)))))
   
   (cond
      ((string? any) any)
      ((eq? any *js-Null*) "null")
      ((eq? any *js-Undefined*) "undefined")
      ((boolean? any) (if any
		       "true"
		       "false"))
      ((flonum? any) (double->string any))
      (else
       (any->string2 (any->primitive any 'string)))))

(define (any->object::Js-Object any)
   (cond
      ((or (eq? any *js-Null*)
	   (eq? any *js-Undefined*))
       (type-error "can't convert to object" any))
      ((Js-Object? any) any)
      ((string? any) (js-new *js-String-orig* any))
      ((real? any) (js-new *js-Number-orig* any))
      ((procedure? any) (procedure-object any))
      ((boolean? any) (js-new *js-Bool-orig* any))
      ((number? any)
       (warning "exact number in any->object. should not happen" any)
       (if (exact? any) ;; TODO (numbers)
	   (js-new *js-Number-orig* (exact->inexact any))
	   (js-new *js-Number-orig* any)))
      (else
       (type-error "could not convert to object" any))))
