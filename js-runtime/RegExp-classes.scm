(module jsre-RegExp-classes
   (static RegExp-class
	   ;; HACK does not work on 64 bit machines.
	   (l::llong (default #l0))
	   (u::llong (default #l0))) ;; on 32bit machines 128 bit.
   (export
    (RegExp-class-condition char/class
			    case-sensitive?)
    (inline RegExp-match-c c class)))

;; TODO: classes should have precomputed ranges for white-space...
;       ((:any) (not (char=? c #\newline)))
;       ;
;       ((:alnum) (or (char-alphabetic? c) (char-numeric? c)))
;       ((:alpha) (char-alphabetic? c))
;       ((:ascii) (< (char->integer c) 128))
;       ((:blank) (or (char=? c #\space) (char=? c *pregexp-tab-char*)))
;       ((:cntrl) (< (char->integer c) 32))
;       ((:digit) (char-numeric? c))
;       ((:graph) (and (>= (char->integer c) 32)
;                      (not (char-whitespace? c))))
;       ((:lower) (char-lower-case? c))
;       ((:print) (>= (char->integer c) 32))
;       ((:punct) (and (>= (char->integer c) 32)
;                      (not (char-whitespace? c))
;                      (not (char-alphabetic? c))
;                      (not (char-numeric? c))))
;       ((:space) (char-whitespace? c))
;       ((:upper) (char-upper-case? c))
;       ((:word) (or (char-alphabetic? c)
;                    (char-numeric? c)
;                    (char=? c #\_)))
;       ((:xdigit) (or (char-numeric? c)
;                      (char-ci=? c #\a) (char-ci=? c #\b)
;                      (char-ci=? c #\c) (char-ci=? c #\d)
;                      (char-ci=? c #\e) (char-ci=? c #\f)))

(define-inline (RegExp-match-c c class)
   (with-access::RegExp-class class (l u)
      (let ((cn (char->integer c)))
	 (if (< cn 64)
	     (let ((mask (make-llong (bit-lsh 1 cn))))
		(not (=llong #l0 (bit-andllong l mask))))
	     (let ((mask (make-llong (bit-lsh 1 (-fx cn 64)))))
		(not (=llong #l0 (bit-andllong u mask))))))))

(define (class-add-bit! class n)
   (with-access::RegExp-class class (l u)
      (if (< n 64)
	  (set! l (bit-orllong l (make-llong (bit-lsh 1 cn))))
	  (set! u (bit-orllong u (make-llong (bit-lsh 1 (- cn 64))))))))

(define (get-single-bit class)
   (define (get-bit-pos i::llong)
      (let loop ((j 1)
		 (k (make-llong 1)))
	 (if (not (= 0 (bit-andllong i k)))
	     j
	     (loop (+ j 1) (*llong k 2)))))
   
   (with-access::RegExp-class class (l u)
      (if (=llong l #l0)
	  (+fx 64 (get-bit-pos u))
	  (get-bit-pos l))))

(define (nb-bits class)
   (define (nb-bits i::llong res::bint)
      (if (= i 0)
	  res
	  (nb-bits (bit-andllong (- i 1) i)
		   (+fx res 1))))
   (with-access::RegExp-class class (l u)
      (+ (nb-bits l 0) (nb-bits u 0))))

;; For now just build a range based on ASCII chars.
(define (RegExp-class-condition char/class case-sensitive?)
   (define (inverted f class . Lrest)
      (let ((tmp (instantiate::RegExp-class)))
	 (apply f tmp Lrest)
	 (class-invert! tmp)
	 (class-add-class class tmp)))

   ;; does *not* take into account case-sensitivity!
   (define (add-chars class .  Lc)
      (for-each (lambda (c) (class-add-bit! class (char->integer c)))
		Lc))

   (define (add-char class c)
      (if case-sensitive?
	  (class-add-bit! class (char->integer c))
	  ;; HACK: case-sensitivity.
	  (begin
	     (class-add-bit! class (char->integer (char-downcase c)))
	     (class-add-bit! class (char->integer (char-upcase c))))))

   ;; handles case-sensitivity
   (define (add-char-range class from to)
      (let ((from (if (char? from)
		      (char->integer from)
		      from))
	    (to (if (char? to)
		    (char->integer to)
		    to)))
	 (let loop ((from from))
	    (unless (>= from to)
	       (if case-sensitive?
		   (class-add-bit! class from)
		   (add-char class (integer->char from)))
	       (loop (+fx from 1))))))

   ;; does not use case-sensitivity
   (define (add-range class from to)
      (let ((from (if (char? from)
		      (char->integer from)
		      from))
	    (to (if (char? to)
		    (char->integer to)
		    to)))
	 (let loop ((from from))
	    (unless (>= from to)
	       (class-add-bit! class from)
	       (loop (+fx from 1))))))

   
   (define (add-digits class)
      (add-range class #\0 #\9))

   (define (add-white class)
      (add-chars class #x09 #x0B #x0C #x20 #xA0))

   (define (add-any class)
      (add-chars class #xA #xD #x2028 #x2029))

   (define (add-word class)
      (add-range class #\a #\z)
      (add-range class #\A #\Z)
      (add-digits class)
      (add-char class #\_)) ;; TODO add-char is case-insensitive

   (define (add-xdigits class)
      (add-digits class)
      (add-range class #\a #\f)
      (add-range class #\A #\F))

   (define (add-element class char/class)
      (match-case char/class
	 ((and (? char?) ?c)
	  (add-char class c))
	 ((:range ?from ?to)
	  (add-range class from to))
	 ((:char-range ?from ?to)
	  (add-char-range class (char->integer from) (char->integer to)))
	 ((:any)        (add-any class))
	 ((:digit)      (add-digit class))
	 ((:not-digit)  (inverted add-digits class))
	 ((:space)      (add-white class))
	 ((:not-space)  (inverted add-white class))
	 ((:word)       (add-word class))
	 ((:not-word)   (inverted add-white class))
	 ((:xdigit)     (add-xdigits class))
	 ((:not-xdigit) (inverted add-xdigits class))
	 ((:neg-char ?inverted-char/class)
	  (inverted add-element class inverted-char/class))
	 ((:one-of-chars . ?chars/classes)
	  (for-each (lambda (c) (add-element class c))
		    chars/classes))
	 (else
	  (error "RegExp Class"
		 "forgot character class"
		 char/class))))

   (define (nb-chars class)
      (nb-bits class))

   (define (get-single-char class)
      (integer->char (get-single-bit class)))

   (let ((res (instantiate::RegExp-class)))
      (add-element! res char/class)
      (if (zerofx? (nb-chars res))
	  (get-single-char res)
	  res)))
