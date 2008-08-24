(module jsre-RegExp-classes
   (static
    (class RE-class
       (RE-class-constructor)
	   ;; TODO: 32/64 bit issue...
;       (bits::u64vector (default '#u64(#l0 #l0 #l0 #l0)))
       (bits (default '#s64(#l0 #l0 #l0 #l0)))
       (constant?::bool (default #f))))
   (export
    (RegExp-class-pattern?::bool pattern)
    (RegExp-class-create char/class case-sensitive?)
    (RegExp-class-match re-class c::char)
    (RegExp-class-subset?::bool re-class1 re-class2)
    (RegExp-class-overlap?::bool re-class1 re-class2)
    (word-boundary str::bstring index::bint)
    (terminator-char? c::char)))


(define (RegExp-class-pattern? pattern)
   (match-case pattern
      ((or (? char?)
	   (or :any (:any)
	       :digit  (:digit)  :not-digit  (:not-digit)
	       :space  (:space)  :not-space  (:not-space)
	       :word   (:word)   :not-word   (:not-word)
	       :xdigit (:xdigit) :not-xdigit (:not-xdigit)
	       :alnum  (:alnum)  :not-alnum  (:not-alnum)
	       :alpha  (:alpha)  :not-alpha  (:not-alpha)
	       :ascii  (:ascii)  :not-ascii  (:not-ascii)
	       :blank  (:blank)  :not-blank  (:not-blank)
	       :cntrl  (:cntrl)  :not-ctrl   (:not-cntrl)
	       :lower  (:lower)  :not-lower  (:not-lower)
	       :upper  (:upper)  :not-upper  (:not-upper)
	       :punct  (:punct)  :not-punct  (:not-punct)
	       :print  (:print)  :not-print  (:not-print)
	       :graph  (:graph)  :not-graph  (:not-graph)
	       (:neg-char ?-)
	       (:one-of-chars ?-)))
       #t)
      (else #f)))
	   
(define *llong-size* 64)
(define *nb-llongs* (/fx 256 *llong-size*))

(define (RE-class-constructor this)
   (with-access::RE-class this (bits)
      (set! bits (make-s64vector *nb-llongs* #l0))))

(define (RegExp-class-match re-class c)
   (with-access::RE-class re-class (bits)
      (let* ((cn (char->integer c))
	     (index (/fx cn *llong-size*))
	     (offset (modulo cn *llong-size*)))
	 (let ((mask (bit-lshllong #l1 offset)))
	    (not (zerollong? (bit-andllong (s64vector-ref bits index)
					   mask)))))))

;; is cl1 a subset of cl2?
(define (RegExp-class-subset? cl1 cl2)
   (with-access::RE-class cl1 (bits)
      (let loop ((i 0))
	 (if (=fx i *nb-llongs*)
	     #t
	     (let ((n1 (s64vector-ref bits i))
		   (n2 (s64vector-ref (RE-class-bits cl2) i)))
		(if (=llong n1 (bit-andllong n1 n2))
		    (loop (+fx i 1))
		    #f))))))

;; is the intersection of cl1 and cl2 not empty?
(define (RegExp-class-overlap? cl1 cl2)
   (with-access::RE-class cl1 (bits)
      (let loop ((i 0))
	 (if (=fx i *nb-llongs*)
	     #f
	     (let ((n1 (s64vector-ref bits i))
		   (n2 (s64vector-ref (RE-class-bits cl2) i)))
		(if (zerollong? (bit-andllong n1 n2))
		    (loop (+fx i 1))
		    #t))))))
      
(define (class-add-n! re-class::RE-class n::bint)
   (with-access::RE-class re-class (bits)
      (let* ((index (/fx n *llong-size*))
	     (offset (modulo n *llong-size*))
	     (old (s64vector-ref bits index)))
	 (s64vector-set! bits
			 index
			 (bit-orllong old
				      (bit-lshllong #l1 offset))))))

(define (class-add-ns! re-class::RE-class . Lns)
   (for-each (lambda (n) (class-add-n! re-class n))
	     Lns))

(define (class-add-range-n! re-class::RE-class from::bint to::bint)
   (unless (>=fx from to)
      (class-add-n! re-class from)
      (class-add-range-n! re-class (+fx from 1) to)))

(define (class-add-range-c! re-class::RE-class from::char to::char)
   (class-add-range-n! re-class (char->integer from) (char->integer to)))

(define (class-add-c! re-class::RE-class c::char)
   (class-add-n! re-class (char->integer c)))
(define (class-add-cs! re-class::RE-class . Lchars)
   (for-each (lambda (c) (class-add-c! re-class c))
	     Lchars))

;; returns a char, if there is only one char in the class. Otherwise returns
;; #f.
(define (class-get-single-char re-class::RE-class)
   (define (one-bit? i::llong)
      (and (not (zerollong? i))
	   (zerollong? (bit-andllong (-llong i #l1) i))))

   (define (get-bit-pos i::llong)
      (let loop ((j 0)
		 (mask #l1))
	 (if (not (=llong #l0 (bit-andllong i mask)))
	     j
	     (loop (+ j 1) (bit-lshllong mask 1)))))
   
   (with-access::RE-class re-class (bits)
      (let loop ((i 0)
		 (non-zero #f))
	 (cond
	    ((=fx i *nb-llongs*)
	     (and non-zero
		  (one-bit? (s64vector-ref bits non-zero))
		  (integer->char
		   (+fx (*fx non-zero *llong-size*)
			(get-bit-pos (s64vector-ref bits non-zero))))))
	    ((zerollong? (s64vector-ref bits i))
	     (loop (+fx i 1) non-zero))
	    (non-zero ;; got already one
	     #f)
	    (else
	     (loop (+fx i 1) i))))))

;; physically inverts the class *only* if it is not constant?. Otherwise a new
;; class is instantiated. In both cases the result is returned.
(define (class-invert! re-class)
   (with-access::RE-class re-class (constant? bits)
      (if constant?
	  (duplicate::RE-class re-class
	     (bits (let ((v (make-s64vector *nb-llongs*)))
		      (let loop ((i 0))
			 (if (< i *nb-llongs*)
			     (let ((old (s64vector-ref bits i)))
				(s64vector-set! v i (bit-notllong old))
				(loop (+fx i 1)))
			     v))))
	     (constant? #f))
	  (let loop ((i 0))
	     (if (=fx i *nb-llongs*)
		 re-class
		 (let ((old (s64vector-ref bits i)))
		    (s64vector-set! bits i (bit-notllong old))
		    (loop (+fx i 1))))))))

;; returns a merged result of both classes. If either is not 'constant?' then
;; it is used as result. Otherwise a new one is created.
(define (merge-classes! re-class1 re-class2)
   (cond
      ((eq? re-class1 *empty-class*)
       re-class2)
      ((eq? re-class2 *empty-class*)
       re-class1)
      (else
       (let ((res (cond
		     ((not (RE-class-constant? re-class1))
		      re-class1)
		     ((not (RE-class-constant? re-class2))
		      re-class2)
		     (else
		      (instantiate::RE-class)))))
	  (let loop ((i 0))
	     (if (=fx i *nb-llongs*)
		 res
		 (with-access::RE-class res (bits)
		    (let ((l1 (s64vector-ref (RE-class-bits re-class1) i))
			  (l2 (s64vector-ref (RE-class-bits re-class2) i)))
		       (s64vector-set! bits i
				       (bit-orllong l1 l2))
		       (loop (+fx i 1))))))))))

(define (class-out c)
   (with-access::RE-class c (bits)
      (tprint (format "~x ~x ~x ~x"
		      (s64vector-ref bits 3)
		      (s64vector-ref bits 2)
		      (s64vector-ref bits 1)
		      (s64vector-ref bits 0)))))

;; precomputed char-classes.
(define *case-sensitive-classes*
   (let ((v (make-vector 256)))
      (let loop ((i 0))
	 (when (<fx i 256)
	    (let ((re-class (instantiate::RE-class
			       (constant? #t))))
	       (class-add-n! re-class i)
	       (vector-set! v i re-class)
	       (loop (+fx i 1))))
	 v)))

;; TODO: we rely on the fact that defines are processed in order.
(define *case-insensitive-classes*
   (let ((v (make-vector 256)))
      (let loop ((i 0))
	 (when (<fx i 256)
	    (let* ((c (integer->char i))
		   (c-up (char-upcase c))
		   (c-down (char-downcase c)))
	       (if (char=? c-up c-down)
		   (vector-set! v i (vector-ref *case-sensitive-classes* i))
		   (let ((re-class (instantiate::RE-class
				      (constant? #t))))
		      (class-add-n! re-class (char->integer c-up))
		      (class-add-n! re-class (char->integer c-down))
		      (vector-set! v i re-class)))
	       (loop (+fx i 1))))
	 v)))

(define (inverted-constant re-class)
   (let ((inverted (class-invert! re-class)))
      (with-access::RE-class inverted (constant?)
	 (set! constant? #t)
	 inverted)))

(define *empty-class* (instantiate::RE-class (constant? #t)))
(define *every-class* (inverted-constant *empty-class*))

(define *digits-class*
   (let ((re-class (instantiate::RE-class
		      (constant? #t))))
      (class-add-range-c! re-class #\0 #\9)
      re-class))
(define *no-digits-class* (inverted-constant *digits-class*))

(define *white-class*
   (let ((re-class (instantiate::RE-class
		      (constant? #t))))
      (class-add-ns! re-class #x09 #x0B #x0C #x20 #xA0)
      (class-invert! re-class)))

(define *no-white-class* (inverted-constant *white-class*))

(define *any-class*
   (let ((re-class (instantiate::RE-class
		      (constant? #f)))) ;; set during invertion
      (class-add-ns! re-class #xA #xD) ;; TODO: add when 16bit #x2028 #x2029
      (inverted-constant re-class)))
(define *no-any-class* (inverted-constant *any-class*))

(define *word-class*
   (let ((re-class (instantiate::RE-class
		      (constant? #t))))
      (class-add-range-c! re-class #\a #\z)
      (class-add-range-c! re-class #\A #\Z)
      (class-add-range-c! re-class #\0 #\9)
      (class-add-c! re-class #\_)
      re-class))
(define *no-word-class* (inverted-constant *word-class*))

(define *xdigits-class*
   (let ((re-class (instantiate::RE-class
		      (constant? #t))))
      (class-add-range-c! re-class #\a #\f)
      (class-add-range-c! re-class #\A #\F)
      (class-add-range-c! re-class #\0 #\9)
      re-class))
(define *no-xdigits-class* (inverted-constant *xdigits-class*))

(define *alnum-class*
   (let ((re-class (instantiate::RE-class
		      (constant? #t))))
      (class-add-range-c! re-class #\a #\z)
      (class-add-range-c! re-class #\A #\Z)
      (class-add-range-c! re-class #\0 #\9)
      re-class))
(define *no-alnum-class* (inverted-constant *alnum-class*))

(define *alpha-class*
   (let ((re-class (instantiate::RE-class
		      (constant? #t))))
      (class-add-range-c! re-class #\a #\z)
      (class-add-range-c! re-class #\A #\Z)
      re-class))
(define *no-alpha-class* (inverted-constant *alnum-class*))

(define *ascii-class*
   (let ((re-class (instantiate::RE-class
		      (constant? #t))))
      (class-add-range-n! re-class 0 127)
      re-class))
(define *no-ascii-class* (inverted-constant *ascii-class*))

(define *blank-class*
   (let ((re-class (instantiate::RE-class
		      (constant? #t))))
      (class-add-c! re-class #\space)
      (class-add-c! re-class #\tab)
      re-class))
(define *no-blank-class* (inverted-constant *blank-class*))

(define *cntrl-class*
   (let ((re-class (instantiate::RE-class
		      (constant? #t))))
      (class-add-n! re-class #x32)
      re-class))
(define *no-cntrl-class* (inverted-constant *cntrl-class*))

(define *graph-class*
   (let ((re-class (instantiate::RE-class
		      (constant? #t))))
      (let loop ((i 32))
	 (when (<fx i 256)
	    (unless (char-whitespace? (integer->char i))
	       (class-add-n! re-class i))
	    (loop (+fx i 1))))
      re-class))
(define *no-graph-class* (inverted-constant *graph-class*))

(define *lower-class*
   (let ((re-class (instantiate::RE-class
		      (constant? #t))))
      (let loop ((i 0))
	 (when (<fx i 256)
	    (when (char-lower-case? (integer->char i))
	       (class-add-n! re-class i))
	    (loop (+fx i 1))))
      re-class))
(define *no-lower-class* (inverted-constant *lower-class*))

(define *upper-class*
   (let ((re-class (instantiate::RE-class
		      (constant? #t))))
      (let loop ((i 0))
	 (when (<fx i 256)
	    (when (char-upper-case? (integer->char i))
	       (class-add-n! re-class i))
	    (loop (+fx i 1))))
      re-class))
(define *no-upper-class* (inverted-constant *upper-class*))

(define *print-class*
   (let ((re-class (instantiate::RE-class
		      (constant? #t))))
      (class-add-range-n! re-class 32 256)
      re-class))
(define *no-print-class* (inverted-constant *print-class*))

(define *punct-class*
   (let ((re-class (instantiate::RE-class
		      (constant? #t))))
      (let loop ((i 32))
	 (when (<fx i 256)
	    (let ((c (integer->char i)))
	       (unless (or (char-whitespace? c)
			   (char-alphabetic? c)
			   (char-numeric? c))
		  (class-add-n! re-class i)))
	    (loop (+fx i 1))))
      re-class))
(define *no-punct-class* (inverted-constant *punct-class*))


;; For now just build a range based on ASCII chars.
(define (RegExp-class-create char/class case-sensitive?)
   (define (add-char! re-class c)
      (let ((precomputed-chars (if case-sensitive?
				   *case-sensitive-classes*
				   *case-insensitive-classes*)))
	 (merge-classes! re-class
			 (vector-ref precomputed-chars (char->integer c)))))

   ;; handles case-sensitivity
   (define (add-char-range re-class from to)
      (let ((from (if (char? from)
		      (char->integer from)
		      from))
	    (to (if (char? to)
		    (char->integer to)
		    to)))
	 (let loop ((re-class re-class)
		    (from from))
	    (if (>= from to)
		re-class
		(loop (add-char! re-class (integer->char from))
		      (+fx from 1))))))

   ;; does not use case-sensitivity
   (define (add-range re-class from to)
      (let ((from (if (char? from)
		      (char->integer from)
		      from))
	    (to (if (char? to)
		    (char->integer to)
		    to)))
	 (if (RE-class-constant? re-class)
	     (add-range (instantiate::RE-class) from to)
	     (begin
		(class-add-range-n! re-class from to)
		re-class))))

   (define (add-element! re-class char/class)
      (match-case char/class
	 ((and (? char?) ?c)
	  (add-char! re-class c))
	 ((:range ?from ?to)
	  (add-range re-class from to))
	 ((:char-range ?from ?to)
	  (add-char-range re-class from to))
	 ((or :any (:any))
	  (merge-classes! re-class *any-class*))
	 ((or :digit (:digit))
	  (merge-classes! re-class *digits-class*))
	 ((or :not-digit (:not-digit))
	  (merge-classes! re-class *no-digits-class*))
	 ((or :space (:space))
	  (merge-classes! re-class *white-class*))
	 ((or :not-space (:not-space))
	  (merge-classes! re-class *no-white-class*))
	 ((or :word (:word))
	  (merge-classes! re-class *word-class*))
	 ((or :not-word (:not-word))
	  (merge-classes! re-class *no-word-class*))
	 ((or :xdigit (:xdigit))
	  (merge-classes! re-class *xdigits-class*))
	 ((or :not-xdigit (:not-xdigit))
	  (merge-classes! re-class *no-xdigits-class*))
	 ((or :alnum (:alnum))
	  (merge-classes! re-class *alnum-class*))
	 ((or :not-alnum (:not-alnum))
	  (merge-classes! re-class *no-alnum-class*))
	 ((or :alpha (:alpha))
	  (merge-classes! re-class *alpha-class*))
	 ((or :not-alpha (:not-alpha))
	  (merge-classes! re-class *no-alpha-class*))
	 ((or :ascii (:ascii))
	  (merge-classes! re-class *ascii-class*))
	 ((or :not-ascii (:not-ascii))
	  (merge-classes! re-class *no-ascii-class*))
	 ((or :blank (:blank))
	  (merge-classes! re-class *blank-class*))
	 ((or :not-blank (:not-blank))
	  (merge-classes! re-class *no-blank-class*))
	 ((or :cntrl (:cntrl))
	  (merge-classes! re-class *cntrl-class*))
	 ((or :not-cntrl (:not-cntrl))
	  (merge-classes! re-class *no-cntrl-class*))
	 ((or :lower (:lower))
	  (merge-classes! re-class *lower-class*))
	 ((or :not-lower (:not-lower))
	  (merge-classes! re-class *no-lower-class*))
	 ((or :upper (:upper))
	  (merge-classes! re-class *upper-class*))
	 ((or :not-upper (:not-upper))
	  (merge-classes! re-class *no-upper-class*))
	 ((or :punct (:punct))
	  (merge-classes! re-class *punct-class*))
	 ((or :not-punct (:not-punct))
	  (merge-classes! re-class *no-punct-class*))
	 ((or :print (:print))
	  (merge-classes! re-class *print-class*))
	 ((or :not-print (:not-print))
	  (merge-classes! re-class *no-print-class*))
	 ((or :graph (:graph))
	  (merge-classes! re-class *graph-class*))
	 ((or :not-graph (:not-graph))
	  (merge-classes! re-class *no-graph-class*))
	 ((:neg-char ?inverted-char/re-class)
	  (let ((tmp (add-element! #f inverted-char/re-class)))
	     (merge-classes! re-class (class-invert! tmp))))
	 ((:one-of-chars ?chars/classes)
	  (let loop ((re-class re-class)
		     (chars/classes chars/classes))
	     (if (null? chars/classes)
		 re-class
		 (let ((re-c (add-element! re-class (car chars/classes))))
		    (loop re-c (cdr chars/classes))))))
	 (else
	  (error "RegExp Class"
		 "forgot character class"
		 char/class))))

   (let ((re-class (add-element! *empty-class* char/class)))
      (or (class-get-single-char re-class)
	  re-class)))

(define (word-boundary str index)
   (define (word-char? c)
      (or (char-alphabetic? c)
	  (char-numeric? c)
	  (char=? c #\_)))
   
   (let ((len (string-length str)))
      (cond
	 ((zerofx? len)
	  #f)
	 ((zerofx? index)
	  (word-char? (string-ref str index)))
	 ((>=fx index len)
	  (word-char? (string-ref str (-fx len 1))))
	 ((word-char? (string-ref str (-fx index 1)))
	  (not (word-char? (string-ref str index))))
	 (else ;; index-1 is not word-char
	  (word-char? (string-ref str index))))))

(define (terminator-char? c)
   (let ((n (char->integer c)))
      (or (=fx n #xA) ;; Linefeed
	  (=fx n #xD) ;; Carriage Return
	  ;; following entries can't happen unless we
	  ;; have switched to UCS2
	  (=fx n #x2028) ;; Line separator
	  (=fx n #x2029)))) ;; Paragraph separator
