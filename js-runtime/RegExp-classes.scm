(module jsre-RegExp-classes
   (static
    (class RE-class
	   ;; TODO: 32/64 bit issue...
;       (bits::u64vector (default '#u64(#l0 #l0 #l0 #l0)))
       (bits (default '#s64(#l0 #l0 #l0 #l0)))
       (constant?::bool (default #f))))
   (export
    (RegExp-class-create char/class case-sensitive?)
    (RegExp-class-match re-class c::char)))

(define *llong-size* 64)
(define *nb-llongs* (/fx 256 *llong-size*))

(define (RegExp-class-match re-class c)
   (with-access::RE-class re-class (bits)
      (let* ((cn (char->integer c))
	     (index (/fx cn *llong-size*))
	     (offset (modulo cn *llong-size*)))
	 (let ((mask (make-llong (bit-lsh 1 offset))))
	    (not (zerollong? (bit-andllong (s64vector-ref bits index)
					   mask)))))))

(define (class-add-n! re-class::RE-class n::bint)
   (with-access::RE-class re-class (bits)
      (let* ((index (/fx n *llong-size*))
	     (offset (modulo n *llong-size*))
	     (old (s64vector-ref bits index)))
	 (s64vector-set! bits
			 index
			 (bit-orllong old
				      (make-llong (bit-lsh 1 offset)))))))
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
      (let loop ((j 1)
		 (k (make-llong 1)))
	 (if (not (= 0 (bit-andllong i k)))
	     j
	     (loop (+ j 1) (*llong k #l2)))))
   
   (with-access::RE-class re-class (bits)
      (let loop ((i 0)
		 (non-zero #f))
	 (cond
	    ((=fx i *nb-llongs*)
	     (and non-zero
		  (one-bit? (s64vector-ref bits non-zero))
		  (+fx (*fx non-zero *llong-size*)
		       (get-bit-pos (s64vector-ref bits i)))))
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
	  (class-invert! (duplicate::RE-class re-class
			    (constant? #f)))
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
   (let ((inverted (class-invert! *digits-class*)))
      (with-access::RE-class inverted (constant?)
	 (set! constant? #t)
	 inverted)))

(define *empty-class* (instantiate::RE-class (constant? #t)))

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
      re-class))

(define *no-white-class* (inverted-constant *white-class*))

(define *any-class*
   (let ((re-class (instantiate::RE-class
		      (constant? #t))))
      (class-add-ns! re-class #xA #xD) ;; TODO: add when 16bit #x2028 #x2029
      re-class))
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
	 ((:any)        (merge-classes! re-class *any-class*))
	 ((:digit)      (merge-classes! re-class *digits-class*))
	 ((:not-digit)  (merge-classes! re-class *no-digits-class*))
	 ((:space)      (merge-classes! re-class *white-class*))
	 ((:not-space)  (merge-classes! re-class *no-white-class*))
	 ((:word)       (merge-classes! re-class *word-class*))
	 ((:not-word)   (merge-classes! re-class *no-word-class*))
	 ((:xdigit)     (merge-classes! re-class *xdigits-class*))
	 ((:not-xdigit) (merge-classes! re-class *no-xdigits-class*))
	 ((:alnum)      (merge-classes! re-class *alnum-class*))
	 ((:not-alnum)  (merge-classes! re-class *no-alnum-class*))
	 ((:alpha)      (merge-classes! re-class *alpha-class*))
	 ((:not-alpha)  (merge-classes! re-class *no-alpha-class*))
	 ((:ascii)      (merge-classes! re-class *ascii-class*))
	 ((:not-ascii)  (merge-classes! re-class *no-ascii-class*))
	 ((:blank)      (merge-classes! re-class *blank-class*))
	 ((:not-blank)  (merge-classes! re-class *no-blank-class*))
	 ((:cntrl)      (merge-classes! re-class *cntrl-class*))
	 ((:not-cntrl)  (merge-classes! re-class *no-cntrl-class*))
	 ((:lower)      (merge-classes! re-class *lower-class*))
	 ((:not-lower)  (merge-classes! re-class *no-lower-class*))
	 ((:upper)      (merge-classes! re-class *upper-class*))
	 ((:not-upper)  (merge-classes! re-class *no-upper-class*))
	 ((:punct)      (merge-classes! re-class *punct-class*))
	 ((:not-punct)  (merge-classes! re-class *no-punct-class*))
	 ((:print)      (merge-classes! re-class *print-class*))
	 ((:not-print)  (merge-classes! re-class *no-print-class*))
	 ((:graph)      (merge-classes! re-class *graph-class*))
	 ((:not-graph)  (merge-classes! re-class *no-graph-class*))
	 ((:neg-char ?inverted-char/re-class)
	  (let ((tmp (add-element! #f inverted-char/re-class)))
	     (merge-classes! re-class (class-invert! tmp))))
	 ((:one-of-chars . ?chars/classes)
	  (let loop ((re-class re-class)
		     (chars/classes chars/classes))
	     (if (null? chars/classes)
		 re-class
		 (begin
		    (add-element! re-class (car chars/classes))
		    (loop re-class (cdr chars/classes))))))
	 (else
	  (error "RegExp Class"
		 "forgot character class"
		 char/class))))

   (let ((re-class (add-element! *empty-class* char/class)))
      (or (class-get-single-char re-class)
	  re-class)))
