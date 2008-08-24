(module jsre-RegExp-char-set
   (export
    (final-class RE-char-set
       (char-set-constructor)
	   ;; TODO: 32/64 bit issue...
;       (bits::u64vector (default '#u64(#l0 #l0 #l0 #l0)))
       (bits (default '#s64(#l0 #l0 #l0 #l0))))
    (new-empty-char-set)
    (char-set-match::bool re-set::RE-char-set c::char)
    (char-set-subset?::bool set1::RE-char-set set2::RE-char-set)
    (char-set-overlap?::bool set1::RE-char-set set2::RE-char-set)

    (char-set-add-n! re-set::RE-char-set n::bint)
    (char-set-add-ns! re-set::RE-char-set . Lns)
    (char-set-add-range-n! re-set::RE-char-set from::bint to::bint)
    (char-set-add-range-c! re-set::RE-char-set from::char to::char)
    (char-set-add-c! re-set::RE-char-set c::char)
    (char-set-add-cs! re-set::RE-char-set . Lchars)
    (char-set-get-single-char re-set::RE-char-set)
    (char-set-copy-inverted from::RE-char-set to::RE-char-set)
    (char-set-invert::RE-char-set re-set::RE-char-set)
    (char-set-invert! re-set::RE-char-set)
    (char-set-merge-into! target::RE-char-set re-set1::RE-char-set
			  re-set2::RE-char-set)
    (char-set-out s)
    ))

(define *llong-size* 64)
(define *nb-llongs* (/fx 256 *llong-size*))

(define (new-empty-char-set)
   (instantiate::RE-char-set))

(define (char-set-constructor this)
   (with-access::RE-char-set this (bits)
      (set! bits (make-s64vector *nb-llongs* #l0))))

(define (char-set-match re-set c)
   (with-access::RE-char-set re-set (bits)
      (let* ((cn (char->integer c))
	     (index (/fx cn *llong-size*))
	     (offset (modulo cn *llong-size*)))
	 (let ((mask (bit-lshllong #l1 offset)))
	    (not (zerollong? (bit-andllong (s64vector-ref bits index)
					   mask)))))))

;; is set1 a subset of set2?
(define (char-set-subset? set1 set2)
   (with-access::RE-char-set set1 (bits)
      (let loop ((i 0))
	 (if (=fx i *nb-llongs*)
	     #t
	     (let ((n1 (s64vector-ref bits i))
		   (n2 (s64vector-ref (RE-char-set-bits set2) i)))
		(if (=llong n1 (bit-andllong n1 n2))
		    (loop (+fx i 1))
		    #f))))))

;; is the intersection of set1 and set2 not empty?
(define (char-set-overlap? set1 set2)
   (with-access::RE-char-set set1 (bits)
      (let loop ((i 0))
	 (if (=fx i *nb-llongs*)
	     #f
	     (let ((n1 (s64vector-ref bits i))
		   (n2 (s64vector-ref (RE-char-set-bits set2) i)))
		(if (zerollong? (bit-andllong n1 n2))
		    (loop (+fx i 1))
		    #t))))))
      

(define (char-set-add-n! re-set::RE-char-set n::bint)
   (with-access::RE-char-set re-set (bits)
      (let* ((index (/fx n *llong-size*))
	     (offset (modulo n *llong-size*))
	     (old (s64vector-ref bits index)))
	 (s64vector-set! bits
			 index
			 (bit-orllong old
				      (bit-lshllong #l1 offset))))))

(define (char-set-add-ns! re-set::RE-char-set . Lns)
   (for-each (lambda (n) (char-set-add-n! re-set n))
	     Lns))

(define (char-set-add-range-n! re-set::RE-char-set from::bint to::bint)
   (unless (>=fx from to)
      (char-set-add-n! re-set from)
      (char-set-add-range-n! re-set (+fx from 1) to)))

(define (char-set-add-range-c! re-set::RE-char-set from::char to::char)
   (char-set-add-range-n! re-set (char->integer from) (char->integer to)))

(define (char-set-add-c! re-set::RE-char-set c::char)
   (char-set-add-n! re-set (char->integer c)))
(define (char-set-add-cs! re-set::RE-char-set . Lchars)
   (for-each (lambda (c) (char-set-add-c! re-set c))
	     Lchars))

;; returns a char, if there is only one char in the set. Otherwise returns
;; #f.
(define (char-set-get-single-char re-set::RE-char-set)
   (define (one-bit? i::llong)
      (and (not (zerollong? i))
	   (zerollong? (bit-andllong (-llong i #l1) i))))

   (define (get-bit-pos i::llong)
      (let loop ((j 0)
		 (mask #l1))
	 (if (not (=llong #l0 (bit-andllong i mask)))
	     j
	     (loop (+ j 1) (bit-lshllong mask 1)))))
   
   (with-access::RE-char-set re-set (bits)
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

(define (char-set-copy-inverted from to)
   (with-access::RE-char-set from (bits)
      (let ((to-bits (RE-char-set-bits to)))
	 (let loop ((i 0))
	    (when (<fx i *nb-llongs*)
	       (let ((old (s64vector-ref bits i)))
		  (s64vector-set! to-bits i (bit-notllong old))
		  (loop (+fx i 1))))))))
   
(define (char-set-invert re-set)
   (let ((result (instantiate::RE-char-set)))
      (char-set-copy-inverted re-set result)
      result))

;; physically inverts the char-set.
(define (char-set-invert! re-set)
   (char-set-copy-inverted re-set re-set))

(define (char-set-merge-into! target re-set1 re-set2)
   (with-access::RE-char-set target (bits)
      (let loop ((i 0))
	 (unless (=fx i *nb-llongs*)
	    (let ((l1 (s64vector-ref (RE-char-set-bits re-set1) i))
		  (l2 (s64vector-ref (RE-char-set-bits re-set2) i)))
	       (s64vector-set! bits i
			       (bit-orllong l1 l2))
	       (loop (+fx i 1)))))))

(define (char-set-out c)
   (with-access::RE-char-set c (bits)
      (format "~x ~x ~x ~x"
	      (s64vector-ref bits 3)
	      (s64vector-ref bits 2)
	      (s64vector-ref bits 1)
	      (s64vector-ref bits 0))))
