(module jsre-RegExp-classes
   (export
    (RegExp-class-condition chars/ranges
			    invert?
			    case-sensitive?)
    (inline RegExp-match-c c class)))

(define-inline (RegExp-match-c c class)
   (let ((cn (char->integer c)))
      (if (< cn 64)
	  (let ((l (vector-ref class 0)) ;; lower
		(mask (make-llong (bit-lsh 1 cn))))
	     (not (=llong #l0 (bit-andllong l mask))))
	  (let ((u (vector-ref class 1)) ;; upper
		(mask (make-llong (bit-lsh 1 (-fx cn 64)))))
	     (not (=llong #l0 (bit-andllong u mask)))))))
	  
;; For now just build a range based on ASCII chars.
(define (RegExp-class-condition chars/ranges invert? case-sensitive?)
   (define (add-char-n cn lower upper)
      (if (< cn 64)
	  (values (bit-orllong lower
			       (make-llong (bit-lsh 1 cn)))
		  upper)
	  (values lower
		  (bit-orllong upper
			       (make-llong (bit-lsh 1 (- cn 64)))))))

   (define (add-char c lower upper)
      (if case-sensitive?
	  (add-char-n (char->integer c) lower upper)
	  ;; HACK: case-sensitivity.
	  (receive (l u)
	     (add-char-n (char->integer (char-downcase c)) lower upper)
	     (add-char-n (char->integer (char-upcase c)) l u))))
      
   (define (add-range from to lower upper)
      (let ((from (if (char? from)
		      (char->integer from)
		      from))
	    (to (if (char? to)
		    (char->integer to)
		    to)))
	 (let loop ((from from)
		    (lower lower)
		    (upper upper))
	    (if (>= from to)
		(values lower upper)
		(receive (l u)
		   (add-char (integer->char from) lower upper)
		   (loop (+fx from 1)
			 l u))))))

   (define (add-chars chars lower upper)
      (if (null? chars)
	  (values lower upper)
	  (receive (l u)
	     (add-char (car chars) lower upper)
	     (add-chars (cdr chars) l u))))

   (define (add-numbers lower upper)
      (add-range #\0 #\9 lower upper))

   (define (add-whitespace lower upper)
      (add-chars '(#x09 #x0B #x0C #x20 #xA0) lower upper))

   (define (add-a-zA-Z0-9_ lower upper)
      (receive (l u)
	 (add-range #\a #\z lower upper)
	 (receive (l1 u1)
	    (add-range #\A #\Z l u)
	    (receive (l2 u2)
	       (add-range #\0 #\9 l1 u1)
	       (add-char #\_ l2 u2)))))

   (define (inverted f lower upper)
      (receive (nl nu)
	 (f #l0 #l0)
	 (values (bit-orllong lower (bit-notllong nl))
		 (bit-orllong upper (bit-notllong nu)))))

   (define (add-c/r c/r lower upper)
      (match-case c/r
	 ((and (? char?) ?c)
	  (add-char c lower upper))
	 ((range ?from ?to)
	  (add-range from to lower upper))
	 ((number)
	  (add-numbers lower upper))
	 ((not-number)
	  (inverted add-numbers lower upper))
	 ((white-space)
	  (add-whitespace lower upper))
	 ((not-white-space)
	  (inverted add-whitespace lower upper))
	 ((a-zA-Z0-9_)
	  (add-a-zA-Z0-9_ lower upper))
	 ((not-a-zA-Z0-9_)
	  (inverted add-a-zA-Z0-9_ lower upper))
	 (else
	  (error "RegExp Class"
		 "forgot character class"
		 c/r))))

   (let loop ((cs/rs chars/ranges)
	      (lower #l0)
	      (upper #l0))
      (if (null? cs/rs) 
	  (if invert?
	      (vector (bit-notllong lower) (bit-notllong upper))
	      (vector lower upper))
	  (let ((c/r (car cs/rs)))
	     (receive (l u)
		(add-c/r c/r lower upper)
		(loop (cdr cs/rs) l u))))))
