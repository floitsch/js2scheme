;; Copyright (c) 2007-2011, Florian Loitsch
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are met:
;;    * Redistributions of source code must retain the above copyright
;;      notice, this list of conditions and the following disclaimer.
;;    * Redistributions in binary form must reproduce the above copyright
;;      notice, this list of conditions and the following disclaimer in the
;;      documentation and/or other materials provided with the distribution.
;;    * Neither the name of the <organization> nor the
;;      names of its contributors may be used to endorse or promote products
;;      derived from this software without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;; DISCLAIMED. IN NO EVENT SHALL <COPYRIGHT HOLDER> BE LIABLE FOR ANY
;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
;; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
;; ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(module jsre-double
   (export (double->string::bstring d::double how::symbol precision::bint)))

(define (bit-lshbx::bignum x::bignum by::bint)
   (if (<fx by 0)
       #z0
       (*bx x (exptbx #z2 (fixnum->bignum by)))))

(define (bit-rshbx::bignum x::bignum by::bint)
   (if (<fx by 0)
       #z0
       (/bx x (exptbx #z2 (fixnum->bignum by)))))

;; biased-e (the one in the double) contains the exponent-bias and is such that
;;  1.f*2^e (with f mantissa-bits)
;; here we use the mantissa rather as 1f (that is without the '.')
;; and we thus have to add the length of 'f' to the bias.
(define *exponent-bias* (+fx #x3ff 52)) ;; == 1023 + 52bits of mantissa.
(define *min-exponent* (negfx *exponent-bias*))
(define *mantissa-size* 53)             ;; 53 if we count hidden bit.
(define *first-normal* 2.2250738585072e-308) ;;everything smaller is a denormal

(define (double-biased-exponent::bint d::double)
   (let* ((s (double->ieee-string d))
	  (b0 (char->integer (string-ref s 0)))
	  (b1 (char->integer (string-ref s 1))))
      (+fx (bit-lsh (bit-and b0 #x7F)
		    4)
	   (bit-rsh (bit-and b1 #xF0)
		    4))))

(define (compute-denormal-e d)
   (if (<fl d 0.0)
       (compute-denormal-e (negfl d))
       ;; this is not fast, but we don't really care, as this should not
       ;; happen often
       ;; denormals do not have the implicit bit anymore.
       ;; in order to have a continuity the exponent is not used the same way
       ;; anymore, but has to be decremented by 1.
       ;; basically: when we had before (2^p + f)*2^e
       ;;            we now have f*2^(e+1)
       ;; in other words. it's as if we stayed in the previous exponent but
       ;; without the implicit bit.
       ;; We are going to shift the denormalized numbers (see the
       ;; mantissa-conversions)
       ;; eg 8.0000.0000.0000 is going to be shifted so it "resembles" a normal 
       ;;   10.0000.0000.0000.
       ;; We do the shift here by multiplying with 2.0
       (let loop ((e (+fx *min-exponent* 1))
		  (shifted-d d))
	  (if (>=fl shifted-d *first-normal*)
	      e
	      (loop (-fx e 1) (*fl shifted-d 2.0))))))
   
(define (double-exponent::bint d::double)
   (let ((biased-e (double-biased-exponent d)))
      (cond
	 ((not (zerofx? biased-e) )
	  (-fx biased-e *exponent-bias*))
	 ((=fl d 0.0)
	  51)
	 (else
	  (compute-denormal-e d)))))

(define (double-zero-mantissa?::bool d::double)
   (let* ((s (double->ieee-string d))
	  (b0 (char->integer (string-ref s 0)))
	  (b1 (char->integer (string-ref s 1)))
	  (b2 (char->integer (string-ref s 2)))
	  (b3 (char->integer (string-ref s 3)))
	  (b4 (char->integer (string-ref s 4)))
	  (b5 (char->integer (string-ref s 5)))
	  (b6 (char->integer (string-ref s 6)))
	  (b7 (char->integer (string-ref s 7))))
      (and (zerofx? (bit-and b1 #x0F))
	   (zerofx? b2)
	   (zerofx? b3)
	   (zerofx? b4)
	   (zerofx? b5)
	   (zerofx? b6)
	   (zerofx? b7))))

(define (double-even-mantissa?::bool d::double)
   (let* ((s (double->ieee-string d))
	  (b7 (char->integer (string-ref s 7))))
      (and (zerofx? (bit-and b7 #x01)))))

(define (double-mantissa->bignum::bignum d::double)
   ;; for exponent == 0 we must not add the implicit 1 bit.
   (let* ((s (double->ieee-string d))
	  (denormal? (and (zerofx? (bit-and (char->integer (string-ref s 0))
					    #x7F))
			  (zerofx? (bit-and (char->integer (string-ref s 1))
					    #xF0))))
	  (highest-tmp (bit-and (char->integer (string-ref s 1))
				#x0F))
	  ;; add the implicit 1-bit
	  (highest (if denormal?
		       highest-tmp
		       (+fx #x10 highest-tmp)))
	  (highest-bx (fixnum->bignum highest)))
      (let loop ((i 2)
		 (res highest-bx))
	 (if (>=fx i 8)
	     (if denormal?
		 (*bx res #z2)
		 res)
	     (loop (+fx i 1)
		   (+bx (*bx #z256 res)
			(fixnum->bignum (char->integer (string-ref s i)))))))))

;; the following value ist the biggest value where we simply can convert an
;; integer floating point to fixnum and then print the integer.
;; we have to pay attention, as fixnums might be bigger than the 52 bits of the
;; double's mantissa. Even if the fixnum yielded a mathematically correct
;; string-representation of the double it is not guaranteed to be the smallest
;; one. By limiting the max-val to 52 bits we are however certain to get the
;; smallest string.
(define *maxvalfx/mantissa* (let ((maxvalfx_fl (fixnum->flonum (maxvalfx)))
				  ;; that's the biggest integer floating point
				  ;; number.
				  ;; In hex-form: 433fffffffffffff
				  (maxmantissa 9007199254740991.0))
			       (if (<fl maxvalfx_fl maxmantissa)
				   maxvalfx_fl
				   maxmantissa)))

(define (fill-fixnum-shortest! buffer::bstring pos::bint n::bint)
   (cond
      ((zerofx? n)
       (string-set! buffer pos #\0)
       (+fx pos 1))
      ((<fx n 0)
       (string-set! buffer pos #\-)
       ;; CARE: the following is only true here!
       ;; indeed: in general negative numbers have a greater range than
       ;; positive numbers!
       (fill-fixnum-shortest! buffer (+fx pos 1) (negfx n)))
      (else
       (let ((nb-digits (let loop ((digits 0)
				   (n n))
			   (if (zerofx? n)
			       digits
			       (loop (+fx digits 1)
				     (/fx n 10))))))
	  (let loop ((n n)
		     (i (+fx pos (-fx nb-digits 1))))
	     (if (zerofx? n)
		 (+fx pos nb-digits)
		 (begin
		    (string-set! buffer i
				 (integer->char (+fx (modulofx n 10)
						     (char->integer #\0))))
		    (loop (/fx n 10)
			  (-fx i 1)))))))))

;; how can be one of 'shortest, 'fixed
;; if it is 'fixed then precision must be an integer such
;;   that  0 <= precision <= 20
(define (fill-fixnum! buffer::bstring pos::bint n::bint
		      how::symbol precision::bint)
   (case how
      ((shortest)
       ;; 9.8.1
       (fill-fixnum-shortest! buffer pos n))
      ((fixed)
       ;; 15.7.4.5
       (if (zerofx? precision)
	   (fill-fixnum-shortest! buffer pos n)
	   (let ((new-pos (fill-fixnum-shortest! buffer pos n)))
	      (string-set! buffer new-pos #\.)
	      (let loop ((i 0))
		 (if (>= i precision)
		     (+fx new-pos (+fx 1 precision))
		     (begin
			(string-set! buffer (+fx new-pos (+fx i 1)) #\0)
			(loop (+fx i 1))))))))
      (else (error "fill-fixnum!"
		   "method (how) not recognized"
		   how))))

;; if the boundaries are not needed, they are set to #z0.
;; This way manipulations (multiplications) should be quite fast on them.
(define (initial-start-values d::double e need-boundaries?)
   (cond
      ((and (not need-boundaries?)
	    (>=fx e 0))
       (values (flonum->bignum d) #z1 #z0 #z0))
      ((not need-boundaries?)
       (let ((f (double-mantissa->bignum d)))
	  (values f (bit-lshbx #z1 (negfx e)) #z0 #z0)))
      (else
       ;; Here we create r, s, m- and m+
       ;; we return r, s such that d = r/s
       ;; The 's' is just a scale, so we can express the boundaries of
       ;; neighboring floating point numbers, m- and m+.
       ;; Basically everything between (v - m-)/s and (v + m+)/s will be
       ;; rounded to v. (Should a number be smaller than (v - m-)/2 it will be
       ;; rounded to v- the next smaller floating point number, etc.)
       ;;
       ;; in the paper 'p' is equal to the size of the mantissa (including
       ;; hidden bit). '^p-1?' should be read as: "is the mantissa of form
       ;; 2^(p-1) where p == mantissa-size. In other words: with the
       ;; exception of the hidden bit the whole mantissa is equal to zero.
       (let ((^p-1? (double-zero-mantissa? d)))
	  (cond
	     ((and (>=fx e 0) (not ^p-1?))
	      (let ((m+ (bit-lshbx #z1 e)))
		 (values (*bx #z2 (flonum->bignum d))    ;; r
			 #z2                             ;; s
			 m+
			 m+)))                           ;; m- == m+
	     ((>=fx e 0) ;; ^p-1
	      (let ((m- (bit-lshbx #z1 e)))
		 (values (*bx #z4 (flonum->bignum d)) ;; r
			 #z4                          ;; s
			 (*bx #z2 m-)
			 m-)))
	     ((or (=fx e *min-exponent*) ;; denormal
		  (not ^p-1?)) ;; e < 0 and not all 0s
	      (let ((f (double-mantissa->bignum d)))
		 (values (*bx #z2 f)               ;; r
			 (bit-lshbx #z1 (-fx 1 e)) ;; s
			 #z1     ;; m+
			 #z1)))  ;; m-
	     (else ;; e < 0 but not a denormal
	      (let ((f (double-mantissa->bignum d)))
		 (values (*bx #z4 f)
			 (bit-lshbx #z1 (-fx 2 e))
			 #z2
			 #z1))))))))

(define *1/log2_10* (/fl (log 2) (log 10)))

;; returns an estimation of k such that v < 10^k with v=f*2^e
;; could undershoot by 1.
;; examples:
;;  (k-estimation 0)   => 16
;;  (k-estimation -52) => 0
(define (k-estimation d e)
   ;; this estimates log10 of v where v = f*2^e
   ;; given that log10(v) == log2(v)/log2(10) and
   ;; e+(len(f)-1) is quite close to log2(v) this is simplified to
   ;; (e+len(f)-1)/log2(10). According to the paper the result can't undershoot
   ;; by more than 0.631 and to ensure that imprecisions of floating point
   ;; operations don't make us overshoot we simply substract 1e10 (thus
   ;; undershooting by at most 0.631+1e10)
   ;; Using doubles len_f = 52
   ;; The result will undershoot by at most 1.

   ;; Note: If the number was a denormal the result could overshoot.
   ;; In this case the result will be something like 0.000..e-..
   ;; Obviously the leading 0s are not needed.
   ;; TODO: special case for denormals.

   (if (=fl d 0.0)
       1
       (let* ((e+len_f-1 (fixnum->flonum (+fx e 51))))
	  (flonum->fixnum (ceilingfl (-fl (*fl e+len_f-1 *1/log2_10*)
					  1e-10))))))


(define *cached-expt10s*
   ;; doubles should never need decimal numbers with more than 326
   ;; digits. (famous last words...)
   (let ((v (make-vector 326)))
      (let loop ((i 0)
		 (expt10_i #z1))
	 (if (>= i 326)
	     v
	     (begin
		(vector-set! v i expt10_i)
		(loop (+fx i 1)
		      (*bx expt10_i #z10)))))))

(define (cached-expt10 i::bint)
   (if (and (<=fx 0 i)
	    (<=fx i 325))
       (vector-ref *cached-expt10s* i)
       (exptbx #z10 (fixnum->bignum i))))

;; take into account the 10^k-est.
;; if v was the original number, then r/s = v
;; we used 's' as scale value to get precise boundaries on neighboring values.
;; (i.e. for the m- and m+).
;; Now we include into r/s the 'k-est'.
;; If k-est is >= 0 then multiply the 'divider' s
;; if k-est is <= 0 the multiply 'r' and the boundaries.
;; we have (for the returned r and s) r/s * 10^k-est = v.
(define (scale r s m+ m- k-est)
   (if (>=fx k-est 0)
       (let ((10^k (cached-expt10 k-est)))
	  (values r (*bx s 10^k) m+ m-))
       (let ((scale (cached-expt10 (negfx k-est))))
	  (values (*bx r scale)
		  s
		  (*bx m+ scale)
		  (*bx m- scale)))))


;; k-est might have been wrong. fix this here now.
;; furthermore multiply r, m+ and m- by 10.
(define (fixup-and-*10 d r s m+ m- k-est even?)
   ;; we know that currently r/s * 10^k-est = v.
   ;; Suppose v is 250 and k-est is 1 (too low).
   ;; The returned k will be 2, and we will adjust r/s accordingly.
   ;;
   ;; The adjusted r/s will be 2.50. Indeed: 2.5 * 10^2 = 250
   ;; However. In order to get the first decimal digit we have to multiply r by
   ;;   10. (10*r)/s = 2. (integer division)
   ;; By premultiplying here, we can avoid the adjustement-multiplications.
   ;;     This is why fixup and *10 have been merged.

   (if (=fl d 0.0)
       (values r s m+ m- k-est)
       (let ((comp (if even? >=bx >bx)))
	  (if (comp (+bx r m+) s)
	      ;; k-est was too low
	      ;; normally we would need to multiply 's' by 10. However by
	      ;; merging fixup with *10 we can avoid this multiplication.
	      (values r s m+ m- (+fx k-est 1))
	      ;; k-est was correct. Now multiply by 10.
	      (values (*bx r #z10)
		      s
		      (*bx m+ #z10)
		      (*bx m- #z10)
		      k-est)))))

;; generate the digits of r/s until r is within m-/m+
;; we have 1<= r/s < 10
(define (generate-digits buffer pos r s m+ m- even?)
   (define (set-digit! n)
      (string-set! buffer pos (integer->char (+fx (char->integer #\0)
						  n)))
      (+fx pos 1))

   ;; first calculate a new digit.
   ;; if the rest is too small to make any difference we are done.
   ;; the second termination condition tc2 tests if the chosen digit was too
   ;; small.
   ;; New digit: r/s (integer division)
   ;;   Suppose the result is 3.99999 and (by integer division) 3.
   ;;   Then tc2 will find that actually 4 was the better choice.
   ;;
   ;; 'even?' is used to determine what to do in boundary cases.
   ;; If the "generated" number lies exactly between 2 floating-point-numbers
   ;; then IEEE will round to the 'even?' floating point number. If 'even?' is
   ;; set we allow hence such numbers.
   (let* ((q (bignum->fixnum (quotientbx r s)))
	  (new-r (modulobx r s))
	  (tc1 ((if even? <=bx <bx) new-r m-))
	  (tc2 ((if even? >=bx >bx) (+bx new-r m+) s)))
      (cond
	 ((and tc1 (not tc2))
	  (set-digit! q))
	 ((and (not tc1) tc2)
	  (set-digit! (+fx q 1)))
	 ((and (not tc1) (not tc2))
	  (set-digit! q)
	  (generate-digits buffer
			   (+fx pos 1)
			   (*bx new-r #z10)
			   s
			   (*bx m+ #z10)
			   (*bx m- #z10)
			   even?))
	 ;; we have a tie. if new-r < 0.5 take the smaller one.
	 ((<bx (*bx new-r #z2) s)
	  (set-digit! q))
	 (else
	  (set-digit! (+fx q 1))))))

(define (generate-counted-digits nb-digits::bint buffer::bstring pos::bint
				 r::bignum s::bignum)
   (define (set-digit! n)
      (string-set! buffer pos (integer->char (+fx (char->integer #\0)
						  n)))
      (+fx pos 1))

   (let* ((q (bignum->fixnum (quotientbx r s)))
	  (new-r (modulobx r s)))
      (cond
	 ((<=fx nb-digits 0)
	  pos)
	 ((and (=fx nb-digits 1)
	       (<bx (*bx new-r #z2) s))
	  (set-digit! q))
	 ((=fx nb-digits 1)
	  (set-digit! (+fx q 1))) ;; -> we could have 10 as digit.
	 (else
	  (set-digit! q)
	  (generate-counted-digits (-fx nb-digits 1)
				   buffer (+fx pos 1)
				   (*bx new-r #z10) s)))))

(define (difficult-fill-double! buffer::bstring pos::bint d::double
				how::symbol precision::bint)
   (let* ((need-shortest? (or (eq? how 'shortest)
			      (eq? how 'shortest-exponential)))
	  (need-boundaries? need-shortest?)
	  (e (double-exponent d))
	  ;; k-est is an estimation of 'k', the exponent of the decimal target
	  ;; number.
	  (k-est (k-estimation d e))
	  ;;
	  ;; IEEE rounds numbers lying just between two flonums to the even
	  ;; flonum. (the one with an even mantissa)
	  ;; if 'd' has an even mantissa, then we may produce a shorter decimal
	  ;; representation by taking this into account.
	  (even? (double-even-mantissa? d)))
      (receive (r s m+ m-)
	 (initial-start-values d e need-boundaries?)
	  (receive (r s m+ m-)
	     (scale r s m+ m- k-est)
	     (receive (r*10 s*10 m+*10 m-*10 k)
		(fixup-and-*10 d r s m+ m- k-est even?)
		(let ((end-pos
		       (cond
			  (need-shortest? ;; shortest and shortest-exponential
			   (generate-digits buffer pos
					    r*10 s*10 m+*10 m-*10
					    even?))
			  ;; we hack 'fixed'-cases, so they always generate the
			  ;; requested number of digits.
			  ((and (eq? how 'fixed)
				(>fx k 0))
			   (generate-counted-digits (+fx k precision)
						    buffer pos
						    r*10 s*10))
			  ((eq? how 'fixed)
			   ;; HACK...
			   ;; the number is something like 0.00abc
			   ;; "get back" to the previous case.
			   ;; by multiplying with 10^-k we start
			   ;; generating digits directly before the '.'
			   ;;
			   ;; Note: the fix-end-pos below can never affect the
			   ;; given 'k'. Our hack adds digits in front of the
			   ;; number, and the fixing thus never needs to add
			   ;; another digit in front.
			   (let* ((scale (cached-expt10 (+fx (negfx k) 1)))
				  (new-s*10 (*bx s*10 scale)))
			      (generate-counted-digits (+fx precision 1)
						       buffer pos
						       r*10 new-s*10)))
			  ((eq? how 'exponential)
			   (generate-counted-digits (+fx precision 1)
						    buffer pos
						    r*10 s*10))
			  (else ;; (eq? how 'precision)
			   (generate-counted-digits precision buffer pos
						    r*10 s*10)))))
		   ;; using generate-counted-digits it is possible that the
		   ;; very last digit is actually 10 instead one of 0-9.
		   ;; fix this here.
		   (receive (fixed-end-pos fixed-k)
		      (fix-bad-digit! buffer pos end-pos k)
		      (prettify-string! buffer pos fixed-end-pos fixed-k
					how precision))))))))

(define (string-shift! str from to len)
   (if (<fx from to)
       (let loop ((i (+fx from (-fx len 1)))
		  (j (+fx to (-fx len 1))))
	  (when (>=fx i from)
	     (string-set! str j (string-ref str i))
	     (loop (-fx i 1) (-fx j 1))))
       (let loop ((i from)
		  (j to))
	  (when (<fx i (+fx from len))
	     (string-set! str j (string-ref str i))
	     (loop (+fx i 1) (+fx j 1))))))

(define (fix-bad-digit! buffer pos end-pos k)
   (cond
      ((=fx pos end-pos)
       (values end-pos k))
      ((<=fx (char->integer (string-ref buffer (-fx end-pos 1)))
	     (char->integer #\9))
       (values end-pos k))
      (else ;; must be 10 (that is #\9 + 1)
       (string-set! buffer (-fx end-pos 1) #\0)
       (let loop ((i (-fx end-pos 2)))
	  (cond
	     ((<fx i pos) ;; we need to add a '1' in front of the generated
	      ;; digits.
	      (string-shift! buffer pos (+fx pos 1) (-fx end-pos pos))
	      (string-set! buffer pos #\1)
	      (values (+fx end-pos 1) (+fx k 1)))
	     ((char=? #\9 (string-ref buffer i))
	      (string-set! buffer i #\0)
	      (loop (-fx i 1)))
	     (else
	      (let ((ci (char->integer (string-ref buffer i))))
		 (string-set! buffer i (integer->char (+fx ci 1)))
		 (values end-pos k))))))))

;; takes k (and not the real exponent).
(define (fill-k-exponent! buffer pos k)
   (string-set! buffer pos #\e)
   (if (>fx k 0)
       (string-set! buffer (+fx pos 1) #\+)
       (string-set! buffer (+fx pos 1) #\-))
   (fill-fixnum-shortest! buffer (+fx pos 2) (absfx (-fx k 1))))
   
;; 9.8.1
;; 0 is already done (by integer-shortcut)
;; NaN and Infinity are already done, too.
;;
;; Note: in the spec k=nb-digits and n=k (power of 10.)
(define (prettify-shortest! buffer pos end-pos k)
   (let ((nb-digits (-fx end-pos pos)))
      (cond
	 ((and (<=fx nb-digits k)
	       (<=fx k 21))
	  ;; we have the first nb-digits already in.
	  ;; Simply add some 0s.
	  (let loop ((i nb-digits))
	     (cond
		((>= i k)
		 (+fx pos i))
		(else
		 (string-set! buffer (+fx pos i) #\0)
		 (loop (+fx i 1))))))
	 ((and (<fx 0 k)
	       (<=fx k 21))
	  ;; comma number. Just insert a '.' at the correct
	  ;; location.
	  (string-shift! buffer
			 (+fx pos k)
			 (+fx pos (+fx k 1))
			 (-fx nb-digits k))
	  (string-set! buffer (+fx pos k) #\.)
	  (+fx pos (+fx nb-digits 1)))
	 ((and (<fx -6 k)
	       (<=fx k 0))
	  ;; something like 0.000abcde
	  ;; add '0.' and some '0's
	  (let ((offset (-fx 2 k)))
	     (string-shift! buffer pos (+fx pos offset) nb-digits)
	     (string-set! buffer pos #\0)
	     (string-set! buffer (+fx pos 1) #\.)
	     (let loop ((i (+fx pos 2)))
		(cond
		   ((>=fx i (+fx pos offset))
		    (+fx pos (+fx nb-digits offset)))
		   (else
		    (string-set! buffer i #\0)
		    (loop (+fx i 1)))))))
	 ((=fx nb-digits 1)
	  ;; just add e...
	  (fill-k-exponent! buffer (+fx pos 1) k))
	 (else
	  ;; leave the first digit. then add a '.' and at the end
	  ;; followed by 'e'...
	  (string-shift! buffer (+fx pos 1) (+fx pos 2)
			 (-fx nb-digits 1))
	  (string-set! buffer (+fx pos 1) #\.)
	  (fill-k-exponent! buffer (+fx pos (+fx nb-digits 1)) k)))))

;; 15.7.4.5
(define (prettify-fixed! buffer pos end-pos k precision)
   (let ((nb-digits (-fx end-pos pos)))
      (cond
	 ((and (=fx precision 0)
	       (<=fx k 0))
	  (+fx pos 1))
	 ((and (=fx precision 0)
	       (>fx k 0))
	  (+fx pos k))
	 ((>fx k 0)
	  (string-shift! buffer (+fx pos k) (+fx pos (+fx k 1)) precision)
	  (string-set! buffer (+fx pos k) #\.)
	  (+fx pos (+fx 1 ;; the .
			(+fx k precision))))
	 (else ;; <=fx k 1
	  (string-shift! buffer (+fx pos 1) (+fx pos 2) precision)
	  (string-set! buffer (+fx pos 1) #\.)
	  (+fx pos (+fx 2 ;; the first digit + .
			precision))))))

;; 15.7.4.6 (when 'fractionDigits' is undefined)
(define (prettify-exponential! buffer pos k precision)
   (if (zerofx? precision)
       ;; just add the exponential
       (fill-k-exponent! buffer (+fx pos 1) k)
       ;; otherwise insert a '.'
       (begin
	  (string-shift! buffer (+fx pos 1) (+fx pos 2) precision)
	  (string-set! buffer (+fx pos 1) #\.)
	  (fill-k-exponent! buffer (+fx pos (+fx precision 2)) k))))

;; 15.7.4.7
(define (prettify-precision! buffer pos k precision)
   (cond
      ;; note: the 'e' of the spec is (-fx k 1)
      ;;       (which is different from the 'e' of 9.8.1)
      ;;   that's why we have to test against -5 and not -6 (as written in the
      ;;   spec).
      ((or (<fx k -5)
	   (>=fx (-fx k 1) precision))
       (prettify-exponential! buffer pos k (-fx precision 1)))
      ((=fx k precision)
       ;; the string is ready.
       (+fx pos precision))
      ((> k 0)
       (string-shift! buffer (+fx pos k) (+fx pos (+fx k 1)) (-fx precision k))
       (string-set! buffer (+fx pos k) #\.)
       (+fx pos (+fx precision 1))) ;; 1 for the '.'
      (else ;; k is negative, insert 0.000..
       ;; shift by 2 for the '0.' and then -k for the following '0's
       (string-shift! buffer pos (+fx pos (+fx 2 (negfx k))) precision)
       (string-set! buffer pos #\0)
       (string-set! buffer (+fx pos 1) #\.)
       (let loop ((i 0)) ;; fill the '0's
	  (when (<fx i (negfx k))
	     (string-set! buffer (+fx pos (+fx 2 i)) #\0)
	     (loop (+fx i 1))))
       (+fx pos
	    (+fx 2 ;; the '0.'
		 (+fx (negfx k) ;; '0's
		      precision))))))

(define (prettify-string! buffer pos end-pos k how precision)
   (case how
      ((shortest) (prettify-shortest! buffer pos end-pos k))
      ((fixed) (prettify-fixed! buffer pos end-pos k precision))
      ((shortest-exponential)
       (prettify-exponential! buffer pos k (-fx (-fx end-pos pos) 1)))
      ((exponential shortest-exponential)
       (prettify-exponential! buffer pos k precision))
      (else
       (prettify-precision! buffer pos k precision))))

(define (copy-string-into! str buffer pos)
   (let ((len (string-length str)))
      (blit-string! str 0 buffer pos len)
      (+fx pos len)))

;; when 'how' == 'fixed, then abs(d) must be < 10^21.
(define (fill-double! buffer::bstring pos::bint d::double
		      how::symbol precision::bint)
   (cond
      ((<fl d 0.0)
       (string-set! buffer pos #\-)
       (fill-double! buffer (+fx pos 1) (negfl d)
		     how precision))
      
      ;;---- from here on 'd' must be positive ----
      ((=fl d +inf.0)
       (copy-string-into! "Infinity" buffer pos))
      ((nanfl? d)
       (copy-string-into! "NaN" buffer pos))
      
      ;; the following takes care of all smaller integers.
      ;; a small test, that can yield huge speed improvements.
      ;	 ((and (<fl d *maxvalfx/mantissa*)
      ;	       (=fl d (fixnum->flonum (flonum->fixnum d)))
      ;	       (or (eq? how 'shortest)
      ;		   (eq? how 'fixed)))
      ;	  (fill-fixnum! buffer pos (flonum->fixnum d) how (or precision 0)))
      (else
       (difficult-fill-double! buffer pos d how precision))))

;; we could try to use a buffer here. but then we would have difficulties when
;; being multithreaded...
(define (double->string d how precision)
   (let* ((str (case how
		  ((shortest)
		   ;; 1sign + 16 decimal digits + e + '+/-' + 3exponent digits
		   ;; + some margin
		    (make-string 24))
		  ((fixed)
		   ;; 1 sign + 21 decimal digits + '.' + 20 digits + margin
		   (make-string 44))
		  ((shortest-exponential)
		   ;; as for shortest
		   (make-string 24))
		  ((exponential)
		   ;; 1 sign + 1 digit + '.' + precision digits + "e+..."
		   ;; + margin
		   (make-string (+fx precision 10)))
		  ((precision)
		   ;; 1 sign + '.' + precision digits + "e+..."
		   ;; + margin
		   (make-string (+fx precision 10)))))
	  (end-pos (fill-double! str 0 d how precision)))
      (string-shrink! str end-pos)))
