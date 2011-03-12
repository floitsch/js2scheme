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

(module bitset
   (export (make-bitset size::bint)
	   (bitset-duplicate set)
	   (bitset-contains?::bool set i::bint)
	   (bitset-set! set i::bint)
	   (bitset-clear! set i::bint)
	   (bitset-subset?::bool set1 set2)
	   (bitsets-overlap?::bool set1 set2)
	   (bitset-single-bit-pos set)
	   (bitset-empty?::bool set)
	   (bitset-copy-inverted! from to)
	   (bitset-merge! target other)))

(define (make-bitset size)
   (let ((rem (remainder size 8)))
      (if (zerofx? rem)
	  (make-string size #a000)
	  (make-string (+fx size 1) #a000))))

(define (bitset-duplicate set)
   (string-copy set))

(define (bitset-contains? set i)
   (let* ((index (/fx i 8))
	  (offset (modulo i 8))
	  (mask (bit-lsh 1 offset)))
      (not (zerofx? (bit-and (bset-ref set index) mask)))))

(define (bitset-set! set i)
   (let* ((index (/fx i 8))
	  (offset (modulo i 8))
	  (mask (bit-lsh 1 offset)))
      (bset-set! set index (bit-or (bset-ref set index) mask))))

(define (bitset-clear! set i)
   (let* ((index (/fx i 8))
	  (offset (modulo i 8))
	  (mask (bit-lsh 1 offset)))
      (bset-set! set index (bit-and (bset-ref set index) (bit-not mask)))))

(define (bset-ref set i)
   (when (>=fx i (string-length set))
      (error "bitset" "index out of range" i))
   (char->integer (string-ref set i)))
(define (bset-set! set i new-v)
   (when (>=fx i (string-length set))
      (error "bitset" "index out of range" i))
   (string-set! set i (integer->char new-v)))
   

;; set1 subset of set2?
(define (bitset-subset? set1 set2)
   (let* ((size1 (string-length set1))
	  (size2 (string-length set2)))
      (unless (=fx size1 size2) (error "bitset" "different size" size1))
      (let loop ((i 0))
	 (if (=fx i size1)
	     #t
	     (let ((n1 (bset-ref set1 i))
		   (n2 (bset-ref set2 i)))
		(if (=fx n1 (bit-and n1 n2))
		    (loop (+fx i 1))
		    #f))))))

;; is the intersection of set1 and set2 not empty?
(define (bitsets-overlap? set1 set2)
   (let* ((size1 (string-length set1))
	  (size2 (string-length set2)))
      (unless (=fx size1 size2) (error "bitset" "different size" size1))
      (let loop ((i 0))
	 (if (=fx i size1)
	     #f
	     (let ((n1 (bset-ref set1 i))
		   (n2 (bset-ref set2 i)))
		(if (zerofx? (bit-and n1 n2))
		    (loop (+fx i 1))
		    #t))))))

;; returns either the index of the sole bit that is set to 1 or #f if none or
;; more than 1 bit are set to 1.
(define (bitset-single-bit-pos set)
   (let loop ((i 0)
	      (found-index #f))
      (if (>=fx i (string-length set))
	  found-index
	  (let ((v (bset-ref set i)))
	     (if (and found-index
		      (not (zerofx? v)))
		 ;; more than one bit set to 1
		 #f
		 (case v
		    ((0)   (loop (+fx i 1) found-index))
		    ((1)   (loop (+fx i 1) (+fx (*fx i 8) 0)))
		    ((2)   (loop (+fx i 1) (+fx (*fx i 8) 1)))
		    ((4)   (loop (+fx i 1) (+fx (*fx i 8) 2)))
		    ((8)   (loop (+fx i 1) (+fx (*fx i 8) 3)))
		    ((16)  (loop (+fx i 1) (+fx (*fx i 8) 4)))
		    ((32)  (loop (+fx i 1) (+fx (*fx i 8) 5)))
		    ((64)  (loop (+fx i 1) (+fx (*fx i 8) 6)))
		    ((128) (loop (+fx i 1) (+fx (*fx i 8) 7)))
		    (else #f))))))) ;; more than one bit set to 1

(define (bitset-empty? set)
   (let loop ((i 0))
      (if (>=fx i (string-length set))
	  #t
	  (and (zerofx? (bset-ref set i))
	       (loop (+fx i 1))))))

(define (bitset-copy-inverted! from to)
   (let* ((size1 (string-length from))
	  (size2 (string-length to)))
      (unless (=fx size1 size2) (error "bitset" "different size" size1))
      (let loop ((i 0))
	 (if (=fx i size1)
	     to
	     (let ((n (bset-ref from i)))
		(bset-set! to i (bit-and #xFF (bit-not n)))
		(loop (+fx i 1)))))))

(define (bitset-merge! target other)
   (let* ((size1 (string-length target))
	  (size2 (string-length other)))
      (unless (=fx size1 size2) (error "bitset" "different size" size1))
      (let loop ((i 0))
	 (if (=fx i size1)
	     other
	     (let ((n1 (bset-ref target i))
		   (n2 (bset-ref other i)))
		(bset-set! target i (bit-or n1 n2))
		(loop (+fx i 1)))))))
