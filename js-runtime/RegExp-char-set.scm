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

(module jsre-RegExp-char-set
   (import bitset
	   interval-avl-tree)
   (export
    (final-class RE-char-set
       (char-set-constructor)
       (bits (default #f))
       (iv-avl (default #f))) ;; interval avl tree
    (new-empty-char-set)
    (char-set-duplicate::RE-char-set re-set::RE-char-set)
    (char-set-match::bool re-set::RE-char-set c::long)
    (char-set-subset?::bool set1::RE-char-set set2::RE-char-set)
    (char-set-overlap?::bool set1::RE-char-set set2::RE-char-set)

    (char-set-add-n! re-set::RE-char-set n::bint)
    (char-set-add-ns! re-set::RE-char-set . Lns)
    (char-set-add-range-n! re-set::RE-char-set from::bint to::bint)
    (char-set-get-single-char re-set::RE-char-set)
    (char-set-copy-inverted from::RE-char-set to::RE-char-set)
    (char-set-invert::RE-char-set re-set::RE-char-set)
    (char-set-invert! re-set::RE-char-set)
    (char-set-merge-into! target::RE-char-set re-set1::RE-char-set
			  re-set2::RE-char-set)
    (char-set-out s)
    ))

(define *bitset-size* 256)

(define (new-empty-char-set)
   (instantiate::RE-char-set))

(define (char-set-constructor this)
   (with-access::RE-char-set this (bits iv-avl)
      (set! bits (make-bitset *bitset-size*))
      (set! iv-avl (make-empty-interval-avl))))

(define (char-set-duplicate re-set)
   (let ((new-set (new-empty-char-set)))
      (with-access::RE-char-set new-set (bits iv-avl)
	 (set! bits (bitset-duplicate (RE-char-set-bits re-set)))
	 (set! iv-avl (interval-avl-duplicate (RE-char-set-iv-avl re-set))))
      new-set))
   
(define (char-set-match re-set cn)
   (with-access::RE-char-set re-set (bits iv-avl)
      (if (<fx cn *bitset-size*)
	  (bitset-contains? bits cn)
	  ;; can only be inside intervals.
	  (interval-avl-contains? iv-avl cn))))

;; is set1 a subset of set2?
(define (char-set-subset? set1 set2)
   (with-access::RE-char-set set1 (bits iv-avl)
      (and (bitset-subset? bits (RE-char-set-bits set2))
	   (interval-avl-subset? iv-avl (RE-char-set-iv-avl set2)))))

;; is the intersection of set1 and set2 not empty?
(define (char-set-overlap? set1 set2)
   (or (bitsets-overlap? (RE-char-set-bits set1) (RE-char-set-bits set2))
       (interval-avl-overlap? (RE-char-set-iv-avl set1)
			      (RE-char-set-iv-avl set2))))
      

(define (char-set-add-n! re-set::RE-char-set n::bint)
   (with-access::RE-char-set re-set (bits iv-avl)
      (if (<fx n *bitset-size*)
	  (bitset-set! bits n)
	  (set! iv-avl (interval-avl-insert iv-avl n n)))))

(define (char-set-add-ns! re-set::RE-char-set . Lns)
   (for-each (lambda (n) (char-set-add-n! re-set n))
	     Lns))

(define (char-set-add-range-n! re-set::RE-char-set from::bint to::bint)
   ;; TODO: can be more efficient. no need to set each bit one by one.
   (unless (>=fx from to)
      (if (<fx from *bitset-size*)
	  (begin
	     (char-set-add-n! re-set from)
	     (char-set-add-range-n! re-set (+fx from 1) to))
	  (with-access::RE-char-set re-set (iv-avl)
	     (set! iv-avl (interval-avl-insert iv-avl from to))))))

;; returns a long, if there is only one char in the set. Otherwise
;; returns #f.
(define (char-set-get-single-char re-set::RE-char-set)
   (with-access::RE-char-set re-set (bits iv-avl)
      (let ((single-bit-pos (bitset-single-bit-pos bits))
	    (single-iv-n (interval-avl-single-n iv-avl)))
	 (cond
	    ((or (and single-bit-pos single-iv-n)
		 (and (not single-bit-pos) (not single-iv-n)))
	     #f)
	    (single-bit-pos
	     single-bit-pos)
	    (else
	     single-iv-n)))))

(define (char-set-copy-inverted from to)
   (with-access::RE-char-set to (bits iv-avl)
      (bitset-copy-inverted! (RE-char-set-bits from) bits)
      (set! iv-avl (interval-avl-invert (RE-char-set-iv-avl from)
					0 (maxvalfx)))))

(define (char-set-invert re-set)
   (let ((result (instantiate::RE-char-set)))
      (char-set-copy-inverted re-set result)
      result))

;; physically inverts the char-set.
(define (char-set-invert! re-set)
   (char-set-copy-inverted re-set re-set))

(define (char-set-merge-into! target re-set1 re-set2)
   (with-access::RE-char-set target (bits iv-avl)
      (bitset-merge! bits (RE-char-set-bits re-set2))
      (set! iv-avl (interval-avl-union (RE-char-set-iv-avl re-set1)
				       (RE-char-set-iv-avl re-set2)))))

(define (char-set-out c)
   (with-access::RE-char-set c (bits iv-avl)
      (format "~x ~x ~x ~x ~a"
	      (s64vector-ref bits 3)
	      (s64vector-ref bits 2)
	      (s64vector-ref bits 1)
	      (s64vector-ref bits 0)
	      (interval-avl-out iv-avl))))
