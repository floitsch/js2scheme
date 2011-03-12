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

(module mset
   (export (make-mset #!key (eqtest eq?) (hash get-hashnumber))
	   (mset-put! mset val)
	   (mset-remove! mset val)
	   (mset-contains? mset val)
	   (mset->list mset)
	   (mset-for-each proc mset)
	   (mset-duplicate mset)
	   (mset-size mset)))

;; A mset is a mutable set. That is instead of returning a new set after each
;; operation it updates its state.
;; Currently based on hashtables. But should improve in the future.

(define (make-mset #!key (eqtest eq?) (hash get-hashnumber))
   (create-hashtable :size 5 :eqtest eqtest :hash hash))

(define (mset-put! mset val)
   (hashtable-put! mset val #t))

(define (mset-remove! mset val)
   (hashtable-remove! mset val))

(define (mset-contains? mset val)
   (hashtable-get mset val))

(define (mset->list mset)
   (hashtable-key-list mset))

(define (mset-for-each proc mset)
   (hashtable-for-each mset
		       (lambda (key b)
			  (proc key))))

(define (mset-duplicate mset)
   (let ((new-mset (make-mset)))
      (hashtable-for-each mset
			  (lambda (key b)
			     (hashtable-put! new-mset key #t)))))

(define (mset-size mset)
   (hashtable-size mset))
