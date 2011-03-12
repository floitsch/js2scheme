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

(module symbol-table
   (export
    (inline make-symbol-table)
    (inline make-scope-table)
    (inline add-scope symbol-table scope-table)
    (inline symbol-var symbol-table symbol)
    (inline local-symbol-var symbol-table symbol)
    (inline symbol-var-set! symbol-table symbol var)
    (inline scope-symbol-var-set! scope symbol var)
    (inline scope-symbol-var-rm! scope symbol)
    (inline scope-symbol-var scope symbol)))


(define-inline (make-symbol-table) '())
(define-inline (make-scope-table) (make-hashtable))

(define-inline (add-scope symbol-table scope-table)
   (cons scope-table symbol-table))

(define-inline (symbol-var symbol-table symbol)
   (any (lambda (ht)
	   (hashtable-get ht symbol))
	symbol-table))

(define-inline (local-symbol-var symbol-table symbol)
   (and (pair? symbol-table)
	(hashtable-get (car symbol-table) symbol)))

(define-inline (symbol-var-set! symbol-table symbol var)
   (scope-symbol-var-set! (car symbol-table) symbol var))

(define-inline (scope-symbol-var-set! scope symbol var)
   (hashtable-put! scope symbol var))

(define-inline (scope-symbol-var-rm! scope symbol)
   (hashtable-remove! scope symbol))

(define-inline (scope-symbol-var scope symbol)
   (hashtable-get scope symbol))

