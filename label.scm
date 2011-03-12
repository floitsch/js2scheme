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

(module label
   (export
    (class Label
       (ids::pair-nil (default '()))
       (used?::bool (default #f))

       (generated (default #f)))
    (final-class Break-Label::Label)
    (final-class Continue-Label::Label)

    (label-add-id! label::Label id)
       
    *default-break-label-id*
    *default-continue-label-id*
    *return-label-id*
    (make-label-table)
    (label-set! label-table id label)
    (label-get label-table id)
    (label-remove! label-table id)))

;; following labels can't clash with JS-labels, as they use a dash in their id.
(define *default-break-label-id* '<*default-break*>)
(define *default-continue-label-id* '<*default-continue*>)
(define *return-label-id* '<*return-label*>)

(define (make-label-table) (make-hashtable))

(define (label-set! label-table id label)
   (hashtable-put! label-table id label))

(define (label-get label-table id)
   (hashtable-get label-table id))

(define (label-remove! label-table id)
   (hashtable-remove! label-table id))

(define (label-add-id! label id)
   (with-access::Label label (ids)
      (set! ids (cons id ids))))
