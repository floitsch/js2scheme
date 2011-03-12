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

(module escape
   (include "tools.sch")
   (import walk
	   nodes
	   verbose)
   (export (escape tree::Program)))

;; marks escaping variables and stores them in their scope

(define (escape tree)
   (verbose " escape")
   (esc tree #f (make-eq-hashtable) (make-eq-hashtable)))

(define-nmethod (Node.esc decls-ht refs-ht)
   (default-walk this decls-ht refs-ht))

;; Program:
;; in theory we should fill the new decls-ht with the imported und runtime
;; variables. But we are not going to use the result anyways (as they must
;; escape).

(define-nmethod (Scope.esc decls-ht refs-ht)
   (let ((scope-decls-ht (make-eq-hashtable))
	 (scope-refs-ht (make-eq-hashtable)))
      (default-walk this scope-decls-ht scope-refs-ht)
      (hashtable-for-each scope-decls-ht
			  (lambda (key val)
			     (hashtable-remove! scope-refs-ht key)))
      (hashtable-for-each scope-refs-ht
			  (lambda (key val)
			     (with-access::Var key (escapes?)
				(set! escapes? #t))))
      (hashtable-for-each scope-refs-ht
			  (lambda (key val)
			     (hashtable-put! refs-ht key #t)))))

(define-nmethod (Decl.esc decls-ht refs-ht)
   (default-walk this decls-ht refs-ht)
   (hashtable-put! decls-ht (Decl-var this) #t))

(define-nmethod (Ref.esc decls-ht refs-ht)
   (default-walk this decls-ht refs-ht)
   (with-access::Ref this (var id)
      (when (not var)
	 (error #f "Internal Error." id))
      (hashtable-put! refs-ht var #t)))
