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

(module arguments
   (import walk
	   nodes
	   verbose)
   (export (arguments tree::Program)))

(define (arguments tree)
   (verbose "arguments")
   (args tree #f))

(define-nmethod (Node.args)
   (default-walk this))

(define-nmethod (Fun.args)
   (with-access::Fun this (eval? arguments-decl locals-table body)
      (when (and eval?
		 ;; don't add it, if the var is shadowed.
		 (eq? (hashtable-get locals-table 'arguments)
		      (Ref-var arguments-decl)))
	 (with-access::Var (Ref-var arguments-decl) (arguments-used?)
	    (set! arguments-used? #t)))
      ;; don't go into arguments-decl (nor args)
      (walk body)))

(define-nmethod (Ref.args)
   (with-access::Ref this (var)
      (with-access::Var var (arguments? arguments-used?)
	 (when arguments?
	    (set! arguments-used? #t)))))
