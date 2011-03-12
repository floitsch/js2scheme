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

(module fun-bindings
   (import nodes
	   walk
	   verbose)
   (export (fun-bindings! tree::Program)))

;; move fun-bindings (declarations) to beginning of functions.
;; order matters. (do not inverse declarations)

(define (fun-bindings! tree)
   (verbose "fun-bindings")
   (fb! tree #f #f))

(define-nmethod (Node.fb! rev-fun-container)
   (default-walk! this rev-fun-container))

(define-nmethod (Scope.fb! rev-fun-container)
   (with-access::Scope this (body)
      (let* ((container (list '*rev-fun-container*))
	     (new-body (walk! body container))
	     (bindings (reverse! (cdr container))))
	 (if (null? bindings)
	     (set! body new-body)
	     (set! body (instantiate::Block
			   (els (append! bindings (list new-body))))))
	 this)))

(define-nmethod (Fun-Binding.fb! rev-fun-container)
   (default-walk! this rev-fun-container)
   (set-cdr! rev-fun-container (cons this (cdr rev-fun-container)))
   (instantiate::NOP))
