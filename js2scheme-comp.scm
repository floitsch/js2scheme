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

(module js2scheme-comp
   (import config
	   verbose
	   nodes
	   parser
	   expand1
	   stmt-result
	   fun-bindings
	   symbol
	   with
	   ewal
	   label
	   label-resolution
	   simplify-labels
	   bind-exit
	   escape
	   simplify
	   liveness
	   let
	   arguments
	   scm-out
	   js-out)
   (export (js2scheme in-p config)
	   (js2scheme-eval in-p config top-level-obj env top-level-this))
   (from verbose))

(define (js2scheme-eval in-p config top-level-obj env top-level-this)
   (js2scheme-compil in-p config #t top-level-obj env top-level-this))

(define (js2scheme in-p config)
   (js2scheme-compil in-p config
		     #f                   ;; not eval
		     '*js-global-object*
		     '*js-global-env*
		     '*js-global-object*))

(define (js2scheme-compil in-p config
			  eval? top-level-obj env top-level-this)
   (thread-parameter-set! 'top-level-object top-level-obj)
   (thread-parameter-set! 'eval-env env)
   (thread-parameter-set! 'top-level-this top-level-this)
   (thread-parameter-set! 'eval? eval?)
   (config-init! config)
   (let ((ast (parse in-p)))
      (js-out ast)
      (fun-bindings! ast)
      (symbol-resolution! ast '())
      (stmt-result ast)
      (label-resolution ast)
      (simplify-labels! ast)
      (expand1! ast)
      (ewal ast)
      (with! ast)
      (bind-exit! ast)
      (escape ast)
      (simplify ast)
      ;; nice optimization would split the vars, and remove unnecessary
      ;; undefined var-inits.
      (liveness ast)
      (let-intro! ast)
      (arguments ast)
      (scm-out ast)
      ))
