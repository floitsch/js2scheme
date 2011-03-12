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

(module jsre-Arguments
   (import jsre-scope-object
	   jsre-ht-object
	   jsre-property-entry
	   jsre-base-string)
   (use jsre-base-object
	jsre-undefined
	jsre-conversion)
   (export (class Js-Arguments::Js-Scope-Object))
   (export (macro make-arguments))
   (eval (class Js-Arguments)))

;; 10.1.8
(define-macro (make-arguments nb-named-params
			      callee nb-args param-vars par-vec)
   (let ((arguments (gensym 'arguments))
	 (counter (gensym 'counter))
	 (new-val (gensym 'new-val))
	 (nb-named (length param-vars)))
      `(let ((,arguments (instantiate::Js-Arguments
			    (props (make-props-hashtable))
			    (proto (natO-object-prototype)))))
	  (js-property-generic-set! ,arguments
				    (STR "callee")
				    ,callee
				    (get-Attributes dont-enum))
	  (js-property-generic-set! ,arguments
				    (STR "length")
				    (fixnum->flonum ,nb-args)
				    (get-Attributes dont-enum))
	  ;; named vars are added as scope-vars
	  ,@(map (lambda (id c)
		    `(when (< ,c ,nb-args)
			(scope-var-add ,arguments
				       (STR ,(integer->string c))
				       ,id
				       (get-Attributes dont-enum))))
		 param-vars
		 (iota nb-named))
	  ;; remaining ones are added as vector-refs
	  (for-each (lambda (,counter)
		       (js-property-generic-set!
			,arguments
			(integer->js-string ,counter)
			(instantiate::Js-Ref
			   (getter (lambda ()
				      (vector-ref ,par-vec
						  (- ,counter
						     ,nb-named-params))))
			   (setter (lambda (,new-val)
				      (vector-set! ,par-vec
						   (- ,counter
						      ,nb-named-params)
						   ,new-val))))
			(get-Attributes dont-enum)))
		    (iota (- ,nb-args ,nb-named) ,nb-named))
	  ,arguments)))

;; 10.1.8
(define-method (js-class-name o::Js-Arguments)
   ;; The actual class-name is not specified.
   ;; Some interpreters use "Arguments" others "Object".
   (STR "Arguments"))

(define-method (js-property-safe-delete!::bool o::Js-Arguments prop)
   ;; shortcut the scope-object and really delete the object.
   (js-property-direct-delete! o prop))
