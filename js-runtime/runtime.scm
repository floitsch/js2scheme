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

(module jsre-runtime
   (import jsre-base-object
	   jsre-ht-object
	   jsre-base-string
	   jsre-property-entry
	   jsre-undefined
	   jsre-Error
	   jsre-global-object
	   jsre-scope-object
	   jsre-globals
	   jsre-eval
	   jsre-Object
	   jsre-Array
	   jsre-Number
	   jsre-String
	   jsre-Function
	   jsre-Bool
	   jsre-Date
	   jsre-Math
	   jsre-RegExp
	   jsre-Eval-env
	   jsre-operators
	   jsre-conversion
	   jsre-Arguments)
   (from jsre-base-object
	 jsre-ht-object
	 jsre-base-string
	 jsre-property-entry
	 jsre-undefined
	 jsre-Error
	 jsre-global-object
	 jsre-scope-object
	 jsre-globals
	 jsre-eval
	 jsre-Object
	 jsre-Array
	 jsre-Function
	 jsre-String
	 jsre-Bool
	 jsre-Date
	 jsre-Math
	 jsre-RegExp
	 jsre-Eval-env
	 jsre-Number
	 jsre-operators
	 jsre-conversion
	 jsre-Arguments)
   (eval (export-all)))

;; Object-init must be before Function-init.
;; in fact: Object-init will first initialize the global Object var, which will
;; then be used by create-function-object.
(Object-init)
(Function-init)
(Array-init)
(Number-init)
(Bool-init)
(String-init)
(Date-init)
(Math-init)
(Error-init)
(RegExp-init)
(globals-init)
(global-object-init) ;; can be last.
