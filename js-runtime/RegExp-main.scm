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

(module RegExp-main
   (import jsre-RegExp-classes
	   jsre-RegExp-fsm
	   jsre-RegExp-parse
	   jsre-RegExp-dot
	   jsre-RegExp-match)
   (main my-main))

(define (str->fsm str)
   ;(print (js-regexp->scm-regexp str))
   (scm-regexp->fsm (js-regexp->scm-regexp str)))

(define *use-bigloo-regexp* #f)

(define (my-main args)
   (when (and (pair? (cdr args))
	      (string=? (cadr args) "-v"))
      (set! *debug* #t)
      (set-cdr! args (cddr args)))
   (when (and (pair? (cdr args))
	      (string=? (cadr args) "-b"))
      (set! *use-bigloo-regexp* #t)
      (set-cdr! args (cddr args)))
   ;(regexp->dot (str->fsm "^(a)bc"))
   ;(print (regexp-run (str->fsm "^(a)bc") "abcd"))
   ;(regexp->dot (scm-regexp->fsm (js-regexp->scm-regexp "^ab(c|d)*e+?[^f][g-i-j]$k(?:l|mno)$") #f #f))
   ;(print (regexp-run (str->fsm "^ab(c|d)*e+?[^f][g-i-j]$k(?:l|mno)$") "abdcdcee-\nkmnt"))
   ;(regexp->dot (str->fsm "^ab(c|d)*e+"))
   ;(print (regexp-run (str->fsm "^e+") "e"))
   ;(regexp->dot (str->fsm "^e+"))
   ;(print (regexp-run (str->fsm "^ab(c|d)*(c*)(d*)e+?[^f][g-i-j]$\\s") "abdcdcee-\n"))
   ;(regexp->dot (scm-regexp->fsm (js-regexp->scm-regexp "^ab(c|d)*e{3}?") #f #f))
   ;(print (regexp-run (str->fsm "^ab(c|d)*e{3}?(.*k)?")
   ;		      "abdcddeeeeeeeebbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"))
   ;(print (regexp-run (str->fsm "a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a{30}")
   ;		      "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"))
   ;(print (regexp-run (str->fsm "a?a?a{2}")
   ;		      "aa"))
   ;(print (regexp-run (str->fsm "(X+X+)+Y")
   ;		      "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"))
   ;(regexp->dot (str->fsm "(a?){2}"))
   ;(regexp->dot (scm-regexp->fsm (js-regexp->scm-regexp "abc{4,3}") #f #f))
   ;(regexp->dot (scm-regexp->fsm (js-regexp->scm-regexp "c{1,}") #f #f))
   ;(regexp->dot (scm-regexp->fsm (js-regexp->scm-regexp "c{1,2}") #f #f))
   ;(regexp->dot (scm-regexp->fsm (js-regexp->scm-regexp "c{1,2}}") #f #f))
   ;(regexp->dot (scm-regexp->fsm (js-regexp->scm-regexp "c{1,2}{3,2}") #f #f))
   ;(regexp->dot (scm-regexp->fsm (js-regexp->scm-regexp "c{1,2}{3,2}") #f #f))
   ;(regexp->dot (scm-regexp->fsm (js-regexp->scm-regexp "c)") #f #f))
   ;(print (regexp-run (str->fsm "(ab)\\1")
   ;		      "abab"))
   ;(print (regexp-run (str->fsm "(ab)\\1")
   ;		      "abab"))
   ;(print (regexp-run (str->fsm ".*(a|aa).*\\1")
   ;		      "aaaaaaaaaaaaaaaaaaba"))
   ;(regexp->dot (str->fsm ".*(aa|a).*\\1"))
   (if *use-bigloo-regexp*
       (print (pregexp-match (cadr args) (read-string)))
       (let ((re (str->fsm (cadr args)))
	     (str (utf8->js-string (read-string))))
	  (tprint "big-regexp: " (pregexp (cadr args)))
	  (tprint "my-regexp:  " (js-regexp->scm-regexp (cadr args)))
	  (tprint "running reg-exp")
	  (print (regexp-match re str 0))))
   )
