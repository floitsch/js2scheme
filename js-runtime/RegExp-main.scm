(module RegExp-main
   (import jsre-RegExp-classes
	   jsre-RegExp-fsm
	   jsre-RegExp-parse
	   jsre-RegExp-dot
	   jsre-RegExp-match)
   (main my-main))

(define (str->fsm str)
   ;(print (js-regexp->scm-regexp str))
   (scm-regexp->fsm (js-regexp->scm-regexp str) #t #t))

(define (my-main args)
   ;(regexp->dot (str->fsm "^(a)bc"))
   (print (regexp-run (str->fsm "^(a)bc") "abcd"))
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
   ;		      "a"))
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
   )
