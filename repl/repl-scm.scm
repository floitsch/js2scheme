(module js-module-repl-scm
   (library js2scheme-runtime)
   (export init-declared-repl-scm
	   init-implicit-repl-scm
	   run-top-level-repl-scm))

(define readLine #unspecified)
(define js-display #unspecified)

(define (init-declared-repl-scm)
   (set! readLine
	 (create-declared-global (ascii->js-string-literal "readLine")))
   (set! js-display
	 (create-declared-global (ascii->js-string-literal "display"))))
(define (init-implicit-repl-scm)
   #unspecified)
(define (run-top-level-repl-scm)
   (global-set! readLine
		(js-fun
		 this #f #f #f ()
		 (utf8->js-string (read-line))))
   (global-set! js-display
		(js-fun
		 this #f #f #f (str)
		 (display (js-string->utf8 str))
		 (flush-output-port (current-output-port)))))
