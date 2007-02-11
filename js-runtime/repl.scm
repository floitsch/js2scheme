(module jsre-repl
   (include "macros.sch")
   (import jsre-runtime)
   (main main))

(define (main args)
   (let ((show-prompt? (= (length args) 1)))
      (with-output-to-string (lambda () (eval '(load "/home/flo/programming/js/js-tools/js-runtime/macros.sch"))))
      (let loop ((got-something? #f))
	 (if show-prompt? (display "> "))
	 (let ((expr (read)))
	    (if (eof-object? expr)
		(if got-something?
		    0
		    1)
	       (let ((res (eval expr)))
		  (if show-prompt? (print res))
	       (loop #t)))))))
