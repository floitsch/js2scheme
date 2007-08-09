(module js2scheme
   (import js2scheme-comp
	   verbose)
   (main js2scheme-prog))

(define (js2scheme-prog args)
   (when (and (pair? (cdr args))
	      (string=? (cadr args) "-v"))
      (set! *verbose* #t))
   (let ((in-p (current-input-port)))
      (pp (js2scheme in-p))
      'done))
