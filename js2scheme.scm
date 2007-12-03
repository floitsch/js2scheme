(module js2scheme
   (import js2scheme-comp
	   verbose)
   (main js2scheme-prog))

(define (js2scheme-prog args)
   (let ((config-ht (make-hashtable)))
      (when (and (pair? (cdr args))
		 (string=? (cadr args) "-v"))
	 (hashtable-put! config-ht 'verbose #t))
      (let ((in-p (current-input-port)))
	 (pp (js2scheme in-p config-ht))
	 'done)))
