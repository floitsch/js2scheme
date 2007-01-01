(module verbose
   (export (verbose . L)
	   *verbose*))

(define *verbose* #f)

(define (verbose . L)
   (if *verbose*
       (with-output-to-port (current-error-port)
	  (lambda ()
	     (apply print L)))))

;; testy
