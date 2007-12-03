(module verbose
   (import config)
   (export (verbose . L)))

(define (verbose . L)
   (if (config 'verbose)
       (with-output-to-port (current-error-port)
	  (lambda ()
	     (apply print L)))))
