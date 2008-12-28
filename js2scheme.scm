(module js2scheme
   (library js2scheme-comp)
   (main js2scheme-prog))

(define *verbose* #f)
(define *rev-in-files* '())
(define *out-file* #f)
(define *fun-strings?* #t)
(define *module?* #f)
(define *module-token* #f)

(define (parse-arguments args)
   (args-parse args
      (section "Help")
      (("?")
       (args-parse-usage #f))
      ((("-h" "--help") (help "?,-h,--help" "This help message"))
       (args-parse-usage #f))
      (section "Misc")
      ((("--no-fun-source") (help "Do not include the source of functions."))
       (set! *fun-strings?* #f))
      ((("-v" "--verbose") (help "Verbose output"))
       (set! *verbose* #t))
      (("-c" (help "Create module file."))
       (set! *module?* #t))
      (("-m" ?name
	(help "Give unique module-token. (otherwise a random-nb is chosen)."))
       (when (string-null? name)
	  (error 'js2scheme
		 "unique token must not be the empty string."
		 ""))
       (set! *module-token* name))
      (("-o" ?file         (help "Output file"))
       (set! *out-file* file))
      (else
       (set! *rev-in-files* (cons else *rev-in-files*))))
   (when (null? *rev-in-files*)
      (error 'js2scheme
	     "No input file given"
	     #f))
   (when (and *module?* (not (null? (cdr *rev-in-files*))))
      (error 'js2scheme
	     "-c flag only works with one input-file"
	     *rev-in-files*))
   (when (and (not *out-file*)
	      (not *module?*))
      (error 'js2scheme
	     "link-module requires explicit output-file"
	     #f))
   (when (and (not *out-file*)
	      *module?*
	      (string=? (car *rev-in-files*) "-"))
      (error 'js2scheme
	     "with input from std-in an output file (can be '-') must be given."
	     #f))
   (when (not *out-file*)
      (set! *out-file* (string-append (prefix (basename (car *rev-in-files*)))
				      ".scm"))))


(define (js2scheme-prog args)
   (seed-random! (llong->fixnum (current-microseconds)))
   (parse-arguments (cdr args))
   (if *module?*
       (create-module)
       (create-linked)))

;; ============== module-creation.

(define *module-prefix* "js-module-")
(define (unique->module-name u)
   (string->symbol (format "~a~a" *module-prefix* u)))
(define (unique->init-declared u)
   (string->symbol (format "init-declared-~a" u)))
(define (unique->init-implicit u)
   (string->symbol (format "init-implicit-~a" u)))
(define (unique->run-top-level u)
   (string->symbol (format "run-top-level-~a" u)))

(define (out-module body out-p)
   (let* ((r (random (maxvalfx)))
	  (u (or *module-token*
		 (format "~a-~a" (prefix (basename *out-file*)) r)))
	  (module-name (unique->module-name u))
	  (init-declared (unique->init-declared u))
	  (init-implicit (unique->init-implicit u))
	  (run-top-level (unique->run-top-level u)))
      (pp `(module ,module-name
	      (library js2scheme-runtime)
	      (export (,init-declared)
		      (,init-implicit)
		      (,run-top-level)))
	  out-p)
      (pp `(define (,init-declared) (js-init-declared)) out-p)
      (pp `(define (,init-implicit) (js-init-implicit)) out-p)
      (pp `(define (,run-top-level) (js-run-top-level)) out-p)
      (pp body out-p)))

(define (create-module)
   (let ((config-ht (make-hashtable)))
      (hashtable-put! config-ht 'verbose *verbose*)
      (hashtable-put! config-ht 'function-strings *fun-strings?*)
      (hashtable-put! config-ht 'module #t)
      (let ((in-p (if (string=? (car *rev-in-files*) "-")
		      (current-input-port)
		      (open-input-file (car *rev-in-files*)))))
	 (unwind-protect
	    (let ((res (js2scheme in-p config-ht)))
	       (let ((out-p (if (string=? *out-file* "-")
				(current-output-port)
				(open-output-file *out-file*))))
		  (unwind-protect
		     (out-module res out-p)
		     (close-output-port out-p))))
	    (close-input-port in-p)))))


;; ===========linked creation

(define (create-linked)
   (let loop ((rev-files *rev-in-files*)
	      (tokens '()))
      (if (null? rev-files)
	  (out-linked-module tokens)
	  (let ((file (car rev-files)))
	     (unless (file-exists? file)
		(error 'js2scheme
		       "file not found"
		       file))
	     (let ((in-p (open-input-file file)))
		(unwind-protect
		   (let ((module-clause (read in-p)))
		      (unless (and (pair? module-clause)
				   (eq? (car module-clause) 'module)
				   (pair? (cdr module-clause))
				   (symbol? (cadr module-clause)))
			 (error 'js2scheme
				"file is not a js2scheme-module"
				file))
		      (let* ((module-name (cadr module-clause))
			     (m-name-str (symbol->string module-name)))
			 (unless (and (string-prefix? *module-prefix*
						      m-name-str)
				      (>fx (string-length m-name-str)
					   (string-length *module-prefix*)))
			    (error 'js2scheme
				   "file is not a js2scheme-module"
				   file))
			 (loop (cdr rev-files)
			       (cons (substring m-name-str
						(string-length *module-prefix*)
						(string-length m-name-str))
				     tokens))))
		   (close-input-port in-p)))))))

(define (out-linked-module tokens)
   (let ((out-p (open-output-file *out-file*)))
      (unwind-protect
	 (begin
	    (pp `(module js-main-linked
		    (main js-main)
		    (library js2scheme-runtime)
		    (import ,@(map unique->module-name tokens)))
		out-p)
	    (out-main tokens out-p))
	 (close-output-port out-p))))

(define (out-main linked-tokens out-p)
   (pp `(define (js-main args)
	   ,@(map (lambda (u) `(,(unique->init-declared u)))
		  linked-tokens)
	   ,@(map (lambda (u) `(,(unique->init-implicit u)))
		  linked-tokens)
	   (with-handler
	      (lambda (e)
		 (if (Js-Object? e)
		     (with-output-to-port (current-error-port)
			(lambda ()
			   (print "caught Js-Object in Main")
			   (print (any->safe-string e))))
		     (with-output-to-port (current-error-port)
			(lambda ()
			   (print "caught Error in Main")
			   (print (&error-fname e))
			   (print (&error-location e))
			   (print (&error-proc e))
			   (print (any->safe-string
				   (error->js-exception e))))))
		 (exit 3))
	      ,@(map (lambda (u) `(,(unique->run-top-level u)))
		     linked-tokens)
	      (let ((js-main (js-property-get *js-global-this* "main")))
		 (if (procedure? js-main)
		     (let ((tmp (js-method-call js-main "apply"
						*js-global-this*
						(scm-list->js-array args))))
			(if (flonum? tmp)
			    (flonum->fixnum tmp)
			    0))
		     0))))
       out-p))
