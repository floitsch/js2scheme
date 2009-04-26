(module extract-globals
   (main my-main))

(define (starts-with? s prefix)
   (if (symbol? s)
       (starts-with? (symbol->string s) prefix)
       (let ((prefix-length (string-length prefix)))
	  (and (>= (string-length s) prefix-length)
	       (string=? prefix (substring s 0 prefix-length))))))

(define (extract-define-globals expr)
   (map (lambda (def)
	   (let ((id (if (pair? (cadr def))
			 (caadr def)
			 (cadr def))))
	      (list id (symbol-append 'jsg- id))))
	(cdr expr)))

(define (extract-operator expr)
   (let* ((formals (cadr expr))
	  (id (car formals))
	  (id-str (symbol->string id)))
      (let* ((id-w/o-prefix-str (substring id-str
					   (string-length "jsop-")
					   (string-length id-str)))
	     (id-w/o-prefix (string->symbol id-w/o-prefix-str)))
	 (list (list id-w/o-prefix id 'operator)))))

(define (search-for-creations expr)
   (match-case expr
      ((module . ?-)
       '())
      ((define-runtime-globals ???-)
       (extract-define-globals expr))
      ((define-inline ((? jsop-symbol?) ???-) ???-)
       (extract-operator expr))
      ((define-macro ((? jsop-symbol?) ???-) ???-)
       (extract-operator expr))
      ((define-macro . ?-)
       '())
      ((define ((? jsop-symbol?) ???-) ???-)
       (extract-operator expr))
      ((define (?- . ?-) . ?-)
       (search-for-creations (cddr expr)))
      (((or define set!)
	?scm-id
	((or create-runtime-global
	     create-special-global)
	 ?js-id
	 . ?-))
       [assert (js-id) (and (pair? js-id) (eq? (car js-id) 'STR))]
       (list (list (string->symbol (cadr js-id)) scm-id)))
      ((?- . ?-) ;; pair
       (let loop ((expr expr)
		  (res '()))
	  (cond
	     ((null? expr) res)
	     ((pair? expr)
	      (loop (cdr expr)
		    (append (search-for-creations (car expr))
			    res)))
	     (else
	      (append (search-for-creations expr)
		      res)))))
      (else
       '())))

(define (jsop-symbol? s)
   (starts-with? s "jsop-"))

(define *in-files* '())
(define *output* #f)

(define (extract-globals files)
   (let loop ((files files)
	      (bindings '()))
      (if (null? files)
	  bindings
	  (begin
	     (display (format "extracting globals from: ~a\n" (car files))
		      (current-error-port))
	     (with-input-from-file (car files)
		(lambda ()
		   (let liip ((bindings bindings))
		      (let ((expr (read)))
			 (if (eof-object? expr)
			     (loop (cdr files) bindings)
			     (liip (append (search-for-creations expr)
					   bindings)))))))))))
   
(define (my-main args)
   (args-parse (cdr args)
      (("-o" ?file (help "Output file"))
       (set! *output* file))
      (else
       (set! *in-files* (cons else *in-files*))))
   (let ((out-p (if (not *output*)
		    (current-output-port)
		    (open-output-file *output*))))
      (with-handler
	 (lambda (e)
	    (when *output*
	       (delete-file *output*))
	    (raise e))
	 (unwind-protect
	    (begin
	       (display ";; GENERATED FILE -- DO NOT EDIT\n" out-p)
	       (display ";; GENERATED FILE -- DO NOT EDIT\n" out-p)
	       (display ";; GENERATED FILE -- DO NOT EDIT\n" out-p)
	       (display ";; GENERATED FILE -- DO NOT EDIT\n" out-p)
	       (display ";; GENERATED FILE -- DO NOT EDIT\n" out-p)
	       (let ((bindings (extract-globals *in-files*)))
		  (pp `(define *runtime-variables* ',bindings) out-p)))
	    (close-output-port out-p)))))
