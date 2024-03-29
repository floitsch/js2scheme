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

(module js2scheme
   (library js2scheme-comp)
   (library js2scheme-runtime)
   (library utf)
   (main js2scheme-prog))

(define *verbose* #f)
(define *rev-in-files* '())
(define *out-file* #f)
(define *fun-strings?* #t)
(define *module?* #f)
(define *module-token* #f)
(define *strict-fun-decls* #t)
(define *liberal-objects* #f)
(define *liberal-fields* #f)

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
      ((("--stmt-funs")
	(help "Allow function declarations at all statement-positions."))
       (set! *strict-fun-decls* #f))
      ((("--non-strict-literals")
	 (help "Allow reserved identifiers as object-literal namen."))
	(set! *liberal-objects* #t))
      ((("--non-strict-fields")
	 (help "Allow reserved identifiers as dot-fields (eg. o.null)"))
	(set! *liberal-fields* #t))
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
(define (unique->init-regexps u)
   (string->symbol (format "init-regexps-~a" u)))
(define (unique->run-top-level u)
   (string->symbol (format "run-top-level-~a" u)))

(define (out-module body out-p)
   (let* ((r (random (maxvalfx)))
	  (u (or *module-token*
		 (format "~a-~a" (prefix (basename *out-file*)) r)))
	  (module-name (unique->module-name u))
	  (init-declared (unique->init-declared u))
	  (init-implicit (unique->init-implicit u))
	  (init-regexps (unique->init-regexps u))
	  (run-top-level (unique->run-top-level u)))
      (pp `(module ,module-name
	      (library js2scheme-runtime)
	      (export (,init-declared)
		      (,init-implicit)
		      (,init-regexps)
		      (,run-top-level)))
	  out-p)
      (pp `(define (,init-declared) (js-init-declared)) out-p)
      (pp `(define (,init-implicit) (js-init-implicit)) out-p)
      (pp `(define (,init-regexps) (js-init-regexps)) out-p)
      (pp `(define (,run-top-level) (js-run-top-level)) out-p)
      (pp body out-p)))

(define (create-module)
   (let ((config-ht (make-hashtable)))
      (hashtable-put! config-ht 'verbose *verbose*)
      (hashtable-put! config-ht 'function-strings *fun-strings?*)
      (hashtable-put! config-ht 'module #t)
      (hashtable-put! config-ht 'strict-fun-decls *strict-fun-decls*)
      (hashtable-put! config-ht 'liberal-object-literal-name *liberal-objects*)
      (hashtable-put! config-ht 'liberal-access-field-name *liberal-fields*)
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
   (pp
    `(define (js-main args)
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
	   ,@(map (lambda (u) `(,(unique->init-regexps u)))
		  linked-tokens)
	   ,@(map (lambda (u) `(,(unique->run-top-level u)))
		  linked-tokens)
	   (let ((js-main (js-property-get *js-global-this*
					   (utf8->js-string-literal "main"))))
	      (if (procedure? js-main)
		  (let ((tmp (js-method-call js-main
					     (utf8->js-string-literal "apply")
					     *js-global-this*
					     (scm-list->js-array args))))
		     (if (flonum? tmp)
			 (flonum->fixnum tmp)
			 0))
		  0))))
       out-p))
