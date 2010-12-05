(module with
   (import nodes
	   walk
	   symbol-table
	   verbose)
   (static (class Interception-Construct
	      intercepted-ht ;; hashtable of (potentially) intercepted variables.
	      obj-id  ;; the obj-id that intercepts.
	      symbol-table))
   (export (with! tree::Program)))

;; whenever we encounter a 'with', we "clear" the current
;; symbol-table. Whenever we encounter a var-ref that has no var in the
;; symbol-table, we know it has been intercepted by a with.
;; this pass must change the eval-next-vars too.
(define (with! tree)
   (verbose "with-interception")
   (with-interception tree #f #f '()))

;; surrounding-interceptors is a list of Interception-Constructs.
;; It mainly contains 'with's but also catch, named-fun, and funs (if they
;; contain evals).
(define-nmethod (Node.with-interception symbol-table surrounding-interceptors)
   (default-walk this symbol-table surrounding-interceptors))

(define-nmethod (Program.with-interception symbol-table surrounding-interceptors)
   (with-access::Program this (runtime-table imported-table globals-table)
      (let ((symbol-table (add-scope (add-scope (add-scope (make-symbol-table)
							   runtime-table)
						imported-table)
				     globals-table)))
	 (default-walk this symbol-table '()))))

(define-nmethod (Fun.with-interception symbol-table surrounding-interceptors)
   (with-access::Fun this (local-eval? locals-table eval-obj-id)
      (if local-eval?
	  (begin
	     ;; fun intercepts all variables.
	     ;; this is necessary, as evals might create new variables inside the
	     ;; function
	     ;; also we need to update the 'eval-next-vars'
	     (hashtable-for-each
	      locals-table
	      (lambda (id var)
		 (when (not (This-Var? var))
		    (with-access::Var var (eval-next-var)
		       (set! eval-next-var
			     (update-var id eval-next-var
					 symbol-table
					 surrounding-interceptors))))))
	     (default-walk this
		           (add-scope (make-symbol-table)
				      locals-table)
			   (cons (instantiate::Interception-Construct
				    (intercepted-ht (make-hashtable))
				    (obj-id eval-obj-id)
				    (symbol-table symbol-table))
				 surrounding-interceptors)))
	  (default-walk this (add-scope symbol-table locals-table)
	                surrounding-interceptors))))
       
(define-nmethod (With.with-interception symbol-table surrounding-interceptors)
   (with-access::With this (obj-id obj body)
      (walk obj symbol-table surrounding-interceptors)
      (walk body
	    (make-symbol-table)
	    (cons (instantiate::Interception-Construct
		     (intercepted-ht (make-hashtable))
		     (obj-id obj-id)
		     (symbol-table symbol-table))
		  (surrounding-interceptors)))))

;; nearly the same as before. but this time we add the symbol-table.
(define-nmethod (Decl-Intercept.with-interception symbol-table
						  surrounding-interceptors)
   (with-access::Decl-Intercept this (locals-table obj-id)
      (default-walk this
	            (add-scope (make-symbol-table) locals-table)
		    (cons (instantiate::Interception-Construct
			     (intercepted-ht (make-hashtable))
			     (obj-id obj-id)
			     (symbol-table symbol-table))
			  surrounding-interceptors))))

;; note: the id is redundant...
(define (update-var id orig-var::Var symbol-table surrounding-interceptors)
   (let ((var (symbol-var symbol-table id)))
      (if (or var (Var-internal? orig-var))
	  ;; not intercepted or internal var which can't be intercepted.
	  (begin
	     (when (and var (not (eq? var orig-var)))
		(verbose "****** " (Var-id var)))
	     orig-var)
	  ;; intercepted
	  (with-access::Interception-Construct (car surrounding-interceptors)
		(intercepted-ht obj-id symbol-table)
	     (let ((entry (hashtable-get intercepted-ht id)))
		(if entry
		 ;; not the first interception. just return the previously
		 ;; created intercepted var.
		 entry
		 ;; first interception.
		 ;; continue recursively: the intercepted var might be
		 ;; intercepted several times...
		 ;;
		 ;; Note that the symbol-table in the call to update-var is the
		 ;; one from the Interception-Construct and not the one given
		 ;; to the function.
		 (let* ((intercepted-var
			 (update-var id
				     orig-var
				     symbol-table
				     (cdr surrounding-interceptors)))
			(fake-var (instantiate::Intercepted-Var
				     (id id)
				     (obj-id obj-id)
				     (intercepted intercepted-var))))
		    (hashtable-put! intercepted-ht id fake-var)
		    fake-var)))))))

(define-nmethod (Ref.with-interception symbol-table surrounding-interceptors)
   (with-access::Ref this (var id)
      (with-access::Var var (operator?)
	 (unless operator? ;; operators can't be intercepted.
	    (set! var
		  (update-var id var symbol-table surrounding-interceptors))))))
