(module with
   (include "protobject.sch")
   (include "nodes.sch")
   (option (loadq "protobject-eval.sch"))
   (import protobject
	   symbol-table
	   verbose
	   nodes
	   var)
   (export (with! tree::pobject)))

;; whenever we encounter a 'with', we "clear" the current
;; symbol-table. Whenever we encounter a var-ref that has no var in the
;; symbol-table, we know it has been intercepted by a with.
;; this pass must change the eval-next-vars too.
(define (with! tree)
   (verbose "with-interception")
   (overload traverse with-interception (Node
					 Program
					 Fun
					 With
					 Decl-Intercept
					 Var-ref)
	     (tree.traverse #f '())))

;; surrounding-withs is a list of 'With's with their symbol-table. (ie. the
;; symbol-table when the 'With' was encountered.
(define-pmethod (Node-with-interception symbol-table surrounding-withs)
   (this.traverse2 symbol-table surrounding-withs))

(define-pmethod (Program-with-interception symbol-table surrounding-withs)
   (let ((symbol-table (add-scope (add-scope (add-scope (make-symbol-table)
							this.runtime-table)
					     this.imported-table)
				  this.globals-table)))
      (this.traverse2 symbol-table '())))

(define-pmethod (Fun-with-interception symbol-table surrounding-withs)
   (if this.local-eval?
       (begin
	  ;; fun intercepts all variables.
	  ;; this is necessary, as evals might create new variables inside the
	  ;; function
	  ;; also we need to update the 'eval-next-vars'
	  (hashtable-for-each this.locals-table
			      (lambda (id var)
				 (when (not (inherits-from? var
							    (node 'This-var)))
				    (set! var.eval-next-var
					  (update-var id var.eval-next-var
						      symbol-table
						      surrounding-withs)))))
	  (set! this.intercepted-ht (make-hashtable))
	  (set! this.obj-id this.eval-obj-id)
	  (this.traverse2 (add-scope (make-symbol-table)
				     this.locals-table)
			  (cons (cons this symbol-table)
				surrounding-withs))
	  (delete! this.intercepted-ht)
	  (delete! this.obj-id))
      (this.traverse2 (add-scope symbol-table this.locals-table)
		      surrounding-withs)))
       
(define-pmethod (With-with-interception symbol-table surrounding-withs)
   (set! this.intercepted-ht (make-hashtable))
   (this.traverse2 (make-symbol-table) (cons (cons this symbol-table)
					     surrounding-withs))
   (delete! this.intercepted-ht))

;; nearly the same as before. but this time we add the symbol-table.
(define-pmethod (Decl-Intercept-with-interception symbol-table surrounding-withs)
   (set! this.intercepted-ht (make-hashtable))
   (this.traverse2 (add-scope (make-symbol-table)
			      this.locals-table)
		   (cons (cons this symbol-table)
			 surrounding-withs))
   (delete! this.intercepted-ht))

;; note: the id is redundant...
(define (update-var id orig-var symbol-table surrounding-withs)
   (let ((var (symbol-var symbol-table id)))
      (if (or var orig-var.internal?)
	  ;; not intercepted or internal var which can't be intercepted.
	  (begin
	     (if (and var (not (eq? var orig-var)))
		 (verbose "****** " var.id))
	     orig-var)
	  ;; intercepted
	  (let* ((surrounding-with (caar surrounding-withs))
		 (intercepted-ht surrounding-with.intercepted-ht)
		 (entry (hashtable-get intercepted-ht id)))
	     (if entry
		 ;; not the first interception. just return the previously
		 ;; created intercepted var.
		 entry
		 ;; first interception.
		 ;; continue recursively: the intercepted var might be
		 ;; intercepted several times...
		 (let* ((fake-var (new-node
				   Intercepted-var
				   id
				   surrounding-with.obj-id
				   ;; continue recursively
				   (update-var id
					       orig-var
					       (cdar surrounding-withs)
					       (cdr surrounding-withs)))))
		    (set! fake-var.intercepted? #t)
		    (hashtable-put! intercepted-ht id fake-var)
		    fake-var))))))

(define-pmethod (Var-ref-with-interception symbol-table surrounding-withs)
   (unless this.var.operator? ;; operators can't be intercepted.
      (set! this.var
	    (update-var this.id this.var symbol-table surrounding-withs))))
