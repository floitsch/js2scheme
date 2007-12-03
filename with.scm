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
					 Decl-With
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
   (let ((extended-symbol-table (add-scope symbol-table
					   this.locals-table)))
      (when this.local-eval?
	 (hashtable-for-each this.locals-table
			     (lambda (id var)
				(when (not (inherits-from? var
							   (node 'This-var)))
				   (set! var.eval-next-var
					 (update-var id var.eval-next-var
						     symbol-table
						     surrounding-withs))))))
      (this.traverse2 extended-symbol-table surrounding-withs)))
       
(define-pmethod (With-with-interception symbol-table surrounding-withs)
   (set! this.intercepted-ht (make-hashtable))
   (this.traverse2 (make-symbol-table) (cons (cons this symbol-table)
					     surrounding-withs))
   (hashtable-for-each this.intercepted-ht
		       (lambda (ignored var)
			  (set! var.intercepted? #t))))

;; nearly the same as before. but this time we add the symbol-table.
(define-pmethod (Decl-With-with-interception symbol-table surrounding-withs)
   (set! this.intercepted-ht (make-hashtable))
   (this.traverse2 (add-scope (make-symbol-table)
			      this.locals-table)
		   (cons (cons this symbol-table)
			 surrounding-withs))
   (hashtable-for-each this.intercepted-ht
		       (lambda (ignored var)
			  (set! var.intercepted? #t))))
   

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
		 ;; build pseudo-decl and put it into With-interceptions.
		 ;; continue recursively: the intercepted var might be
		 ;; intercepted several times...
		 (let* ((decl (new-node Decl id))
			(fake-var (new-node With-var
				       id
				       surrounding-with
				       ;; continue recursively
				       (update-var id
						   orig-var
						   (cdar surrounding-withs)
						   (cdr surrounding-withs)))))
		   (set! decl.var fake-var)
		   (hashtable-put! intercepted-ht id fake-var)
		   fake-var))))))

(define-pmethod (Var-ref-with-interception symbol-table surrounding-withs)
   (set! this.var (update-var this.id this.var symbol-table surrounding-withs)))
