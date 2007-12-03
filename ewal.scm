(module ewal
   (include "protobject.sch")
   (include "nodes.sch")
   (option (loadq "protobject-eval.sch"))
   (import protobject
	   symbol-table
	   verbose
	   nodes
	   var)
   (export (ewal! tree::pobject)))

;; ewal needs to access runtime 'eval' directly. So 'with'-vars are not yet
;; allowed. -> this pass must happen before the with-pass.
;; with-pass however needs to update .eval-next-var. (not too clean. I know).

;; functions that (may) call 'eval' are marked as '.local-eval?'.
;; functions that have (maybe somewhere in a nested funcion) an eval are marked
;; as '.eval?'. They also have a .eval-obj-var which must be used to construct a
;; scope-object containing all local variables (including the parameters).
;; same is true for local variables.
;; variables that are marked as '.local-eval?' furthermore receive a
;; .eval-next-var which must be used, if the local var is deleted.
;; Potential Eval-calls are replaced by an "Eval-call". They store the
;; environment.
(define (ewal! tree)
   (verbose "eval")
   (overload traverse! ewal! (Node
			      Program
			      Fun
			      With
			      Decl-With
			      Call)
	     (tree.traverse! #f '() #f)))

(define-pmethod (Node-ewal! symbol-table surrounding-scopes implicit-proc)
   (this.traverse3! symbol-table surrounding-scopes implicit-proc))

(define-pmethod (Program-ewal! symbol-table surrounding-scopes ignored)
   (let ((implicit-proc (lambda (id decl)
			   (set! this.implicit-globals
				 (cons decl this.implicit-globals))
			   (scope-symbol-var-set! this.globals-table
						  id
						  decl.var)
			   (set! decl.var.implicit-global? #t)))
	 (symbol-table (add-scope (add-scope (add-scope (make-symbol-table)
							this.runtime-table)
					     this.imported-table)
				  this.globals-table)))
      (this.traverse3! symbol-table
		       (cons this surrounding-scopes)
		       implicit-proc)))

(define-pmethod (Fun-ewal! symbol-table surrounding-scopes implicit-proc)
   (let ((extended-symbol-table (add-scope symbol-table
					   this.locals-table))
	 (eval-obj-var (new-node Var (gensym 'eval))))
      (set! this.eval-obj-var eval-obj-var)
      (this.traverse3! extended-symbol-table
		       (cons this surrounding-scopes)
		       implicit-proc)
      (unless this.eval? (delete! this.eval-obj-var))
      (when this.eval?
	 (let ((local-eval? this.local-eval?))
	    (hashtable-for-each
	     this.locals-table
	     (lambda (id var)
		(when (not (inherits-from? var (node 'This-var)))
		   (set! var.eval? #t)
		   (when local-eval?
		      (set! var.local-eval? #t)
		      (set! var.eval-obj-var eval-obj-var)
		      ;; this may create new implicit globals.
		      (let ((next-var (symbol-var symbol-table id)))
			 (if next-var
			     (set! var.eval-next-var next-var)
			     (let* ((implicit-var (new-node Var id))
				    (decl (new-node Decl id)))
				(set! decl.var implicit-var)
				(implicit-proc id decl)
				(set! var.eval-next-var
				      implicit-var)))))))))))
   this)

(define-pmethod (With-ewal! symbol-table surrounding-scopes implicit-proc)
   (set! this.eval-obj-var this.obj.var)
   (this.traverse3! symbol-table
		    (cons this surrounding-scopes)
		    implicit-proc)
   (delete! this.eval-obj-var)
   this)

(define-pmethod (Decl-With-ewal! symbol-table surrounding-scopes implicit-proc)
   (set! this.eval-obj-var this.obj.var)
   (this.traverse3! (add-scope symbol-table this.locals-table)
		    (cons this surrounding-scopes)
		    implicit-proc)
   (delete! this.eval-obj-var)
   this)
   

(define-pmethod (Call-ewal! symbol-table surrounding-scopes implicit-proc)
   (this.traverse3! symbol-table surrounding-scopes implicit-proc)
   (if (and (inherits-from? this.op (node 'Var-ref))
	    (inherits-from? this.op.var (node 'Runtime-var))
	    (eq? this.op.var.id 'eval))
       (let* ((surrounding-fun/prog-scope
	       (let loop ((scopes surrounding-scopes))
		  (if (or (inherits-from? (car scopes) (node 'Program))
			  (inherits-from? (car scopes) (node 'Fun)))
		      (car scopes)
		      (loop (cdr scopes)))))
	      (top-level-object (if (inherits-from? surrounding-fun/prog-scope
						    (node 'Program))
				    (thread-parameter 'top-level-object)
				    surrounding-fun/prog-scope.eval-obj-var))
	      (env-vars (map (lambda (scope) scope.eval-obj-var)
			    (filter (lambda (scope)
				       (not (inherits-from? scope
							    (node 'Program))))
				    surrounding-scopes))))
	  (for-each (lambda (scope) (set! scope.eval? #t))
		    surrounding-scopes)
	  (set! surrounding-fun/prog-scope.local-eval? #t)
       
	  (new-node Eval-call this.op this.args top-level-object env-vars))
       this))
