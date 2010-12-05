(module ewal
   (import symbol-table
	   verbose
	   nodes
	   walk)
   (export (ewal tree::Program)))

;; ewal needs to access runtime 'eval' directly. So 'with'-vars are not yet
;; allowed. -> this pass must happen before the with-pass.
;; with-pass however needs to update .eval-next-var. (not too clean. I know).

;; functions that (may) call 'eval' are marked as '.local-eval?'.
;; functions that have (maybe somewhere in a nested function) an eval are marked
;; as '.eval?'. They also have a .eval-obj-id which must be used to construct a
;; scope-object containing all local variables (including the parameters).
;; same is true for local variables.
;; variables that are marked as '.local-eval?' furthermore receive a
;; .eval-next-var which must be used, if the local var is deleted.
;; Potential Eval-calls are replaced by an "Eval-call". They store the
;; environment.
(define (ewal tree)
   (verbose "eval")
   (ewal! tree #f #f '() #f))

(define-nmethod (Node.ewal! symbol-table surrounding-scopes implicit-proc)
   (default-walk! this symbol-table surrounding-scopes implicit-proc))

(define-nmethod (Program.ewal! symbol-table surrounding-scopes ignored)
   (with-access::Program this
	 (implicit-globals runtime-table globals-table imported-table)
      (let ((implicit-proc (lambda (id decl)
			      (set! implicit-globals
				    (cons decl implicit-globals))
			      (with-access::Decl decl (var)
				 (scope-symbol-var-set! globals-table
							id
							var))))
	    (symbol-table (add-scope (add-scope (add-scope (make-symbol-table)
							   runtime-table)
						imported-table)
				     globals-table)))
	 (default-walk! this symbol-table
	                (cons this surrounding-scopes)
		        implicit-proc))))

(define-nmethod (Fun.ewal! symbol-table surrounding-scopes implicit-proc)
   (with-access::Fun this (locals-table eval-obj-id eval? local-eval?)
      (let ((extended-symbol-table (add-scope symbol-table locals-table)))
	 (set! eval-obj-id (gensym 'eval))
	 (default-walk! this
	                extended-symbol-table
		        (cons this surrounding-scopes)
		        implicit-proc)
	 (if (not eval?)
	     (set! eval-obj-id #f)
	     (hashtable-for-each
	      locals-table
	      (lambda (id var)
		 (when (not (This-Var? var))
		    (Var-eval?-set! var #t)
		    (Var-local-eval?-set! var local-eval?)
		    (Var-eval-obj-id-set! var eval-obj-id)
		    ;; this may create new implicit globals.
		    (let ((next-var (symbol-var symbol-table id)))
		       (if next-var
			   (Var-eval-next-var-set! var next-var)
			   (let* ((implicit-var (instantiate::Var
						   (id id)))
				  (decl (instantiate::Decl
					   (id id)
					   (var implicit-var))))
			      (implicit-proc id decl)
			      (Var-eval-next-var-set! var implicit-var)))))))))
      this))

(define-nmethod (With.ewal! symbol-table surrounding-scopes implicit-proc)
   (default-walk! this
                  symbol-table
		  (cons this surrounding-scopes)
		  implicit-proc))

   ; (set! this.eval-obj-id this.obj-id)
   ; (this.traverse3! symbol-table
   ; 		    (cons this surrounding-scopes)
   ; 		    implicit-proc)
   ; (delete! this.eval-obj-id)
   ; this)

(define-nmethod (Decl-Intercept.ewal! symbol-table surrounding-scopes
				      implicit-proc)
   (with-access::Decl-Intercept this (locals-table)
      (default-walk! this
	             (add-scope symbol-table locals-table)
		     (cons this surrounding-scopes)
		     implicit-proc)))

   ; (set! this.eval-obj-id this.obj-id)
   ; (this.traverse3! (add-scope symbol-table this.locals-table)
   ; 		    (cons this surrounding-scopes)
   ; 		    implicit-proc)
   ; (delete! this.eval-obj-id)
   ; this)
   

(define (eval-obj-id n::Node)
   (cond
      ((Program? n) (thread-parameter 'top-level-object))
      ((Fun? n) (Fun-eval-obj-id n))
      ((Decl-Intercept? n) (Decl-Intercept-obj-id n))
      ((With? n) (With-obj-id n))
      (else (error #f
		   "Internal Error. Forgot type."
		   (class-name (object-class n))))))

(define (set-eval?-to-true n::Node)
   (if (Scope? n)
       (Scope-eval?-set! n #t)
       (Intercept-eval?-set! n #t)))

(define-nmethod (Call.ewal! symbol-table surrounding-scopes implicit-proc)
   (default-walk! this symbol-table surrounding-scopes implicit-proc)
   (with-access::Call this (op args)
      (if (and (Ref? op)
	       (Runtime-Var? (Ref-var op))
	       (eq? (Var-id (Ref-var op)) 'eval))
	  (let* ((surrounding-fun/prog-scope
		  (let loop ((scopes surrounding-scopes))
		     (if (or (Program? (car scopes))
			     (Fun? (car scopes)))
			 (car scopes)
			 (loop (cdr scopes)))))
		 (top-level-object (eval-obj-id surrounding-fun/prog-scope))
		 (env-vars (map eval-obj-id
				(filter (lambda (scope)
					   (not (Program? scope)))
					surrounding-scopes))))
	     (for-each (lambda (scope)
			  (set-eval?-to-true scope))
		       surrounding-scopes)
	     (Scope-local-eval?-set! surrounding-fun/prog-scope #t)

	     (instantiate::Eval-Call
		(op op)
		(eval-scm-id (Var-scm-id (Ref-var op)))
		(args args)
		(top-level-object top-level-object)
		(env-vars env-vars)))
	  this)))
