(module symbol
   (include "js-runtime/runtime-variables.sch")
   (import nodes
	   symbol-table
	   verbose
	   walk)
   (export (symbol-resolution! tree::Program imported-ids::pair-nil)
	   (id->runtime-var id::symbol)))

(define (id->runtime-var id)
   (hashtable-get (thread-parameter '*runtime-table*) id))
(define (runtime-table-set! table)
   (thread-parameter-set! '*runtime-table* table))

(define (symbol-resolution! tree imported-ids)
   (verbose "symbol-resolution")
   (verbose " collect")
   (start-collect tree imported-ids)
   (verbose " resolve")
   (resolve! tree #f #f #f))

;; See 10.1.3 of Ecmascript spec.
;; Basically:
;; each param gets a var (even those with same name). If 2 params have same
;; name, last param wins.
;; Every 'var' statement declares var. If one already exists (for instance
;; param, or other 'var') nothing happens.
;; Function declarations win over 'var'-statements. This is only important when
;; initializing (setting local vars to 'undefined')
;;
;; In order to minimize set!s we allocate different vars if they are shadowed.
;; ex: fun f() {
;;        fun g() { ... };
;;        fun g() { .2. };
;;     }
;; the second g shadows the first one. -> rename the first one.
;; same is true, if function shadows parameter.

   
(define (start-collect program::Program imported-ids)
   (let* ((runtime-table (make-scope-table))
	  (imported-table (make-scope-table))
	  (globals-table (make-scope-table)))
      (for-each (lambda (runtime-var-binding)
		   (let* ((id (car runtime-var-binding))
			  (scm-id (cadr runtime-var-binding))
			  (v (instantiate::Runtime-Var
				(id id)
				(scm-id scm-id)
				(global? #t))))
		      (when (and (not (null? (cddr runtime-var-binding)))
				 (eq? (caddr runtime-var-binding) 'operator))
			  (Var-operator?-set! v #t))
		      (hashtable-put! runtime-table
				      id
				      v)))
		*runtime-variables*)
      (for-each (lambda (imported-var-binding)
		   (let* ((id (car imported-var-binding))
			  (scm-id (cadr imported-var-binding))
			  (v (instantiate::Imported-Var
				(id id)
				(scm-id scm-id)
				(global? #t))))
		      (hashtable-put! imported-table
				      id
				      v)))
		imported-ids)

      (runtime-table-set! runtime-table)
      (Program-imported-table-set! program imported-table)
      (Program-runtime-table-set! program runtime-table)
      (Program-globals-table-set! program globals-table)
      (collect program #f globals-table)))

(define-nmethod (Node.collect scope-table)
   (default-walk this scope-table))

(define-nmethod (Program.collect scope-table)
   (default-walk this scope-table)
   (with-access::Program this (declared-globals)
      (set! declared-globals (hashtable->list scope-table))
      (for-each (lambda (var)
		   (with-access::Var var (global?)
		      (set! global? #t)))
		declared-globals)))

(define-nmethod (Scope.collect scope-table)
   (let* ((scope-table (make-scope-table)))
      (with-access::Scope this (locals-table)
	 (set! locals-table scope-table)
	 (default-walk this scope-table))))
   
;; some words for the Function:
;; arguments is created first.
;; can be shadowed by parameters.
;; if parameters share same name, last one wins.
;; If a local fun (function declaration) has the same var as any arg, then it
;; will overwrite the parameter inside the body. -> they don't need special
;; treatment.
;;
;; Scope-collect is hence completely sufficient, as long as Arguments are
;; visited before Parameters, and before vars.

(define-nmethod (Fun-Binding.collect scope-table)
   ;; remove the current association from the scope-table.
   ;; the fun-binding takes precedence.
   (scope-symbol-var-rm! scope-table (Ref-id (Fun-Binding-lhs this)))
   ;; during traversal the lhs will now set a new association.
   (default-walk this scope-table)
   (with-access::Fun-Binding this (lhs)
      (with-access::Ref lhs (var)
	 (with-access::Var var (fun?)
	    (set! fun? #t)))))

;; we know that a Named-fun has a function inside. -> easier than Catch.
(define-nmethod (Named-Fun.collect scope-table)
   (with-access::Named-Fun this (locals-table decl)
      (let* ((scope-table (make-scope-table)))
	 (set! locals-table scope-table)
	 (default-walk this scope-table))
      (with-access::Decl decl (var)
	 (with-access::Var var (named-fun? no-let?)
	    (set! named-fun? #t)
	    (set! no-let? #t)))))
   
(define-nmethod (Catch.collect scope-table)
   (with-access::Catch this (locals-table decl body)
      (let* ((catch-scope-table (make-scope-table)))
	 (set! locals-table catch-scope-table)
	 (walk decl catch-scope-table)
	 (with-access::Decl decl (var)
	    (with-access::Var var (no-let? catch?)
	       (set! no-let? #t)
	       (set! catch? #t)))
	 ;; the catch's body is only partially a scope.
	 ;; new variables are not inside the Catch.
	 (walk body scope-table))))

;; Params always have their proper vars
(define-nmethod (Param.collect scope-table)
   (let* ((id (Param-id this))
	  (var (instantiate::Var
		  (id id)
		  (param? #t)
		  (arguments? (Arguments-Decl? this)))))
      (scope-symbol-var-set! scope-table id var)))

;; This-param
(define-nmethod (This-Decl.collect scope-table)
   (with-access::This-Decl this (var)
      (set! var (instantiate::This-Var
		   (id 'this)))
      (scope-symbol-var-set! scope-table 'this var)))

;; all other decls reuse vars of previously declared vars of the same name.
;; function-declarations are not treated differently (they are only initialized
;; right in the beginning.)
;; Ex: fun f() { var x; ... var x; }  => both x share same var
;;     fun f(y, y) { var y; }  => the 'y's of the params are different (see
;;                                Param-collect), but the last param and var y;
;;                                share the same var.
;;     fun f() { var arguments; } => the implicite arguments and 'var
;;                                   arguments' share the same var.
(define-nmethod (Decl.collect scope-table)
   (with-access::Decl this (id var)
      (let ((resolved (scope-symbol-var scope-table id)))
	 (when (not resolved)
	    (set! var (instantiate::Var
			 (id id)))
	    (scope-symbol-var-set! scope-table id var)))))


;; =============
;; Resolve

(define-nmethod (Node.resolve! symbol-table implicit-proc)
   (default-walk! this symbol-table implicit-proc))

(define-nmethod (Program.resolve! ignored ignored)
   (with-access::Program this (runtime-table imported-table globals-table
					     implicit-globals)
      (let ((symbol-table (add-scope (add-scope (add-scope (make-symbol-table)
							   runtime-table)
						imported-table)
				     globals-table))
	    (implicit-proc (lambda (id decl)
			      (set! implicit-globals
				    (cons decl implicit-globals))
			      (scope-symbol-var-set! globals-table
						     id
						     (Decl-var decl)))))
	 (default-walk! this symbol-table implicit-proc)

	 (for-each (lambda (decl)
		      (with-access::Decl decl (var)
			 (with-access::Var var (global?)
			    (set! global? #t))))
		   implicit-globals)
	 this)))
					     
(define-nmethod (Scope.resolve! symbol-table implicit-proc)
   (with-access::Scope this (locals-table)
      (let ((extended-symbol-table (add-scope symbol-table locals-table)))
	 (default-walk! this extended-symbol-table implicit-proc))))

(define-nmethod (Named-Fun.resolve! symbol-table implicit-proc)
   (with-access::Named-Fun this (locals-table)
      (let ((extended-symbol-table (add-scope symbol-table locals-table)))
	 (default-walk! this extended-symbol-table implicit-proc))))

(define-nmethod (Catch.resolve! symbol-table implicit-proc)
   (with-access::Catch this (locals-table)
      (let ((extended-symbol-table (add-scope symbol-table locals-table)))
	 (default-walk! this extended-symbol-table implicit-proc))))

(define-nmethod (Decl.resolve! symbol-table implicit-proc)
   (with-access::Decl this (var id)
      (let ((table-var (symbol-var symbol-table id)))
	 (cond
	    ((and var  (Var-fun? var))
	     ;; never replace the vars of funs. they all have their proper ones.
	     this)
	    ((or (not var) ;; already during collect there was another var
		 (not (eq? var table-var))) ;; got shadowed by function var
	     (instantiate::Demoted-Decl
		(id id)
		(var table-var)))
	    (else ;; all set.
	     this)))))
   
(define-nmethod (Ref.resolve! symbol-table implicit-proc)
   (with-access::Ref this (var id)
      (let ((table-var (symbol-var symbol-table id)))
	 (cond
	    (table-var
	     (set! var table-var))
; 	     ((assq this.id *runtime-variables*)
; 	      =>
; 	      (lambda (binding)
; 		 (set! this.var (new-node Runtime-var
; 				     (car binding)
; 				     (cdr binding)))))
	    (else
	     ;; a new global...
	     (let* ((new-var (instantiate::Var
				(id id)))
		    (decl (instantiate::Decl
			     (id id)
			     (var new-var))))
		(unless (thread-parameter 'eval?)
		   (verbose "implicit global: " id))
		(implicit-proc id decl)
		(set! var new-var))))
	 this)))
