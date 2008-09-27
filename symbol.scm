(module symbol
   (include "protobject.sch")
   (include "nodes.sch")
   (include "js-runtime/runtime-variables.sch")
   (option (loadq "protobject-eval.sch"))
   (import protobject
	   symbol-table
	   verbose
	   nodes
	   var)
   (export (symbol-resolution! tree::pobject imported-ids::pair-nil)
	   (id->runtime-var id::symbol)))

(define (id->runtime-var id)
   (hashtable-get (thread-parameter '*runtime-table*) id))
(define (runtime-table-set! table)
   (thread-parameter-set! '*runtime-table* table))

(define (symbol-resolution! tree imported-ids)
   (verbose "symbol-resolution")
   (collect tree imported-ids)
   (resolve! tree))

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
(define (collect tree imported-ids)
   (verbose "collect")
   (overload traverse collect (Node
			       Program
			       Scope
			       Fun-binding
			       Named-fun
			       Catch
			       Param
			       This-decl
			       Decl)
	     (tree.traverse #f imported-ids)))


(define-pmethod (Node-collect scope-table)
   (this.traverse1 scope-table))

(define-pmethod (Program-collect scope-table imported-ids)
   (let* ((runtime-table (make-scope-table))
	  (imported-table (make-scope-table))
	  (globals-table (make-scope-table)))
      (for-each (lambda (runtime-var-binding)
		   (let* ((id (car runtime-var-binding))
			  (scm-id (cadr runtime-var-binding))
			  (v (new-node Runtime-var id scm-id)))
		      (if (and (not (null? (cddr runtime-var-binding)))
			       (eq? (caddr runtime-var-binding) 'operator))
			  (set! v.operator? #t))
		      (set! v.global? #t)
		      (hashtable-put! runtime-table
				      id
				      v)))
		*runtime-variables*)
      (for-each (lambda (imported-var-binding)
		   (let* ((id (car imported-var-binding))
			  (scm-id (cadr imported-var-binding))
			  (v (new-node Imported-var id scm-id)))
		      (set! v.global? #t)
		      (hashtable-put! imported-table
				      id
				      v)))
		imported-ids)

      (runtime-table-set! runtime-table)
      (set! this.imported-table imported-table)
      (set! this.runtime-table runtime-table)
      (set! this.globals-table globals-table)
      (this.traverse1 globals-table)
      (set! this.declared-globals (hashtable->list globals-table))
      (for-each (lambda (var)
		   (set! var.global? #t)
		   (set! var.declared-global? #t))
		this.declared-globals)))

(define-pmethod (Scope-collect scope-table)
   (let* ((scope-table (make-scope-table)))
      (set! this.locals-table scope-table)
      (this.traverse1 scope-table)))
   
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

(define-pmethod (Fun-binding-collect scope-table)
   ;; remove the current association from the scope-table.
   ;; the fun-binding takes precedence.
   (scope-symbol-var-rm! scope-table this.lhs.id)
   ;; during traversal the lhs will now set a new association.
   (this.traverse1 scope-table)
   (set! this.lhs.var.fun? #t))

;; we know that a Named-fun has a function inside. -> easier than Catch.
(define-pmethod (Named-fun-collect scope-table)
   (pcall this Scope-collect scope-table)
   (set! this.decl.var.named-fun? #t)
   (set! this.decl.var.no-let? #t))
   
(define-pmethod (Catch-collect scope-table)
   (let* ((catch-scope-table (make-scope-table)))
      (set! this.locals-table catch-scope-table)
      (this.decl.traverse catch-scope-table)
      (set! this.decl.var.no-let? #t)
      (set! this.decl.var.catch? #t)
      ;; the catch's body is only partially a scope.
      ;; new variables are not inside the Catch.
      (this.body.traverse scope-table)))

;; Params always have their proper vars
(define-pmethod (Param-collect scope-table)
   (let* ((id this.id)
	  (var (new-node Var id)))
      (set! this.var var)
      (set! var.param? #t)
      (when (inherits-from? this (node 'Arguments-decl))
	 (set! var.arguments? #t))
      (scope-symbol-var-set! scope-table id var)))

;; This-param
(define-pmethod (This-decl-collect scope-table)
   (let* ((var (new-node This-var)))
      (set! this.var var)
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
(define-pmethod (Decl-collect scope-table)
   (let* ((id this.id)
	  (var (scope-symbol-var scope-table id)))
      (unless var
	 (let ((new-var (new-node Var id)))
	    (set! this.var new-var)
	    (set! new-var.declared? #t)
	    (scope-symbol-var-set! scope-table id new-var)))))

(define (resolve! tree)
   (verbose " resolve!")
   (overload traverse! resolve! (Node
				 Program
				 Scope
				 Named-fun
				 Catch
				 Decl
				 Var-ref)
	     (tree.traverse! #f #f)))

(define-pmethod (Node-resolve! symbol-table implicit-proc)
   (this.traverse2! symbol-table implicit-proc))

(define-pmethod (Program-resolve! symbol-table ignored)
   (let ((symbol-table (add-scope (add-scope (add-scope (make-symbol-table)
							this.runtime-table)
					     this.imported-table)
				  this.globals-table))
	 (implicit-proc (lambda (id decl)
			   (set! this.implicit-globals
				 (cons decl this.implicit-globals))
			   (scope-symbol-var-set! this.globals-table
						  id
						  decl.var)
			   (set! decl.var.global? #t)
			   (set! decl.var.implicit-global? #t))))
      (this.traverse2! symbol-table implicit-proc)

      (for-each (lambda (decl)
		   (set! decl.var.global? #t))
		this.implicit-globals)
      this))
					     
(define-pmethod (Scope-resolve! symbol-table implicit-proc)
   (let ((extended-symbol-table (add-scope symbol-table this.locals-table)))
      (this.traverse2! extended-symbol-table implicit-proc)))

(define-pmethod (Named-fun-resolve! symbol-table implicit-proc)
   (let ((extended-symbol-table (add-scope symbol-table this.locals-table)))
      (this.traverse2! extended-symbol-table implicit-proc)))

(define-pmethod (Catch-resolve! symbol-table implicit-proc)
   (let ((extended-symbol-table (add-scope symbol-table this.locals-table)))
      (this.traverse2! extended-symbol-table implicit-proc)))

(define-pmethod (Decl-resolve! symbol-table implicit-proc)
   (let ((decl-var this.var)
	 (table-var (symbol-var symbol-table this.id)))
      (cond
	 ((and decl-var
	       decl-var.fun?)
	  ;; never replace the vars of funs. they all have their proper ones.
	  this)
	 ((or (not decl-var) ;; already during collect there was another var
	      (not (eq? decl-var table-var))) ;; got shadowed by function var
	  (let ((ref (new-node Var-ref this.id)))
	     (set! ref.var table-var)
	     (set! ref.was-decl? #t)
	     ref))
	 (else ;; all set.
	  this))))
   
(define-pmethod (Var-ref-resolve! symbol-table implicit-proc)
   (let ((var (symbol-var symbol-table this.id)))
      (cond
	 (var
	  (set! this.var var))
; 	     ((assq this.id *runtime-variables*)
; 	      =>
; 	      (lambda (binding)
; 		 (set! this.var (new-node Runtime-var
; 				     (car binding)
; 				     (cdr binding)))))
	 (else
	  ;; a new global...
	  (let* ((id this.id)
		 (var (new-node Var id))
		 (decl (new-node Decl id)))
	     (set! decl.var var)
	     (unless (thread-parameter 'eval?)
		(verbose "implicit global: " id))
	     (implicit-proc id decl)
	     (set! this.var var))))
      this))
