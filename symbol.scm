(module symbol
   (include "protobject.sch")
   (include "nodes.sch")
   (include "js-runtime/runtime-variables.sch")
   (option (loadq "protobject-eval.sch"))
   (import protobject
	   verbose
	   nodes
	   var)
   (export (symbol-resolution! tree::pobject)
	   *imported-vars*
	   (id->runtime-var id::symbol)))

(define *imported-vars* '())

(define (id->runtime-var id)
   (hashtable-get *runtime-table* id))
;; will be set later
(define *runtime-table* #unspecified)

(define (symbol-resolution! tree)
   (verbose "symbol-resolution")
   (collect! tree)
   (resolve tree))


(define (make-symbol-table) '())
(define (make-scope-table) (make-hashtable))

(define (add-scope symbol-table scope-table)
   (cons scope-table symbol-table))

(define (symbol-var symbol-table symbol)
   (any (lambda (ht)
	   (hashtable-get ht symbol))
	symbol-table))

(define (local-symbol-var symbol-table symbol)
   (and (pair? symbol-table)
	(hashtable-get (car symbol-table) symbol)))

(define (symbol-var-set! symbol-table symbol var)
   (scope-symbol-var-set! (car symbol-table) symbol var))

(define (scope-symbol-var-set! scope symbol var)
   (hashtable-put! scope symbol var))

(define (scope-symbol-var scope symbol)
   (hashtable-get scope symbol))

;; See 10.1.3 of Ecmascript spec.
;; Basically:
;; each param gets a var (even those with same name). If 2 params have same
;; name, last param wins.
;; Every 'var' statement declares var. If one already exists (for instance
;; param, or other 'var') nothing happens.
;; Function declarations win over 'var'-statements. This is only important when
;; initializing (setting local vars to 'undefined')
(define (collect! tree)
   (verbose "collect!")
   (overload traverse! collect! (Node
				 Program
				 Scope
				 Catch
				 Param
				 This-decl
				 Decl)
	     (tree.traverse! #f)))


(define-pmethod (Node-collect! scope-table)
   (this.traverse1! scope-table))

(define-pmethod (Program-collect! scope-table)
   (let* ((runtime-table (make-scope-table))
	  (imported-table (make-scope-table))
	  (globals-table (make-scope-table)))
      (for-each (lambda (runtime-var-binding)
		   (let* ((id (car runtime-var-binding))
			  (scm-id (cadr runtime-var-binding))
			  (v (new Runtime-var id scm-id)))
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
			  (v (new Imported-var id scm-id)))
		      (set! v.global? #t)
		      (hashtable-put! imported-table
				      id
				      v)))
		*imported-vars*)

      (set! *runtime-table* runtime-table)
      (set! this.imported-table imported-table)
      (set! this.runtime-table runtime-table)
      (set! this.globals globals-table)
      (this.traverse1! globals-table)
      (hashtable-for-each globals-table
			  (lambda (var-id var)
			     (set! var.global? #t))))
   this)

(define-pmethod (Scope-collect! scope-table)
   (let* ((scope-table (make-scope-table)))
      (set! this.locals scope-table)
      (this.traverse1! scope-table)))
   

;; some words for the Function:
;; it is important, that the arguments are parsed first and in order. This way
;; the last argument takes precedence over previous same-named arguments. If a
;; local fun has the same var, the arg is going to be shadowed by the fun-decl.
;; all these properties are met by a default traverse, and the Scope-collect
;; is hence sufficient.

(define-pmethod (Catch-collect! scope-table)
   (let* ((catch-scope-table (make-scope-table)))
      (set! this.locals catch-scope-table)
      (set! this.expection (this.exception.traverse! catch-scope-table))
      ;; the catch's body is not a scope.
      (set! this.body (this.body.traverse! scope-table))
      this))

;; Params always have their proper vars (for the 'arguments-object')
(define-pmethod (Param-collect! scope-table)
   (let* ((id this.id)
	  (var (new Var id)))
      (set! this.var var)
      (scope-symbol-var-set! scope-table id var))
   this)

;; This-param
(define-pmethod (This-decl-collect! scope-table)
   (let* ((var (new This-var)))
      (set! this.var var)
      (scope-symbol-var-set! scope-table 'this var))
   this)

;; all other decls reuse vars of previously declared vars of the same name.
;; function-declarations are not treated differently (they are only initialized
;; right in the beginning.)
;; Ex: fun f() { var x; ... var x; }  => both x share same var
;;     fun f(y, y) { var y; }  => the 'y's of the params are different (see
;;                                Param-collect), but the last param and var y;
;;                                share the same var.
;;     fun f() { var arguments; } => the implicite arguments and 'var
;;                                   arguments' share the same var.
(define-pmethod (Decl-collect! scope-table)
   (let* ((id this.id)
	  (var (scope-symbol-var scope-table id)))
      (if var
	  ;; there exists already a decl. -> remove this one.
	  (let ((ref (new Var-ref this.id)))
	     (set! ref.var var)
	     ref)
	  (let ((new-var (new Var id)))
	     (set! this.var new-var)
	     (scope-symbol-var-set! scope-table id new-var)
	     this))))

(define (resolve tree)
   (verbose " resolve")
   (overload traverse resolve (Node
			       Program
			       Scope
			       Catch
			       Var-ref)
	     (tree.traverse #f)))

(define *program* #f)

(define-pmethod (Node-resolve symbol-table)
   (this.traverse1 symbol-table))

(define-pmethod (Program-resolve symbol-table)
   (let ((symbol-table (add-scope (add-scope (add-scope (make-symbol-table)
							this.runtime-table)
					     this.imported-table)
				  this.globals)))
      (set! *program* this)
      (this.traverse1 symbol-table)
      (for-each (lambda (decl)
		   (set! decl.var.global? #t))
		this.implicit-globals)))
					     
(define-pmethod (Scope-resolve symbol-table)
   (let ((extended-symbol-table (add-scope symbol-table this.locals)))
      (this.traverse1 extended-symbol-table)))

(define-pmethod (Catch-resolve symbol-table)
   (let ((extended-symbol-table (add-scope symbol-table this.locals)))
      (this.traverse1 extended-symbol-table)))

(define-pmethod (Var-ref-resolve symbol-table)
   (unless this.var ;; already has var
       (let ((var (symbol-var symbol-table this.id)))
	  (cond
	     (var
	      (set! this.var var))
; 	     ((assq this.id *runtime-variables*)
; 	      =>
; 	      (lambda (binding)
; 		 (set! this.var (new Runtime-var
; 				     (car binding)
; 				     (cdr binding)))))
	     (else
	      ;; a new global...
	      (let* ((id this.id)
		     (prog *program*)
		     (decl (Decl-of-new-Var id)))
		 (verbose "implicit global: " id)
		 (set! prog.implicit-globals (cons decl prog.implicit-globals))
		 (scope-symbol-var-set! prog.globals id decl.var)
		 (set! decl.var.implicit-global? #t)
		 (set! this.var decl.var)))))))

;; for now another pass.
;;   whenever we encounter a 'with', we "clear" the current
;; symbol-table. Whenever we encounter a var-ref that has no var in the
;; symbol-table, we know it has been intercepted by a with.
(define (with-interception tree)
   (verbose " with-interception")
   (overload traverse with-interception (Node
					 Program
					 Scope
					 With
					 Var-ref)
	     (tree.traverse #f '())))

;; surrounding-withs is a list of 'With's with their symbol-table. (ie. the
;; symbol-table when the 'With' was encountered.
(define-pmethod (Node-with-interception symbol-table surrounding-withs)
   (this.traverse2 symbol-table surrounding-withs))

(define-pmethod (Program-with-interception symbol-table surrounding-withs)
   (let ((symbol-table (add-scope (add-scope (add-scope (make-symbol-table)
							this.runtime)
					     this.imported)
				  this.locals)))
      (this.traverse2 symbol-table '())))

(define-pmethod (Scope-with-interception symbol-table surrounding-withs)
   (let ((extended-symbol-table (add-scope symbol-table this.locals)))
      (this.traverse2 extended-symbol-table surrounding-withs)))

(define-pmethod (With-with-interception symbol-table surrounding-withs)
   (set! this.intercepted-ht (make-hashtable))
   (this.traverse2 (make-symbol-table) (cons (cons this symbol-table)
					     surrounding-withs))
   ;; store the 'fake-var'-declarations.
   (set! this.intercepted (hashtable->list this.intercepted-ht))
   (for-each (lambda (var)
		(set! var.intercepted? #t))
	     this.intercepted)
   (delete! this.intercepted-ht))


;; note: the id is reduntant...
(define (update-var id var symbol-table surrounding-withs)
   (let ((var (symbol-var symbol-table id)))
      (if var
	  ;; not intercepted
	  var
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
		 (let* ((decl (new Decl id))
			(fake-var (new With-var
				       id
				       surrounding-with
				       ;; continue recursively
				       (update-var id
						   var
						   (cdar surrounding-withs)
						   (cdr surrounding-withs)))))
		   (set! decl.var fake-var)
		   (hashtable-put! intercepted-ht id fake-var)
		   fake-var))))))

(define-pmethod (Var-ref-with-interception symbol-table surrounding-withs)
   (set! this.var (update-var this.id this.var symbol-table surrounding-withs)))
