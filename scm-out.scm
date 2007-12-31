(module scm-out
   (include "protobject.sch")
   (include "nodes.sch")
   (option (loadq "protobject-eval.sch"))
   (import parser
	   config
	   protobject
	   nodes
	   var
	   label
	   verbose)
   (export (scm-out tree::pobject)))

(define (scm-out tree)
   (verbose "scm-out")
   (overload traverse out (Node
			   Program
			   Begin
			   Var-ref
			   NOP
			   If
			   While
			   Do
			   For-in
			   With
			   Switch
			   Fall-through
			   Case
			   Default
			   Throw
			   Try
			   Catch
			   Named-fun
			   Fun
			   Vassig
			   Fun-binding
			   Accsig
			   Call
			   Delete-call
			   Delete-property-call
			   Method-call
			   Eval-call
			   New
			   Access
			   This
			   Literal
			   Undefined
			   Null
			   Bool
			   Number
			   String
			   Array
			   Array-element
			   Obj-init
			   Property-init
			   Reg-exp
			   Let*
			   Bind-exit
			   Bind-exit-invoc)
	     (overload access access (Var
				      Intercepted-var
				      This-var
				      Imported-var)
	     (overload typeof typeof (Var
				      Intercepted-var
				      This-var
				      Imported-var)
             ;; set! must return the val (which is an expr!)
	     (overload set! set! (Var
				  Intercepted-var
				  This-var
				  Imported-var)
	     (overload delete delete (Var
				      Intercepted-var
				      This-var
				      Imported-var)
	     (overload access+base access+base (Var
						Intercepted-var
						This-var
						Imported-var)
	     (overload compiled-id compiled-id (Var
						Intercepted-var
						This-var
						Imported-var)
		       (overload traverse out (Label)
				 (tree.traverse))))))))))

(define (map-node-compile l)
   (map (lambda (n)
	   (n.traverse))
	l))

(define (get/assign-scm-id! var)
   (or var.scm-id
       (let ((id (gensym (symbol-append 'jsv- var.id))))
	  (set! var.scm-id id)
	  id)))
   
(define-pmethod (Var-access)
   (let ((scm-id (get/assign-scm-id! this)))
      (cond
	 ;; don't even try to do fancy stuff, when we are in an eval. just
	 ;; access the eval object
	 ((and (thread-parameter 'eval?)
	       this.global?)
	  `(env-get ,(thread-parameter 'eval-env)
		    ,(symbol->string this.id)))
	 (this.local-eval?
	  ;; stupid thing: evals might delete local variables. If there's a
	  ;; local eval, we need to verify first, if the variable actually
	  ;; still exists.
	  `(if (not (js-undeclared? ,scm-id))
	       ,scm-id
	       ,(this.eval-next-var.access)))
	 (this.global?
	  `(if (not (js-undeclared? ,scm-id))
	       ,scm-id
	       (undeclared-error ',this.id)))
	 (else
	  scm-id))))
(define-pmethod (Intercepted-var-access)
   (let* ((id this.id)
	  (id-str (symbol->string id))
	  (obj-id this.obj-id)
	  (intercepted this.intercepted))
      `(if (js-property-contains ,obj-id ,id-str)
	   (js-property-safe-get ,obj-id ,id-str)
	   ,(intercepted.access))))
(define-pmethod (This-var-access)
   'this)
(define-pmethod (Imported-var-access)
   (error "scm-out"
	  "Encountered imported var"
	  this.id))

(define-pmethod (Var-typeof)
   (let ((scm-id (get/assign-scm-id! this)))
      (cond
	 ;; don't even try to do fancy stuff, when we are in an eval. just
	 ;; access the eval object
	 ((and (thread-parameter 'eval?)
	       this.global?)
	  `(env-typeof-get ,(thread-parameter 'eval-env)
			   ,(symbol->string this.id)))
	 (this.local-eval?
	  ;; stupid thing: evals might delete local variables. If there's a
	  ;; local eval, we need to verify first, if the variable actually
	  ;; still exists.
	  `(if (not (js-undeclared? ,scm-id))
	       ,scm-id
	       ,(this.eval-next-var.typeof)))
	 (this.global? scm-id)
	 (else scm-id))))
(define-pmethod (Intercepted-var-typeof)
   (let* ((id this.id)
	  (id-str (symbol->string id))
	  (obj-id this.obj-id)
	  (intercepted this.intercepted))
      `(if (js-property-contains ,obj-id ,id-str)
	   (js-property-safe-get ,obj-id ,id-str)
	   ,(intercepted.typeof))))
(define-pmethod (This-var-typeof)
   'this)
(define-pmethod (Imported-var-typeof)
   (error "scm-out"
	  "Encountered imported var"
	  this.id))

(define-pmethod (Var-set! val)
   (let ((scm-id (get/assign-scm-id! this)))
      (cond
	 ;; don't even try to do fancy stuff when we are in an eval. just
	 ;; set! the id in the eval object
	 ((and (thread-parameter 'eval?)
	       this.global?)
	  `(env-set! ,(thread-parameter 'eval-env)
		     ,(symbol->string this.id)
		     ,val))
	 (this.local-eval?
	  ;; stupid thing: evals might delete local variables. If there's a
	  ;; local eval, we need to verify first, if the variable actually
	  ;; still exists.
	  `(if (not (js-undeclared? ,scm-id))
	       (begin (set! ,scm-id ,val) ,scm-id)
	       ,(this.eval-next-var.set! val)))
	 (this.global?
	  ;; Verified: no global runtime-variable is read-only.
	  ;; some properties inside global objects are, but not properties of
	  ;; the global this-object.
	  `(begin (set! ,scm-id ,val) ,scm-id))
	 (else
	  `(begin (set! ,scm-id ,val) ,scm-id)))))
(define-pmethod (Intercepted-var-set! val)
   (let* ((id this.id)
	  (id-str (symbol->string id))
	  (obj-id this.obj-id)
	  (intercepted this.intercepted))
      `(if (js-property-contains ,obj-id ,id-str)
	   ;; do not use generic-set! here
	   (js-property-update! ,obj-id ,id-str ,val)
	   ,(intercepted.set! val))))
(define-pmethod (This-var-set! val)
   (error "Var-set!"
	  "Can't assign to 'this'"
	  #f))
(define-pmethod (Imported-var-set! val)
   (error "scm-out"
	  "Encountered imported var"
	  this.id))

(define-pmethod (Var-delete)
   (let ((scm-id (get/assign-scm-id! this)))
      (cond
	 (this.global?
	  `(env-delete! ,(thread-parameter 'eval-env)
			,(symbol->string this.id)))
	 (this.local-eval?
	  `(if (not (js-undeclared? ,scm-id))
	       (js-property-delete! ,this.eval-obj-id
				    ,(symbol->string this.id))
	       ,(this.eval-next-var.delete)))
	 (else
	  #f))))
(define-pmethod (Intercepted-var-delete)
   (let* ((id this.id)
	  (id-str (symbol->string id))
	  (obj-id this.obj-id)
	  (intercepted this.intercepted))
      `(if (js-property-contains ,obj-id ,id-str)
	   (jsop-property-delete! ,obj-id ,id-str)
	   ,(intercepted.delete))))
(define-pmethod (This-var-delete)
   (error "Var-delete"
	  "Can't delete 'this'"
	  #f))
(define-pmethod (Imported-var-delete)
   (error "scm-out"
	  "Encountered imported var"
	  this.id))

;; slightly HACKish: if the referenced var is inside an object (different from
;; an activation object), then it updates (using 'set!') the given
;; base-variable.
(define-pmethod (Var-access+base base)
   ;; mostly similar to Var-access
   (let ((scm-id (get/assign-scm-id! this)))
      (cond
	 ;; don't even try to do fancy stuff, when we are in an eval. just
	 ;; access the eval object
	 ((and (thread-parameter 'eval?)
	       this.global?)
	  (let ((v (gensym 'v))
		(o (gensym 'o)))
	     `(multiple-value-bind (,v ,o)
		 (env-get+object ,(thread-parameter 'eval-env)
				 ,(symbol->string this.id))
		 (unless (Js-Activation-Object? ,o)
		    (set! ,base ,o))
		 ,v)))
	 (this.local-eval?
	  ;; stupid thing: evals might delete local variables. If there's a
	  ;; local eval, we need to verify first, if the variable actually
	  ;; still exists.
	  `(if (not (js-undeclared? ,scm-id))
	       ,scm-id
	       ,(this.eval-next-var.access+base base)))
	 (this.global?
	  `(if (not (js-undeclared? ,scm-id))
	       ,scm-id
	       (undeclared-error ',this.id)))
	 (else
	  scm-id))))
(define-pmethod (Intercepted-var-access+base base)
   (let* ((id this.id)
	  (id-str (symbol->string id))
	  (obj-id this.obj-id)
	  (intercepted this.intercepted))
      `(if (js-property-contains ,obj-id ,id-str)
	   (begin
	      (set! ,base ,obj-id)
	      (js-property-safe-get ,obj-id ,id-str))
	   ,(intercepted.access+base base))))
(define-pmethod (This-var-access+base base) 'this)
(define-pmethod (Imported-var-access+base base)
   (error "scm-out"
	  "Encountered imported var"
	  this.id))



(define-pmethod (Var-compiled-id)
   (get/assign-scm-id! this))
(define-pmethod (Intercepted-var-compiled-id)
   (error "scm-out"
	  "Intercepted-vars don't have any id. (internal error)"
	  this.id))
(define-pmethod (This-var-compiled-id)
   'this)
(define-pmethod (Imported-var-compiled-id)
   (error "scm-out"
	  "Encountered imported var"
	  this.id))

(define-pmethod (Label-out)
   (if (not this.generated)
       (set! this.generated (gensym 'label-id)))
   this.generated)

(define-pmethod (Node-out)
   (error "Node-out"
	  "forgot node type: "
	  (pobject-name this)))

(define-pmethod (Program-out)
   (let ((tlo (thread-parameter 'top-level-object))
	 (in-eval? (thread-parameter 'eval?))
	 (tl-this (thread-parameter 'top-level-this))
	 (declared-globals-w/o-this
	  (filter (lambda (var)
		     (not (inherits-from? var (node 'This-var))))
		  this.declared-globals)))
      (if (not in-eval?)
	  `(let ((this ,tl-this)
		 ,@(if (config 'function-strings)
		       (hashtable-map this.function-str-ids-ht
				      list)
		       '()))
	      ,@(map (lambda (var)
			`(define ,(var.compiled-id) (js-undefined)))
		     declared-globals-w/o-this)
	      ,@(map (lambda (decl)
			`(define ,(decl.var.compiled-id) (js-undeclared)))
		     this.implicit-globals)
	      ,@(map (lambda (var)
			`(global-declared-add! ',var.id ,(var.compiled-id)))
		     declared-globals-w/o-this)
	      ,@(map (lambda (decl)
			(let ((var decl.var))
			   `(global-implicit-add! ',var.id
						  ,(var.compiled-id))))
		     this.implicit-globals)
	      ,(this.body.traverse))
	  ;; eval does not do anything for implicit vars.
	  ;; declared globals: if the var is a declared function, than it
	  ;; must replace the original entry (including the attributes).
	  ;; otherwise just make sure there is an entry.
	  `(let ((this ,tl-this)
		 ,@(if (config 'function-strings)
		       (hashtable-map this.function-str-ids-ht
				      list)
		       '()))
	      ,@(map (lambda (var)
			(let ((id-str (symbol->string var.id)))
			   (if var.fun?
			       ;; declared fun.
			       ;; always replace existing entry.
			       ;; initially set to undefined, but that
			       ;; changes soon.
			       ;; we need to use generic-set, otherwise we
			       ;; can't overwrite existing entries, if they
			       ;; have different attributes.
			       `(js-property-generic-set! ,tlo ,id-str
							  (js-undefined)
							  (default-attributes))
			       `(unless (js-scope-one-level-property-contains?
					 ,tlo ,id-str)
				   (js-property-safe-set! ,tlo
							  ,id-str
							  (js-undefined))))))
		     declared-globals-w/o-this)
	      ,(this.body.traverse)))))

(define-pmethod (Begin-out)
   `(begin
       ,@(map-node-compile this.els)))

(define-pmethod (Let*-out)
   `(let* (,@(map (lambda (vassig)
		     `(,(vassig.lhs.var.compiled-id) ,(vassig.val.traverse)))
		  this.vassigs))
       ,(this.body.traverse)))

(define-pmethod (Var-ref-out)
   (this.var.access))

(define-pmethod (NOP-out)
   '(js-undefined))

(define-pmethod (If-out)
   `(if (js-boolify ,(this.test.traverse))
	,(this.then.traverse)
	,(this.else.traverse)))

(define-pmethod (While-out)
   (let ((loop (gensym 'loop)))
      `(let ,loop ()
	    (if (js-boolify ,(this.test.traverse))
		(begin
		   ,(this.body.traverse)
		   (,loop))))))

(define-pmethod (Do-out)
   (let ((loop (gensym 'loop)))
      `(let ,loop (())
	    ,(this.body.traverse)
	    (if (js-boolify ,(this.test.traverse))
		(,loop)))))

(define-pmethod (For-in-out)
   `(for-each (lambda (,(this.lhs.var.compiled-id)) ,(this.body.traverse))
	      (js-for-in-properties-list (any->object ,(this.obj.traverse)))))

(define-pmethod (With-out)
   ;; the obj is not yet transformed to an object.
   `(let ((,this.obj-id (any->object ,(this.obj.traverse))))
       ,(this.body.traverse)))

(define (make-clause default-clause? body-id expr body)
   (list default-clause? body-id expr body))

(define clause-default-clause? car)
(define clause-body-id cadr)
(define clause-expr caddr) ;; ignored in default-clause
(define clause-body cadddr)

(define-pmethod (Switch-out)
   (define (rev-bind-clauses compiled-clauses)
      (let loop ((clauses compiled-clauses)
		 (rev-result '()))
	 (cond
	    ((null? clauses) ;; should not happen
	     '())
	    ((null? (cdr clauses))
	     (cons (list (clause-body-id (car clauses))
			 `(lambda ()
			     ,(clause-body (car clauses))))
		   rev-result))
	    (else
	     (loop (cdr clauses)
		   (cons (list (clause-body-id (car clauses))
			       `(lambda ()
				   (let ((fall-through ,(clause-body-id (cadr clauses))))
				      ,(clause-body (car clauses)))))
			 rev-result))))))
   
   (let* ((key (gensym 'key))
	  (compiled-key (this.key.traverse))
	  (compiled-clauses (map-node-compile this.cases))
	  (default-body-id (let loop ((clauses compiled-clauses))
			      (cond
				 ((null? clauses) #f)
				 ((clause-default-clause? (car clauses))
				  (clause-body-id (car clauses)))
				 (else
				  (loop (cdr clauses))))))
	  (rev-clauses-bindings (rev-bind-clauses compiled-clauses)))
      `(let* (,@(cons (list key compiled-key)
		      rev-clauses-bindings))
	  (cond
	     ,@(map (lambda (clause)
		       (if (clause-default-clause? clause)
			   `(#f 'default-clause-was-here)
			   `((jsop-=== ,(clause-expr clause) ,key)
			     (,(clause-body-id clause)))))
		    compiled-clauses)
	     ,@(if default-body-id
		   `((else (,default-body-id)))
		   '())))))

(define-pmethod (Fall-through-out)
   '(fall-through))

(define-pmethod (Case-out)
   (make-clause #f
		(gensym 'clause-body)
		(this.expr.traverse)
		(this.body.traverse)))

(define-pmethod (Default-out)
   (make-clause #t
		(gensym 'default-body)
		'ignored
		(this.body.traverse)))

(define-pmethod (Throw-out)
   `(raise ,(this.expr.traverse)))

(define-pmethod (Try-out)
   ;; trampoline-style
   ;; with-handlers are executed before the surrounding unwind-protects.
   ;; -> wrap the result into a lambda.
   ;;    same goes for catch-clause.
   ;; the catch-clause is then executed after the body and all unwind-protects.
   (let* ((tmp (gensym 'tmp))
	  (inner (if this.catch
		     `((with-handler
			  ;; catch must return a trampoline
			  ,(this.catch.traverse)
			  (let ((,tmp ,(this.body.traverse)))
			     (lambda ()
				,tmp))))
		     (this.body.traverse))))
      (if this.finally
	  `(unwind-protect
	      ,inner
	      ,(this.finally.traverse))
	  inner)))

(define-pmethod (Catch-out)
   (let ((exc-scm-id (this.decl.var.compiled-id))
	 (exc-js-id this.decl.var.id)
	 (obj-id this.obj-id)
	 (compiled-body (this.body.traverse)))
      ;; the exc-scm-id is assigned in the Try-clause.
      `(lambda (,exc-scm-id)
	  (lambda ()
	     (let* ((,exc-scm-id (error->js-exception ,exc-scm-id))
		    (,obj-id (js-create-scope-object (js-object-literal '()))))
		(scope-var-add ,obj-id
			       ,(symbol->string exc-js-id)
			       ,exc-scm-id
			       (dont-delete-attributes))
		,compiled-body)))))

(define-pmethod (Named-fun-out)
   (let ((fun-scm-id (this.decl.var.compiled-id))
	 (fun-js-id this.decl.var.id)
	 (obj-id this.obj-id)
	 (compiled-body (this.body.traverse)))
      `(let ((,obj-id (js-create-scope-object (js-object-literal '()))))
	  (letrec ((,fun-scm-id ,compiled-body))
	     (scope-var-add ,obj-id
			    ,(symbol->string fun-js-id)
			    ,fun-scm-id
			    (dont-delete-attributes))
	     ,fun-scm-id))))

(define-pmethod (Bind-exit-out)
   `(bind-exit (,(this.label.traverse))
       ,(this.body.traverse)))

(define-pmethod (Bind-exit-invoc-out)
   `(,(this.label.traverse) ,(this.expr.traverse)))

(define *nb-named-params* 3)
(define-pmethod (Fun-out)
   (define (add-eval-object body)
      (let* ((fun-vars (filter! (lambda (var)
				   (not (inherits-from? var
							(node 'This-var))))
				(hashtable->list this.locals-table)))
	     (local-vars (filter (lambda (var) (not var.param?))
				 fun-vars))
	     (eval-obj-id this.eval-obj-id))
	 `(let ((,eval-obj-id (js-create-activation-object))
		,@(map (lambda (var)
			  `(,(var.compiled-id) (js-undefined)))
		       local-vars))
	     ,@(map (lambda (var)
		       `(scope-var-add ,eval-obj-id
				       ,(symbol->string var.id)
				       ,(var.compiled-id)
				       (dont-delete-attributes)))
		    fun-vars)
	     ,body)))
	    

   ;; when adding conditional for arguments-var don't forget, that arguments
   ;; var might be added into eval-obj.
   ;; in theory only for function with local-eval, but as this one can be
   ;; deleted, the next one needs to expose its arguments object too. As we
   ;; have (not yet) any means to detect that, we have to add the arguments
   ;; object for all functions with '.eval?'.
   (let ((compiled-arguments (if this.arguments-decl.var.arguments-used?
				 (this.arguments-decl.var.compiled-id)
				 #f))
	 (compiled-this (this.this-decl.var.compiled-id))
	 (compiled-params (map (lambda (decl) (decl.var.compiled-id))
			       this.params))
	 (compiled-body (if this.eval?
			    (add-eval-object (this.body.traverse))
			    (this.body.traverse))))
      `(js-fun ,compiled-this
	       #f ;; no this-fun (only accessible through arguments)
	       ,compiled-arguments
	       ,(if (config 'function-strings)
		    `(list ,@this.str)
		    (symbol->string (gensym "function")))
	       (,@compiled-params)
	       ,compiled-body)))

(define-pmethod (Vassig-out)
   (this.lhs.var.set! (this.val.traverse)))

(define-pmethod (Fun-binding-out)
   (if (and (thread-parameter 'eval?)
	    this.lhs.var.global?)
       ;; assign fun-binding to top-level object and not to current
       ;; environment. (Rhino 1.7 release 1 Pre 2007 11 25 has this bug)
       `(js-property-safe-set! ,(thread-parameter 'top-level-object)
			       ,(symbol->string this.lhs.var.id)
			       ,(this.val.traverse))
       (pcall this Vassig-out)))

(define-pmethod (Accsig-out)
   ;; gcc seems to be broken (quadratic on the number of variables?)
   ;; special case to avoid temporaries
   (let ((tmp-o (gensym 'tmp-o))
	 (tmp-field (gensym 'tmp-field))
	 (tmp-object-o (gensym 'tmp-object-o))
	 (tmp-string-field (gensym 'tmp-string-field))
	 (traversed-obj (this.lhs.obj.traverse))
	 (traversed-field (this.lhs.field.traverse))
	 (traversed-val (this.val.traverse)))
      (if (and (symbol? traversed-obj)
	       (string? traversed-field))
	  `(let* ((,tmp-object-o (any->object ,traversed-obj)))
	      (js-property-safe-set! ,tmp-object-o
				     ,traversed-field
				     ,traversed-val))
	  ;; we need all these tmp-variables, to ensure the correct order of
	  ;; evaluation.
	  `(let* ((,tmp-o ,traversed-obj)
		  (,tmp-field ,traversed-field)
		  (,tmp-object-o (any->object ,tmp-o))
		  (,tmp-string-field (any->string ,tmp-field)))
	      (js-property-safe-set! ,tmp-object-o
				     ,tmp-string-field
				     ,(this.val.traverse))))))

(define (Operator-out this)
   (cond
      ((and (eq? this.op.var.id 'typeof)
	    (not (null? this.args))
	    (null? (cdr this.args))
	    (inherits-from? (car this.args) (node 'Var-ref)))
       ;; we need to use .typeof to avoid undeclared error
       `(,(this.op.var.compiled-id) ,((car this.args).var.typeof)))
      ((or (eq? this.op.var.id '&&)
	   (eq? this.op.var.id 'OR))
       ;; not really operator calls, but macros.
       ;; do not add the 'let'.
       `(,(this.op.var.compiled-id) ;; operator call.
	 ,@(map-node-compile this.args)))
      (else
       (let* ((compiled-args (map-node-compile this.args))
	      (tmp-ids (map (lambda (arg) (gensym 'tmp))
			    compiled-args))
	      (bindings (map (lambda (tmp-id arg)
				(list tmp-id arg))
			     tmp-ids
			     compiled-args)))
	  `(let* ,bindings
	      (,(this.op.var.compiled-id) ;; operator call.
	       ,@tmp-ids))))))
   
(define-pmethod (Call-out)
   (cond
      ((and (inherits-from? this.op (node 'Var-ref))
	    (inherits-from? this.op.var (node 'Runtime-var))
	    this.op.var.operator?)
       (Operator-out this))
      ((inherits-from? this.op (node 'Var-ref))
       (let ((base (gensym 'base))
	     (f (gensym 'f)))
	  `(let ((,base *js-global-this*))
	      ;; if the variable has a base, it will update the base-variable
	      ;; we then perform (implicitely) a method-call.
	      (let ((,f ,(this.op.var.access+base base)))
		 (js-call ,f
			  ,base ;; might have been updated by access+base.
			  ,@(map-node-compile this.args))))))
      (else
       `(js-call ,(this.op.traverse)
		 #f
		 ,@(map-node-compile this.args)))))

; (define-pmethod (Binary-out)
;    `(,(this.op.traverse)
;      ,@(map-node-compile this.args)))

(define-pmethod (Delete-call-out)
   ((car this.args).var.delete))

(define-pmethod (Delete-property-call-out)
   `(jsop-property-delete! ,((car this.args).traverse)
			   ,((cadr this.args).traverse)))

(define-pmethod (Method-call-out)
   `(js-method-call ,(this.op.obj.traverse)
		    ,(this.op.field.traverse)
		    ,@(map-node-compile this.args)))

(define-pmethod (Eval-call-out)
   (let* ((t (gensym 'tmp))
	  (eval-id this.eval-scm-id)
	  (tlo this.top-level-object)
	  (tlo-id/obj (if (inherits-from? tlo (node 'Var))
			  (tlo.compiled-id)
			  tlo))
	  (next-env (thread-parameter 'eval-env))
	  (env-vars this.env-vars))
      `(let ((,t ,(this.op.traverse)))
	  (if (eq? ,t ,eval-id)
	      ;; we have a real eval here
	      (js-eval ,((car this.args).traverse)
		       ,tlo-id/obj this ,next-env
		       ,@env-vars)
	      (js-call ,t #f ,@(map-node-compile this.args))))))

(define-pmethod (New-out)
   `(js-new ,(this.class.traverse)
	    ,@(map-node-compile this.args)))

(define-pmethod (Access-out)
   (let ((tmp-o (gensym 'tmp-o))
	 (tmp-field (gensym 'tmp-field))
	 (tmp-object-o (gensym 'tmp-object-o))
	 (tmp-string-field (gensym 'tmp-string-field)))
      ;; we need all these tmp-variables, to ensure the correct order of
      ;; evaluation.
      `(let* ((,tmp-o ,(this.obj.traverse))
	      (,tmp-field ,(this.field.traverse))
	      (,tmp-object-o (any->object ,tmp-o))
	      (,tmp-string-field (any->string ,tmp-field)))
	  (js-property-safe-get ,tmp-object-o ,tmp-string-field))))

(define-pmethod (This-out)
   'this)

(define-pmethod (Literal-out)
   (error "Literal-out" "forgot literal type" this.val))

(define-pmethod (Undefined-out)
   '(js-undefined))

(define-pmethod (Null-out)
   '(js-null))

(define-pmethod (Bool-out)
   (if this.val #t #f))

(define-pmethod (Number-out)
   (let* ((str this.val)
	  (hex? (or (string-prefix? "0x" str)
		    (string-prefix? "0X" str)))
	  (nb (if hex?
		  (llong->flonum
		   (string->llong
		    (substring str 2 (string-length str)) 16))
		  (string->real str))))
      (cond
	 ((real? nb)
	  nb)
	 (else
	  (error "Number-out"
		 "could not transform to number:"
		 str)))))

(define-pmethod (String-out)
   ;; TODO: fix strings. (escaping always correct?)
   this.val)

(define-pmethod (Array-out)
   `(js-array-literal
     ,this.length
     ,(list 'quasiquote (map-node-compile this.els))))

(define-pmethod (Array-element-out)
   (list this.index
	 (list 'unquote (this.expr.traverse))))

(define-pmethod (Obj-init-out)
   `(js-object-literal
     ,(list 'quasiquote (map-node-compile this.props))))

(define-pmethod (Property-init-out)
   `(,(this.name.traverse) ,(list 'unquote (this.val.traverse))))

(define-pmethod (Reg-exp-out)
   ''(TODO TODO TODO reg-exp TODO TODO TODO))
