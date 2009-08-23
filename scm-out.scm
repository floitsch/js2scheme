(module scm-out
   (library js2scheme-runtime)
   (library utf)
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

;; not thread-safe: we have a global variable here.
(define *strs* '())
(define (strs-reset!) (set! *strs* '()))
(define (strs-alist) *strs*)
(define (add-str! str)
   (let ((t (assoc str *strs*)))
      (if t
	  (cdr t)
	  (let ((str-id (symbol-append 'jsstr- (gensym))))
	     (set! *strs* (cons (cons str str-id) *strs*))
	     str-id))))
(define (string-var? var::symbol)
   (string-prefix? "jsstr-" (symbol->string var)))

(define (comp:any->js-string any)
   (cond
      ((and (symbol? any) (string-var? any))
       any)
      ((flonum? any)
       (add-str! (js-string->utf8 (any->js-string any))))
      ((eq? any '(js-undefined))
       (add-str! "undefined"))
      ((equal? any '(js-null))
       (add-str! "null"))
      ((boolean? any)
       (add-str! (if any "true" "false")))
      (else
       #f)))

(define (comp:any->js-object any)
   (if (and (symbol? any)
	    (eq? any 'this))
       'this
       #f))

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
	  (let ((str-id (add-str! (symbol->string this.id))))
	     `(env-get ,(thread-parameter 'eval-env) ,str-id)))
	 (this.local-eval?
	  ;; stupid thing: evals might delete local variables. If there's a
	  ;; local eval, we need to verify first, if the variable actually
	  ;; still exists.
	  `(if (not (js-deleted? ,scm-id))
	       ,scm-id
	       ,(this.eval-next-var.access)))
	 (this.global?
	  `(global-read ,scm-id))
	 (else
	  scm-id))))
(define-pmethod (Intercepted-var-access)
   (let ((str-id (add-str! (symbol->string this.id)))
	 (obj-id this.obj-id)
	 (intercepted this.intercepted)
	 (tmp (gensym 'tmp)))
      `(let ((,tmp (js-property-contains ,obj-id ,str-id)))
	  (if ,tmp
	      (unmangle-false ,tmp)
	      ,(intercepted.access)))))
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
	  (let ((str-id (add-str! (symbol->string this.id))))
	     `(env-typeof-get ,(thread-parameter 'eval-env) ,str-id)))
	 (this.local-eval?
	  ;; stupid thing: evals might delete local variables. If there's a
	  ;; local eval, we need to verify first, if the variable actually
	  ;; still exists.
	  `(if (not (js-deleted? ,scm-id))
	       ,scm-id
	       ,(this.eval-next-var.typeof)))
	 (this.global? `(global-typeof-read ,scm-id))
	 (else scm-id))))
(define-pmethod (Intercepted-var-typeof)
   (let ((str-id (add-str! (symbol->string this.id)))
	 (obj-id this.obj-id)
	 (intercepted this.intercepted)
	 (tmp (gensym 'tmp)))
      `(let ((,tmp (js-property-contains ,obj-id ,str-id)))
	  (if ,tmp
	      (unmange-false ,tmp)
	      ,(intercepted.typeof)))))
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
	  (let ((str-id (add-str! (symbol->string this.id))))
	     `(env-set! ,(thread-parameter 'eval-env) ,str-id ,val)))
	 (this.local-eval?
	  ;; stupid thing: evals might delete local variables. If there's a
	  ;; local eval, we need to verify first, if the variable actually
	  ;; still exists.
	  `(if (not (js-deleted? ,scm-id))
	       (begin (set! ,scm-id ,val) ,scm-id)
	       ,(this.eval-next-var.set! val)))
	 (this.global?
	  `(global-set! ,scm-id ,val))
	 (this.named-fun? ;; named funs are read-only
	  `(begin ,val #f))
	 (else
	  `(begin (set! ,scm-id ,val) ,scm-id)))))
(define-pmethod (Intercepted-var-set! val)
   (let ((str-id (add-str! (symbol->string this.id)))
	 (obj-id this.obj-id)
	 (intercepted this.intercepted))
      `(if (js-property-contains ,obj-id ,str-id)
	   ;; do not use generic-set! here
	   (js-property-update! ,obj-id ,str-id ,val)
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
	  (let ((str-id (add-str! (symbol->string this.id))))
	     `(env-delete! ,(thread-parameter 'eval-env) ,str-id)))
	 (this.local-eval?
	  (let ((str-id (add-str! (symbol->string this.id))))
	     `(if (not (js-deleted? ,scm-id))
		  (js-property-safe-delete! ,this.eval-obj-id ,str-id)
		  ,(this.eval-next-var.delete))))
	 (else
	  #f))))
(define-pmethod (Intercepted-var-delete)
   (let* ((str-id (add-str! (symbol->string this.id)))
	  (obj-id this.obj-id)
	  (intercepted this.intercepted))
      `(if (js-property-contains ,obj-id ,str-id)
	   (js-property-safe-delete! ,obj-id ,str-id)
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
;; access+base will return a lambda that should then be executed.
;; this is in response to 11.2.3 which states that the function is first
;; evaluated without taking the value.
(define-pmethod (Var-access+base base)
   ;; mostly similar to Var-access
   (let ((scm-id (get/assign-scm-id! this)))
      (cond
	 ;; don't even try to do fancy stuff, when we are in an eval. just
	 ;; access the eval object
	 ((and (thread-parameter 'eval?)
	       this.global?)
	  (let ((v (gensym 'v))
		(o (gensym 'o))
		(str-id (add-str! (symbol->string this.id))))
	     `(let ((,o (env-object ,(thread-parameter 'eval-env) ,str-id)))
		 (unless (Js-Activation-Object? ,o)
		    (set! ,base ,o))
		 (lambda () (js-property-get ,o ,str-id)))))
	 (this.local-eval?
	  ;; stupid thing: evals might delete local variables. If there's a
	  ;; local eval, we need to verify first, if the variable actually
	  ;; still exists.
	  `(if (not (js-deleted? ,scm-id))
	       (lambda () ,scm-id)
	       ,(this.eval-next-var.access+base base)))
	 (this.global?
	  `(begin
	      ;; first read var. this will throw an error if the var is
	      ;; not declared.
	      (global-read ,scm-id)
	      ;; but then return a lambda that will reread again.
	      (lambda ()
		 (if (global-declared? ,scm-id)
		     (global-read ,scm-id)
		     (js-undefined)))))
	 (else
	  `(lambda () ,scm-id)))))
(define-pmethod (Intercepted-var-access+base base)
   (let ((str-id (add-str! (symbol->string this.id)))
	 (obj-id this.obj-id)
	 (intercepted this.intercepted))
      `(if (js-property-contains ,obj-id ,str-id)
	   (begin
	      (set! ,base ,obj-id)
	      (lambda () (js-property-get ,obj-id ,str-id)))
	   ,(intercepted.access+base base))))
(define-pmethod (This-var-access+base base) '(lambda () this))
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
   (strs-reset!)
   (let* ((tlo (thread-parameter 'top-level-object))
	  (in-eval? (thread-parameter 'eval?))
	  (tl-this (thread-parameter 'top-level-this))
	  (declared-globals-w/o-this
	   (filter (lambda (var)
		      (not (inherits-from? var (node 'This-var))))
		   this.declared-globals))
	  (compiled-body (this.body.traverse)))
      (if (not in-eval?)
	  `(begin
	      ;; declare strings
	      ;; create 'this'
	      ;; declare global variables (declared and implicit)
	      ;; create fun-strings (when configured this way)
	      ;; create 'init-declared', 'init-implicit' and 'run-top-level
	      ;; if not in module execute these funs. (otherwise somebody else
	      ;;   needs to invoke them.)
	      ,@(map (lambda (p)
			`(define ,(cdr p)
			    ,(utf8->js-string-literal (car p) #t)))
		     (strs-alist))
	      (define this ,tl-this)
	      ,@(map (lambda (var)
			`(define ,(var.compiled-id) #unspecified))
		     declared-globals-w/o-this)
	      ,@(map (lambda (decl)
			`(define ,(decl.var.compiled-id) #unspecified))
		     this.implicit-globals)
	      ,@(if (config 'function-strings)
		    (hashtable-map this.function-str-ids-ht
				   (lambda (str-id str)
				      `(define ,str-id
					  (utf8->js-string-literal ,str))))
		    '())
	      (define (js-init-declared)
		 #unspecified ;; so the fun is never empty
		 ,@(map (lambda (var)
			   `(set! ,(var.compiled-id)
				  (create-declared-global
				   ,(utf8->js-string-literal
				     (symbol->string var.id)))))
			declared-globals-w/o-this))
	      (define (js-init-implicit)
		 #unspecified ;; so the fun is never empty
		 ,@(map (lambda (decl)
			   (let ((var decl.var))
			      `(set! ,(var.compiled-id)
				     (create-implicit-global
				      ,(utf8->js-string-literal
					(symbol->string var.id))))))
			this.implicit-globals))
	      (define (js-run-top-level)
		 ,compiled-body)
	      ,@(if (config 'module)
		    '()
		    '((js-init-declared)
		      (js-init-implicit)
		      (js-run-top-level))))

	  ;; eval does not do anything for implicit vars.
	  ;; declared globals: if the var is a declared function, then it
	  ;; must replace the original entry (including the attributes).
	  ;; otherwise just make sure there is an entry.
	  `(let ((this ,tl-this)
		 ,@(map (lambda (p)
			   `(,(cdr p) ,(utf8->js-string-literal (car p))))
			(strs-alist))
		 ,@(if (config 'function-strings)
		       (hashtable-map this.function-str-ids-ht
				      (lambda (id str)
					 `(,id (utf8->js-string-literal ,str))))
		       '()))
	      ,@(map (lambda (var)
			(let ((id-str (symbol->string var.id))
			      (str-id (symbol-append 'jsstr- (gensym))))
			   (if var.fun?
			       ;; declared fun.
			       ;; always replace existing entry.
			       ;; initially set to undefined, but that
			       ;; changes soon.
			       ;; we need to use generic-set, otherwise we
			       ;; can't overwrite existing entries, if they
			       ;; have different attributes.
			       `(js-property-generic-set!
				 ,tlo
				 ,(utf8->js-string-literal id-str)
				 (js-undefined)
				 (default-attributes))
			       `(let ((,str-id
				       ,(utf8->js-string-literal id-str)))
				   (unless (js-property-one-level-contains?
					    ,tlo ,str-id)
				   (js-property-set! ,tlo ,str-id (js-undefined)))))))
		     declared-globals-w/o-this)
	      ,compiled-body))))

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
   (let ((prop (gensym 'prop))
	 (val (gensym 'val))
	 (read-only? (gensym 'ro))
	 (deletable? (gensym 'del))
	 (enumerable? (gensym 'enum)))
      `(js-property-for-each
	(jsop-any->object ,(this.obj.traverse))
	(lambda (,prop ,val ,read-only? ,deletable? ,enumerable?)
	   (when ,enumerable?
	      ,(this.lhs.var.set! prop)
	      ,(this.body.traverse))))))

(define-pmethod (With-out)
   ;; the obj is not yet transformed to an object.
   `(let ((,this.obj-id (jsop-any->object ,(this.obj.traverse))))
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
			     (let ((fall-through (lambda () #unspecified)))
				,(clause-body (car clauses)))))
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
	 (exc-js-str (add-str! (symbol->string this.decl.var.id)))
	 (obj-id this.obj-id)
	 (compiled-body (this.body.traverse)))
      ;; the exc-scm-id is assigned in the Try-clause.
      `(lambda (,exc-scm-id)
	  (lambda ()
	     (let* ((,exc-scm-id (error->js-exception ,exc-scm-id))
		    (,obj-id (js-create-scope-object (js-object-literal '()))))
		;; 12.14
		(scope-var-add ,obj-id
			       ,exc-js-str
			       ,exc-scm-id
			       (get-Attributes dont-delete))
		,compiled-body)))))

(define-pmethod (Named-fun-out)
   (let ((fun-scm-id (this.decl.var.compiled-id))
	 (fun-js-str (add-str! (symbol->string this.decl.var.id)))
	 (obj-id this.obj-id)
	 (compiled-body (this.body.traverse)))
      `(let ((,obj-id (js-create-scope-object (js-object-literal '()))))
	  (letrec ((,fun-scm-id ,compiled-body))
	     ;; 13
	     (scope-var-add ,obj-id
			    ,fun-js-str
			    ,fun-scm-id
			    (get-Attributes dont-delete read-only))
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
				       ,(add-str! (symbol->string var.id))
				       ,(var.compiled-id)
				       (declared-attributes)))
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
		    `(list ,@this.str) ;; something like fun-str-id, from, to.
		    `(add-str! ,(symbol->string (gensym "function"))))
	       (,@compiled-params)
	       ,compiled-body)))

(define-pmethod (Vassig-out)
   (this.lhs.var.set! (this.val.traverse)))

(define-pmethod (Fun-binding-out)
   (if (and (thread-parameter 'eval?)
	    this.lhs.var.global?)
       ;; assign fun-binding to top-level object and not to current
       ;; environment. (Rhino 1.7 release 1 Pre 2007 11 25 has this bug)
       `(js-property-set! ,(thread-parameter 'top-level-object)
			  ,(add-str! (symbol->string this.lhs.var.id))
			  ,(this.val.traverse))
       (pcall this Vassig-out)))

(define-pmethod (Accsig-out)
   (let* ((tmp-o (gensym 'tmp-o))
	  (tmp-field (gensym 'tmp-field))
	  (tmp-object-o (gensym 'tmp-object-o))
	  (tmp-string-field (gensym 'tmp-string-field))
	  (traversed-obj (this.lhs.obj.traverse))
	  (traversed-obj-object (comp:any->js-object traversed-obj))
	  (traversed-field (this.lhs.field.traverse))
	  (traversed-field-string (comp:any->js-string traversed-field))
	  (traversed-val (this.val.traverse)))
      ;; we need all these tmp-variables, to ensure the correct order of
      ;; evaluation.
      `(let* ((,tmp-o ,(if traversed-obj-object #unspecified traversed-obj))
	      (,tmp-field ,(if traversed-field-string #unspecified traversed-field))
	      (,tmp-object-o ,(or traversed-obj-object
				  `(jsop-any->object ,tmp-o)))
	      (,tmp-string-field ,(or traversed-field-string
				      `(any->js-string ,tmp-field))))
	  (js-property-set! ,tmp-object-o
			    ,tmp-string-field
			    ,traversed-val))))

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
      ((and (eq? this.op.var.id 'unary--)
	    (inherits-from? (car this.args) (node 'Number)))
       `(negfl ,((car this.args).traverse)))
      ((and (eq? this.op.var.id 'unary-+)
	    (inherits-from? (car this.args) (node 'Number)))
       ((car this.args).traverse))
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

;; we assume:
;;  - args expressions that need to be put into a let for explicit left to
;;   right evaluation.
(define (generate-call fun base args)
   (if (null? args)
       `(js-call ,fun ,base)
       (let ((arg-names (map (lambda (ign) (gensym 'arg)) args)))
	  `(let* ,(map list arg-names args)
	      (js-call ,fun ,base ,@arg-names)))))

(define-pmethod (Call-out)
   ;; 11.2.3
   (cond
      ((and (inherits-from? this.op (node 'Var-ref))
	    (inherits-from? this.op.var (node 'Runtime-var))
	    this.op.var.operator?)
       (Operator-out this))
      ((inherits-from? this.op (node 'Var-ref))
       (let ((base (gensym 'base))
	     (f (gensym 'f)))
	  `(let* ((,base *js-global-this*)
		  ;; if the variable has a base, it will update the
		  ;; base-variable (during runtime). we then perform
		  ;; (implicitely) a method-call.
		  (,f ,(this.op.var.access+base base)))
	      ,(generate-call `(,f)
			      base ;; might have been updated by access+base.
			      (map-node-compile this.args)))))
      (else
       (let ((f (gensym 'f)))
	  `(let ((,f ,(this.op.traverse)))
	      ,(generate-call f #f (map-node-compile this.args)))))))

; (define-pmethod (Binary-out)
;    `(,(this.op.traverse)
;      ,@(map-node-compile this.args)))

(define-pmethod (Delete-call-out)
   ((car this.args).var.delete))

(define-pmethod (Delete-property-call-out)
   `(jsop-delete ,((car this.args).traverse)
		 ,((cadr this.args).traverse)))

(define-pmethod (Method-call-out)
   (let* ((tmp-o (gensym 'o))
	  (tmp-o-object (gensym 'o-object))
	  (tmp-this (gensym 'this))
	  (tmp-field (gensym 'field))
	  (tmp-field-string (gensym 'field-string))
	  (traversed-o (this.op.obj.traverse))
	  (traversed-o-object (comp:any->js-object traversed-o))
	  (traversed-this (this.op.obj.traverse))
	  (traversed-this-object (comp:any->js-object traversed-o))
	  (traversed-field (this.op.field.traverse))
	  (traversed-field-string (comp:any->js-string traversed-field)))
      ;; we need all these tmp-variables, to ensure the correct order of
      ;; evaluation.
      `(let* ((,tmp-o ,(if traversed-o-object #unspecified traversed-o))
	      (,tmp-field ,(if traversed-field-string
			       #unspecified
			       traversed-field))
	      (,tmp-this ,(or traversed-o-object
			      `(any->object ,tmp-o)))
	      (,tmp-o-object (safe-js-object ,tmp-this))
	      (,tmp-field-string ,(or traversed-field-string
				      `(any->js-string ,tmp-field))))
	  ,(generate-call `(js-property-get ,tmp-o-object ,tmp-field-string)
			  tmp-this
			  (map-node-compile this.args)))))

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
	  (if (eq? ,t (js-eval-orig))
	      ;; we have a real eval here
	      ,(if (null? this.args)
		   `(js-undefined)
		   `(js-eval ,((car this.args).traverse)
			     ,tlo-id/obj this ,next-env
			     ,@env-vars))
	      (js-call ,t #f ,@(map-node-compile this.args))))))

(define-pmethod (New-out)
   `(js-new ,(this.class.traverse)
	    ,@(map-node-compile this.args)))

(define-pmethod (Access-out)
   (let* ((tmp-o (gensym 'tmp-o))
	  (tmp-field (gensym 'tmp-field))
	  (tmp-object-o (gensym 'tmp-object-o))
	  (tmp-string-field (gensym 'tmp-string-field))
	  (traversed-o (this.obj.traverse))
	  (traversed-o-object (comp:any->js-object traversed-o))
	  (traversed-field (this.field.traverse))
	  (traversed-field-string (comp:any->js-string traversed-field)))
      ;; we need all these tmp-variables, to ensure the correct order of
      ;; evaluation.
      `(let* ((,tmp-o ,(if traversed-o-object #unspecified traversed-o))
	      (,tmp-field ,(if traversed-field-string
			       #unspecified
			       traversed-field))
	      (,tmp-object-o ,(if traversed-o-object
				  `(safe-js-object ,traversed-o-object)
				  `(jsop-any->object ,tmp-o)))
	      (,tmp-string-field ,(or traversed-field-string
				      `(any->js-string ,tmp-field))))
	  (js-property-get ,tmp-object-o ,tmp-string-field))))

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
		  (bignum->flonum
		   (string->bignum
		    (substring str 2 (string-length str)) 16))
		  (string->real str))))
      (cond
	 ((flonum? nb)
	  nb)
	 (else
	  (error "Number-out"
		 "could not transform to number:"
		 str)))))

(define (unescape str)
   (define (char-hex? c)
      (or (char-numeric? c)
	  (case c
	     ((#\a #\b #\c #\d #\e #\f) #t)
	     ((#\A #\B #\C #\D #\E #\F) #t)
	     (else #f))))
   (let ((char-l (string->list str)))
      (let loop ((char-l char-l)
		 (rev-res '()))
	 (if (null? char-l)
	     (list->string (reverse rev-res))
	     (cond
		((and (char=? #\\ (car char-l))
		      (char=? #\0 (cadr char-l)))
		 ;; only if the following char is not
		 ;; another digit count it as null-char.
		 ;; was initially octal escape
		 ;; sequence. (now deprecated)
		 (if (or (null? (cddr char-l))
			 (not (char-numeric? (caddr char-l))))
		     (loop (cddr char-l)
			   (cons #\null rev-res))
		     (loop (cddr char-l)
			   (cons* (cadr char-l)
				  (car char-l)
				  rev-res))))
		((and (char=? #\\ (car char-l))
		      (not (null? (cddr char-l)))
		      (not (null? (cdddr char-l)))
		      (char-hex? (caddr char-l))
		      (char-hex? (cadddr char-l))
		      (let ((char-l+4 (cddddr char-l)))
			 (or (char=? #\x (cadr char-l))
			     (and (char=? #\u (cadr char-l))
				  (not (null? char-l+4))
				  (not (null? (cdr char-l+4)))
				  (char-hex? (car char-l+4))
				  (char-hex? (cadr char-l+4))))))
		 ;; we can't really handle unicode chars now (depends on the
		 ;; configuration (utf8, utf16, etc). so we leave them
		 ;; in the string.
		 (loop (cddr char-l)
		       (cons* (cadr char-l)
			      (car char-l)
			      rev-res)))
		((char=? #\\ (car char-l))
		 (loop (cddr char-l)
		       ;; 7.8.4
		       (cons (case (cadr char-l)
				;; SingleEscapeCharacters vvv
				((#\b) #a008)
				((#\t) #\tab)
				((#\n) #\newline)
				((#\v) #a011)
				((#\f) #a012)
				((#\r) #\return)
				;; the else-part covers ', " and \
				;; SingleEscapeCharacters ^^^
				(else (cadr char-l)))
			     rev-res)))
		(else
		 (loop (cdr char-l)
		       (cons (car char-l) rev-res))))))))

(define (unescaped-minus-quotes s)
   ;; 7.8.4
   (unescape (substring s 1 (-fx (string-length s) 1))))

(define-pmethod (String-out)
   (add-str! (unescaped-minus-quotes this.val)))

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
   `(,(list 'unquote (comp:any->js-string (this.name.traverse)))
     ,(list 'unquote (this.val.traverse))))

(define-pmethod (Reg-exp-out)
   (let* ((pattern/flags this.pattern)
	  (last-/ (string-index-right pattern/flags #\/))
	  (pattern (substring pattern/flags 1 last-/))
	  (flags (substring pattern/flags
			    (+ last-/ 1)
			    (string-length pattern/flags))))
      `(js-new (global-read *jsg-RegExp*)
	       ,(add-str! pattern)
	       ,(add-str! flags))))
