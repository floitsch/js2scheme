(module scm-out
   (include "protobject.sch")
   (include "nodes.sch")
   (option (loadq "protobject-eval.sch"))
   (import parser
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
			   Fun
			   Vassig
			   Accsig
			   Call
			   Method-call
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
	     (overload traverse out (Var This-var Runtime-var Imported-var)
		       (overload traverse out (Label)
				 (tree.traverse)))))

(define (map-node-compile l)
   (map (lambda (n)
	   (n.traverse))
	l))

(define-pmethod (Var-out)
   (symbol-append 'js- this.id))
(define-pmethod (This-var-out)
   'this)
(define-pmethod (Runtime-var-out)
   this.scm-id)
(define-pmethod (Imported-var-out)
   this.scm-id)

(define-pmethod (Label-out)
   (if (not this.generated)
       (set! this.generated (gensym 'label-id)))
   this.generated)

(define-pmethod (Node-out)
   (error "Node-out"
	  "forgot node type: "
	  (pobject-name this)))

(define-pmethod (Program-out)
   (if (null? this.implicit-globals)
       (this.body.traverse)
       `(let (,@(map (lambda (decl)
			`(,(decl.var.traverse) *js-Undeclared*))
		     this.implicit-globals))
	   ,(this.body.traverse))))
   

(define-pmethod (Begin-out)
   `(begin
       ,@(map-node-compile this.els)))

(define-pmethod (Let*-out)
   `(let* (,@(map (lambda (vassig)
		     `(,(vassig.lhs.var.traverse) ,(vassig.val.traverse)))
		  this.vassigs))
       ,(this.body.traverse)))

(define-pmethod (Var-ref-out)
   ;; TODO: handle with-variables
   (let ((var this.var))
      (if var.global?
	  `(check-undeclared ,(this.var.traverse) ',this.var.id)
	  (this.var.traverse))))

(define-pmethod (NOP-out)
   '*js-Undefined*)

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
   `(for-each (lambda (,(this.lhs.var.traverse)) ,(this.body.traverse))
	      (object-for-in-attributes ,(this.obj.traverse))))

(define-pmethod (With-out)
   ''(TODO TODO TODO With TODO TODO TODO))

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
   (let ((inner (if this.catch
		    `(with-handler
			,(this.catch.traverse)
			,(this.body.traverse))
		    (this.body.traverse))))
      (if this.finally
	  `(unwind-protect
	      ,inner
	      ,(this.finally.traverse)))))

(define-pmethod (Catch-out)
   `(lambda (,(this.exception.traverse))
       ,(this.body.traverse)))

(define-pmethod (Bind-exit-out)
   `(bind-exit (,(this.label.traverse))
       ,(this.body.traverse)))

(define-pmethod (Bind-exit-invoc-out)
   `(,(this.label.traverse) ,(this.expr.traverse)))

(define *nb-named-params* 3)
(define-pmethod (Fun-out)
   (let ((compiled-arguments (this.arguments-decl.traverse))
	 (compiled-this (this.this-decl.traverse)))
      `(js-fun ,compiled-this
	       #f ;; no this-fun (only accessible through arguments)
	       ,compiled-arguments
	       (,@(map-node-compile this.params))
	       ,(this.body.traverse))))

(define-pmethod (Vassig-out)
   `(begin
       ;; don't do undeclared-check (-> take directly the var)
       (set! ,(this.lhs.var.traverse) ,(this.val.traverse))
       ,(this.lhs.var.traverse)))

(define-pmethod (Accsig-out)
   (let ((tmp-o (gensym 'tmp-o))
	 (tmp-field (gensym 'tmp-field))
	 (tmp-val (gensym 'tmp-val)))
      `(let* ((,tmp-o ,(this.lhs.obj.traverse))
	      (,tmp-field ,(this.lhs.field.traverse))
	      (,tmp-val ,(this.val.traverse)))
	  (js-property-set! ,tmp-o ,tmp-field ,tmp-val)
	  ,tmp-val)))

(define-pmethod (Call-out)
   (if (and (inherits-from? this.op Var-ref)
	    (inherits-from? this.op.var Runtime-var)
	    this.op.var.operator?)
       (cond
	  ((and (eq? this.op.var.id 'typeof)
		(not (null? this.args))
		(null? (cdr this.args))
		(inherits-from? (car this.args) Var-ref))
	   ;; avoid both undeclared checks (the second must not be here)
	   `(,(this.op.var.traverse) ,((car this.args).var.traverse)))
	  (else
	   `(,(this.op.var.traverse) ;; avoid undeclared-check
	     ,@(map-node-compile this.args))))
       `(js-call ,(this.op.traverse)
		 #f
		 ,@(map-node-compile this.args))))

; (define-pmethod (Binary-out)
;    `(,(this.op.traverse)
;      ,@(map-node-compile this.args)))

(define-pmethod (Method-call-out)
   `(js-call ,(this.op.traverse)
	     ,(this.o.traverse)
	     ,@(map-node-compile this.args)))

(define-pmethod (New-out)
   `(js-new ,(this.class.traverse)
	    ,@(map-node-compile this.args)))

(define-pmethod (Access-out)
   (let ((tmp-o (gensym 'tmp-o))
	 (tmp-field (gensym 'tmp-field)))
      `(let* ((,tmp-o ,(this.obj.traverse))
	      (,tmp-field ,(this.field.traverse)))
	  (js-property-get ,tmp-o ,tmp-field))))

(define-pmethod (This-out)
   'this)

(define-pmethod (Literal-out)
   (error "Literal-out" "forgot literal type" this.val))

(define-pmethod (Undefined-out)
   '*js-Undefined*)

(define-pmethod (Null-out)
   '*js-Null*)

(define-pmethod (Bool-out)
   (if this.val #t #f))

(define-pmethod (Number-out)
   (string->number this.val))

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
