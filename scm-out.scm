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
			   Obj-init
			   Reg-exp
			   Let
			   Bind-exit
			   Bind-exit-invoc)
	     (overload traverse out (Var Runtime-var Imported-var)
		       (overload traverse out (Label)
				 (tree.traverse)))))

(define (map-node-compile l)
   (map (lambda (n)
	   (n.traverse))
	l))

(define-pmethod (Var-out)
   this.id)
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
   (this.body.traverse))

(define-pmethod (Begin-out)
   `(begin
       ,@(map-node-compile this.els)))

(define-pmethod (Let-out)
   `(let ((,(this.vassig.lhs.var.traverse) ,(this.vassig.val.traverse)))
       ,(this.body.traverse)))

(define-pmethod (Var-ref-out)
   (this.var.traverse))

(define-pmethod (NOP-out)
   '*js-Undefined*)

(define-pmethod (If-out)
   `(if (js-boolify ,(this.test.traverse))
	,(this.then.traverse)
	,(this.else.traverse)))

(define-pmethod (While-out)
   (let ((loop (gensym 'loop)))
      `(let ,loop (())
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
   '(TODO TODO TODO With TODO TODO TODO))

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
			   `((=== ,(clause-expr clause) ,key)
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
   '(TODO TODO TODO throw TODO TODO TODO))

(define-pmethod (Try-out)
   '(TODO TODO TODO throw TODO TODO TODO))

(define-pmethod (Catch-out)
   '(TODO TODO TODO catch TODO TODO TODO))

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
   (let ((tmp (gensym 'tmp)))
      `(let ((,tmp ,(this.val.traverse)))
	  (set! ,(this.lhs.traverse) ,tmp)
	  ,tmp)))

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
       `(,(this.op.traverse)
	 ,@(map-node-compile this.args))
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
   '(TODO TODO TODO array TODO TODO TODO))

(define-pmethod (Obj-init-out)
   '(TODO TODO TODO obj-init TODO TODO TODO))

(define-pmethod (Reg-exp-out)
   '(TODO TODO TODO reg-exp TODO TODO TODO))
