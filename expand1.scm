(module expand1
   (include "protobject.sch")
   (include "nodes.sch")
   (option (loadq "protobject-eval.sch"))
   (import protobject
	   verbose
	   symbol
	   nodes
	   label
	   var)
   (export (expand1! tree::pobject)))

;; - replace Named-Fun with Vassig
;; - add implicit "Labelled"-nodes for Loops, Funs and Case
;; - replace For with While
;; - add implicit "Fall-through"-node.
;; - add implicit "return undefined".
;; - replace x += exp with x = x + exp ...
;; - replace Calls that are actually method calls with Method-call
;; - replace void x; with (begin x, undefined)
;; - (with expr body) is transformed into
;;          (let ((tmp (any->object expr))) (with tmp body))
;;    this could be done after the symbol-pass too.
;; - delete X is transformed to
;;    * delete o f if X is of form o[f]
;;    * false if X is X is of form v and v is a declared var.
;;    * with-delete if X is of form v and v might be with-intercepted.
;;    * delete-implicit-global if X is of form v and v is undeclared.

(define (expand1! tree)
   (verbose " expand")
   (overload traverse! expand! (Node
				Named-fun
				For
				While
				(Do While-expand!) ; same as for While
				(For-in While-expand!) ; same as for While
				Switch-clause
				Fun
				Call
				Vassig-op
				Accsig-op
				Unary)
	     (tree.traverse!)))

(define-pmethod (Node-expand!)
   (this.traverse0!))

(define-pmethod (Named-fun-expand!)
   (this.traverse0!)
   (new Vassig this.decl this.fun))

(define-pmethod (For-expand!)
   (let* ((continue-labelled (new Labelled
				  *default-continue-label-id*
				  (this.body.traverse!)))
	  (while-body (if this.incr
			  (new Block (list continue-labelled
					   (this.incr.traverse!)))
			  continue-labelled))
	  (while (new While (if this.test
				(this.test.traverse!)
				(new Bool #t))
		      while-body))
	  (block (if this.init
		     (new Block (list (this.init.traverse!) while))
		     while))
	  (break-labelled (new Labelled *default-break-label-id* block))
	  (new-this break-labelled))
      (set! continue-labelled.label this.continue-label)
      (set! break-labelled.label this.break-label)
      (delete! this.continue-label)
      (delete! this.break-label)
      new-this))

(define-pmethod (While-expand!)
   (let ((continue-labelled (new Labelled *default-continue-label-id* this.body)))
      (set! continue-labelled.label this.continue-label)
      (delete! this.continue-label)
      (set! this.body continue-labelled)
      (let* ((old-this (this.traverse0!))
	     (break-labelled (new Labelled *default-break-label-id* old-this)))
	 (set! break-labelled.label this.break-label)
	 (delete! this.break-label)
	 break-labelled)))

(define-pmethod (Switch-clause-expand!)
   (this.traverse0!)
   (let ((new-body (new Labelled #f (new Block (list this.body
						     (new Fall-through))))))
      (set! new-body.label this.break-label)
      (set! this.body new-body)
      (delete! this.break-label)
      this))

(define-pmethod (Fun-expand!)
   (this.traverse0!)
   (let ((return (new Return (new Undefined))))
      (set! return.label this.return-label)
      (let ((new-body (new Labelled #f (new Block (list this.body return)))))
	 (set! new-body.label this.return-label)
	 (set! this.body new-body)
	 (delete! this.return-label)
	 this)))

(define-pmethod (Call-expand!)
   (this.traverse0!)
   (if (inherits-from? this.op Access)
       (let* ((op this.op)
	      (tmp-o (gensym 'o))
	      (o-decl (Decl-of-new-Var tmp-o))
	      (o-var o-decl.var)
	      (assig (new Vassig o-decl op.obj)))
	  (set! op.obj (o-decl.var.reference))
	  (new Sequence (list assig
			      (new Method-call
				   (o-var.reference)
				   op
				   this.args))))
       this))

(define-pmethod (Vassig-op-expand!)
   (this.traverse0!)
   (let ((new-rhs (new Binary
		       (this.lhs.var.reference)
		       this.op
		       this.val)))
      (this.lhs.var.assig new-rhs)))

(define-pmethod (Accsig-op-expand!)
   (this.traverse0!)
   (let* ((o this.lhs.obj)
	  (field this.lhs.field)
	  (tmp-o-id (gensym 'tmp-o))
	  (tmp-field-id (gensym 'tmp-field))
	  (tmp-o-decl (Decl-of-new-Var tmp-o-id))
	  (tmp-field-decl (Decl-of-new-Var tmp-field-id))
	  (init-o (new Init tmp-o-decl o))
	  (tmp-o-var tmp-o-decl.var)
	  (init-field (new Init tmp-field-decl field))
	  (tmp-field-var tmp-field-decl.var)

	  (access-lhs (new Access
			   (tmp-o-var.reference)
			   (tmp-field-var.reference)))
	  (access-rhs (new Access
			   (tmp-o-var.reference)
			   (tmp-field-var.reference)))
	  (rhs-binary (new Binary
			   access-rhs
			   this.op
			   this.val))
	  (accsig (new Accsig access-lhs rhs-binary))
	  (sequence (new Sequence (list init-o
					init-field
					accsig))))
      sequence))

(define-pmethod (Unary-expand!)
   (this.traverse0!)
   (let ((op this.op.id)
	 (expr (car this.args)))
      (cond
	 ((eq? op 'void)
	  (new Sequence
	       `(,expr ,(new Undefined))))
	 ((eq? op 'delete)
	  (cond
	     ((inherits-from? expr Access)
	      (new Call
		   (id->runtime-var 'delete)
		   (list expr.obj expr.field)))
	     ((and (inherits-from? expr Var-ref)
		   (inherits-from? expr.var With-var))
	      (let loop ((rev-surrounding-withs '())
			 (var expr.var))
		 (cond
		    ((inherits-from? var With-var)
		     (loop (cons var.with rev-surrounding-withs)
			   var.with.intercepted))
		    (var.implicit-global?
		     (new Call
			  (id->runtime-var 'with-delete)
			  `(,(reverse! rev-surrounding-withs)
			    ,(symbol->string var.id)
			    ,var)))
		    (else
		     (new Call
			  (id->runtime-var 'with-delete)
			  `(,(reverse! rev-surrounding-withs)
			    ,(symbol->string var.id)
			    ,(new Bool #f)))))))
	     ((and (inherits-from? expr Var-ref)
		   expr.var.implicit-global?)
	      (new Call
		   (id->runtime-var 'delete-implicit-global)
		   (list expr (symbol->string expr.var.id))))
	     ((inherits-from? expr Var-ref)
	      ;; neither with-var, nor implicit-global
	      (new Bool #f))
	     (else
	      (new Sequence
		   (list expr
			 (new Bool #t))))))
	 (else
	  this))))

(define-pmethod (With-expand!)
   (this.traverse0!)
   (let ((tmp-decl (Decl-of-new-Var (gensym 'with)))
	 (old-expr this.expr))
      (set! this.expr (tmp-decl.var.reference))
      (new Sequence
	   `(,(new Vassig
		   tmp-decl
		   (new Unary
			(new Var-ref 'any->object)
			old-expr))
	     ,this))))
