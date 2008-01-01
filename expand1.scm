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

;; - add implicit "Labelled"-nodes for Loops, Funs and Case
;; - replace For with While
;; - add implicit "Fall-through"-node.
;; - add implicit "return undefined".
;; - replace x += exp with x = x + exp ...
;; - replace Calls that are actually method calls with Method-call
;; - replace void x; with (begin x, undefined)
;; - delete X is transformed to
;;    * delete o f (Delete-property-call) if X is of form o[f]
;;    * delete v   (Delete-call) otherwise
;; - replace ++x with tmp = x; x = x+1; tmp  (same for --) 11.4.4, 11.4.5
;; - replace x++ with x = x+1;  (same for --) 11.3.1, 11.3.2
(define (expand1! tree)
   (verbose " expand")
   (overload traverse! expand! (Node
				For
				While
				(Do While-expand!) ; same as for While
				(For-in While-expand!) ; same as for While
				Switch-clause
				Fun
				Call
				Vassig-op
				Accsig-op
				Unary
				Postfix)
	     (tree.traverse!)))

(define-pmethod (Node-expand!)
   (this.traverse0!))

(define-pmethod (For-expand!)
   (let* ((continue-labelled (new-node Labelled
				       *default-continue-label-id*
				       (this.body.traverse!)))
	  (while-body (if this.incr
			  (new-node Block (list continue-labelled
						(this.incr.traverse!)))
			  continue-labelled))
	  (while (new-node While (if this.test
				     (this.test.traverse!)
				     (new-node Bool #t))
		      while-body))
	  (block (if this.init
		     (new-node Block (list (this.init.traverse!) while))
		     while))
	  (break-labelled (new-node Labelled *default-break-label-id* block))
	  (new-this break-labelled))
      (set! continue-labelled.label this.continue-label)
      (set! break-labelled.label this.break-label)
      (delete! this.continue-label)
      (delete! this.break-label)
      new-this))

(define-pmethod (While-expand!)
   (let ((continue-labelled (new-node Labelled *default-continue-label-id* this.body)))
      (set! continue-labelled.label this.continue-label)
      (delete! this.continue-label)
      (set! this.body continue-labelled)
      (let* ((old-this (this.traverse0!))
	     (break-labelled (new-node Labelled *default-break-label-id* old-this)))
	 (set! break-labelled.label this.break-label)
	 (delete! this.break-label)
	 break-labelled)))

(define-pmethod (Switch-clause-expand!)
   (this.traverse0!)
   (let ((new-body (new-node Labelled #f
			     (new-node Block (list this.body
						   (new-node Fall-through))))))
      (set! new-body.label this.break-label)
      (set! this.body new-body)
      (delete! this.break-label)
      this))

(define-pmethod (Fun-expand!)
   (this.traverse0!)
   (let ((return (new-node Return (new-node Undefined))))
      (set! return.label this.return-label)
      (let ((new-body (new-node Labelled #f (new-node Block (list this.body return)))))
	 (set! new-body.label this.return-label)
	 (set! this.body new-body)
	 (delete! this.return-label)
	 this)))

(define-pmethod (Call-expand!)
   (this.traverse0!)
   (if (inherits-from? this.op (node 'Access))
       (new-node Method-call
	    this.op
	    this.args)
       this))

(define-pmethod (Vassig-op-expand!)
   (this.traverse0!)
   (let ((new-rhs (new-node Binary
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
	  (init-o (new-node Init tmp-o-decl o))
	  (tmp-o-var tmp-o-decl.var)
	  (init-field (new-node Init tmp-field-decl field))
	  (tmp-field-var tmp-field-decl.var)

	  (access-lhs (new-node Access
			   (tmp-o-var.reference)
			   (tmp-field-var.reference)))
	  (access-rhs (new-node Access
			   (tmp-o-var.reference)
			   (tmp-field-var.reference)))
	  (rhs-binary (new-node Binary
			   access-rhs
			   this.op
			   this.val))
	  (accsig (new-node Accsig access-lhs rhs-binary))
	  (sequence (new-node Sequence (list init-o
					init-field
					accsig))))
      (set! tmp-o-var.internal? #t)
      (set! tmp-field-var.internal? #t)
      sequence))

(define-pmethod (Unary-expand!)
   (this.traverse0!)
   (let ((op this.op.id)
	 (expr (car this.args)))
      (cond
	 ((eq? op 'void)
	  (new-node Sequence
	       `(,expr ,(new-node Undefined))))
	 ((eq? op 'delete)
	  (cond
	     ((inherits-from? expr (node 'Access))
	      (new-node Delete-property-call
			this.op
			expr.obj
			expr.field))
	     (else
	      (new-node Delete-call
			this.op
			expr))))
	 ((and (or (eq? op '++)
		   (eq? op '--))
	       (inherits-from? expr (node 'Access)))
	  ;; 11.4.4, 11.4.5
	  (let* ((obj expr.obj)
		 (field expr.field)
		 (tmp-obj-id (gensym 'o))
		 (tmp-obj-decl (Decl-of-new-Var tmp-obj-id))
		 (tmp-obj-var tmp-obj-decl.var)
		 (tmp-prop-id (gensym 'prop))
		 (tmp-prop-decl (Decl-of-new-Var tmp-prop-id))
		 (tmp-prop-var tmp-prop-decl.var)
		 (any->number-var (id->runtime-var 'any->number))
		 (op-var (id->runtime-var (if (eq? op '++) '+ '-))))
	     (new-node
	      Sequence
	      `(,(new-node Vassig tmp-obj-decl obj)
		,(new-node Vassig tmp-prop-decl field)
		,(new-node Accsig
			   (new-node Access
				     (tmp-obj-var.reference)
				     (tmp-prop-var.reference))
			   (new-node
			    Binary
			    (new-node Unary
				      (any->number-var.reference)
				      (new-node Access
						(tmp-obj-var.reference)
						(tmp-prop-var.reference)))
			    (op-var.reference)
			    (new-node Number "1")))))))
	 ((or (eq? op '++)
	      (eq? op '--))
	  ;; 11.4.4, 11.4.5
	  (new-node
	   Vassig
	   expr ;; a variable (either Var-ref or Decl)
	   (new-node Binary
		     (new-node Unary
			       ((id->runtime-var 'any->number).reference)
			       (expr.var.reference))
		     ((id->runtime-var (if (eq? op '++) '+ '-)).reference)
		     (new-node Number "1"))))
	 (else
	  this))))

(define-pmethod (Postfix-expand!)
   ;;11.3.1, 11.3.2
   (this.traverse0!)
   (let ((op this.op.id)
	 (expr (car this.args)))
      (if (inherits-from? expr (node 'Access))
	  (let* ((obj expr.obj)
		 (field expr.field)
		 (tmp-return-id (gensym 'tmp))
		 (tmp-return-decl (Decl-of-new-Var tmp-return-id))
		 (tmp-return-var tmp-return-decl.var)
		 (tmp-obj-id (gensym 'o))
		 (tmp-obj-decl (Decl-of-new-Var tmp-obj-id))
		 (tmp-obj-var tmp-obj-decl.var)
		 (tmp-prop-id (gensym 'prop))
		 (tmp-prop-decl (Decl-of-new-Var tmp-prop-id))
		 (tmp-prop-var tmp-prop-decl.var)
		 (any->number-var (id->runtime-var 'any->number))
		 (op-var (id->runtime-var (if (eq? op '++) '+ '-))))
	     (new-node
	      Sequence
	      `(,(new-node Vassig tmp-obj-decl obj)
		,(new-node Vassig tmp-prop-decl field)
		,(new-node Vassig
			   tmp-return-decl
			   (new-node Unary
				     (any->number-var.reference)
				     (new-node Access
					       (tmp-obj-var.reference)
					       (tmp-prop-var.reference))))
		,(new-node Accsig
			   (new-node Access
				     (tmp-obj-var.reference)
				     (tmp-prop-var.reference))
			   (new-node Binary
				     (tmp-return-var.reference)
				     (op-var.reference)
				     (new-node Number "1")))
		,(tmp-return-var.reference))))
	  (let* ((tmp-return-id (gensym 'tmp))
		 (tmp-return-decl (Decl-of-new-Var tmp-return-id))
		 (tmp-return-var tmp-return-decl.var)
		 (any->number-var (id->runtime-var 'any->number))
		 (op-var (id->runtime-var (if (eq? op '++) '+ '-))))
	     (new-node
	      Sequence
	      `(,(new-node Vassig
			   tmp-return-decl
			   (new-node Unary
				     (any->number-var.reference)
				     expr))
		,(expr.var.assig (new-node Binary
					   (tmp-return-var.reference)
					   (op-var.reference)
					   (new-node Number "1")))
		,(tmp-return-var.reference)))))))
