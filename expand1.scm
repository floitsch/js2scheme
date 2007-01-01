(module expand1
   (include "protobject.sch")
   (include "nodes.sch")
   (option (loadq "protobject-eval.sch"))
   (import protobject
	   verbose
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
			       Accsig-op)
	     (tree.traverse!)))

(define-pmethod (Node-expand!)
   (this.traverse0!))

(define-pmethod (For-expand!)
   (let* ((continue-labelled (new Labelled
				  *default-continue-label-id*
				  (this.body.traverse!)))
	  (while-body (new Block (list continue-labelled (this.incr.traverse!))))
	  (while (new While (this.test.traverse!) while-body))
	  (block (new Block (list (this.init.traverse!) while)))
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
	  (tmp-o (gensym 'tmp-o))
	  (tmp-field (gensym 'tmp-field))
	  (init-o (new Init (new Decl tmp-o) o))
	  (init-field (new Init (new Decl tmp-field) field))
	  (access-lhs (new Access
			   (new Var-ref tmp-o)
			   (new Var-ref tmp-field)))
	  (access-rhs (new Access
			   (new Var-ref tmp-o)
			   (new Var-ref tmp-field)))
	  (rhs-binary (new Binary
			   access-rhs
			   this.op
			   this.rhs))
	  (accsig (new Accsig access-lhs rhs-binary))
	  (sequence (new Sequence (list init-o
					init-field
					accsig))))
      sequence))
