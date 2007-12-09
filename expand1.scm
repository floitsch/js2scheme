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
;; - (with expr body) is transformed into
;;          (let ((tmp expr)) (with tmp body))
;; - Decl-Withs however receive just a new decl.
;; - delete X is transformed to
;;    * delete o f (Delete-property-call) if X is of form o[f]
;;    * delete v   (Delete-call) otherwise
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
				With
				Decl-With)
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
	 (else
	  this))))

(define-pmethod (With-expand!)
   (this.traverse0!)
   (let ((tmp-decl (Decl-of-new-Var (gensym 'with)))
	 (old-expr this.obj))
      (set! tmp-decl.var.internal? #t)
      (set! this.obj (tmp-decl.var.reference))
      (new-node Sequence
	   `(,(new-node Vassig
			tmp-decl
			old-expr)
	     ;; the tmp-variable is not yet an object.
	     ;; scm-out still has to do that.
	     ,this))))

(define-pmethod (Decl-With-expand!)
   (this.traverse0!)
   (let ((decl (Decl-of-new-Var (gensym 'with))))
      (set! decl.var.internal? #t)
      (set! this.obj decl)
      this))
