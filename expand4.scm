(module expand4
   (include "protobject.sch")
   (include "nodes.sch")
   (option (loadq "protobject-eval.sch"))
   (import protobject
	   verbose
	   nodes)
   (export (expand4! tree::pobject)))

;; - replace ++x with tmp = x; x = x+1; tmp  (same for --)
;; - replace x++ with x = x+1;  (same for --)
(define (expand4! tree)
   (verbose " expand4")
   (overload traverse! expand! (Node
				Unary
				Postfix)
	     (tree.traverse!)))

(define-pmethod (Node-expand!)
   (this.traverse0!))


(define-pmethod (Unary-expand!)
   (this.traverse0!)
   (let ((op this.op.id)
	 (expr (car this.args)))
      (if (or (eq? op '++)
	      (eq? op '--))
	  (if (inherits-from? expr (node 'Access))
	      (let* ((obj expr.obj)
		     (field expr.field)
		     (tmp-obj (gensym 'o))
		     (tmp-f (gensym 'f)))
		 (new-node Sequence
		      `(,(new-node Vassig (new-node Decl tmp-obj) obj)
			,(new-node Vassig (new-node Decl tmp-f) field)
			,(new-node Accsig
			      (new-node Access
				   (new-node Var-ref tmp-obj)
				   (new-node Var-ref tmp-f))
			      (new-node Binary
				   (new-node Access
					(new-node Var-ref tmp-obj)
					(new-node Var-ref tmp-f))
				   (if (eq? op '++)
				       (new-node Var-ref '+)
				       (new-node Var-ref '-))
				   ;; TODO: number as string
				   (new-node Number "1"))))))
	      (new-node Vassig
		   expr ;; a variable
		   (new-node Binary
			(new-node Var-ref expr.id)
			(new-node Var-ref (if (eq? op '++) '+ '-))
			;; TODO: number as string
			(new-node Number "1"))))
	  this)))

(define-pmethod (Postfix-expand!)
   (this.traverse0!)
   (let ((op this.op.id)
	 (expr (car this.args)))
      (if (inherits-from? expr (node 'Access))
	  (let* ((obj expr.obj)
		 (field expr.field)
		 (tmp-return (gensym 'tmp))
		 (tmp-obj (gensym 'o))
		 (tmp-f (gensym 'f)))
	     (new-node Sequence
		  `(,(new-node Vassig (new-node Decl tmp-obj) obj)
		    ,(new-node Vassig (new-node Decl tmp-f) field)
		    ,(new-node Vassig
			  (new-node Decl tmp-return)
			  (new-node Access
			       (new-node Var-ref tmp-obj)
			       (new-node Var-ref tmp-f)))
		    ,(new-node Accsig
			  (new-node Access
			       (new-node Var-ref tmp-obj)
			       (new-node Var-ref tmp-f))
			  (new-node Binary
			       (new-node Var-ref tmp-return)
			       (new-node Var-ref
				    (if (eq? op '++)
					'+
					'-))
			       ;; TODO: number as string
			       (new-node Number "1")))
		    ,(new-node Var-ref tmp-return))))
	  (let ((tmp-return (gensym 'tmp))
		(id expr.id))
	     (new-node Sequence
		  `(,(new-node Vassig (new-node Decl tmp-return) expr)
		    ,(new-node Vassig
			  (new-node Var-ref id)
			  (new-node Binary
			       (new-node Var-ref id)
			       (new-node Var-ref (if (eq? op '++) '+ '-))
			       (new-node Number "1")))
		    ,(new-node Var-ref tmp-return)))))))
