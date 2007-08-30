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
	  (if (inherits-from? expr Access)
	      (let* ((obj expr.obj)
		     (field expr.field)
		     (tmp-obj (gensym 'o))
		     (tmp-f (gensym 'f)))
		 (new Sequence
		      `(,(new Vassig (new Decl tmp-obj) obj)
			,(new Vassig (new Decl tmp-f) field)
			,(new Accsig
			      (new Access
				   (new Var-ref tmp-obj)
				   (new Var-ref tmp-f))
			      (new Binary
				   (new Access
					(new Var-ref tmp-obj)
					(new Var-ref tmp-f))
				   (if (eq? op '++)
				       (new Var-ref '+)
				       (new Var-ref '-))
				   ;; TODO: number as string
				   (new Number "1"))))))
	      (new Vassig
		   expr ;; a variable
		   (new Binary
			(new Var-ref expr.id)
			(new Var-ref (if (eq? op '++) '+ '-))
			;; TODO: number as string
			(new Number "1"))))
	  this)))

(define-pmethod (Postfix-expand!)
   (this.traverse0!)
   (let ((op this.op.id)
	 (expr (car this.args)))
      (if (inherits-from? expr Access)
	  (let* ((obj expr.obj)
		 (field expr.field)
		 (tmp-return (gensym 'tmp))
		 (tmp-obj (gensym 'o))
		 (tmp-f (gensym 'f)))
	     (new Sequence
		  `(,(new Vassig (new Decl tmp-obj) obj)
		    ,(new Vassig (new Decl tmp-f) field)
		    ,(new Vassig
			  (new Decl tmp-return)
			  (new Access
			       (new Var-ref tmp-obj)
			       (new Var-ref tmp-f)))
		    ,(new Accsig
			  (new Access
			       (new Var-ref tmp-obj)
			       (new Var-ref tmp-f))
			  (new Binary
			       (new Var-ref tmp-return)
			       (new Var-ref
				    (if (eq? op '++)
					'+
					'-))
			       ;; TODO: number as string
			       (new Number "1")))
		    ,(new Var-ref tmp-return))))
	  (let ((tmp-return (gensym 'tmp))
		(id expr.id))
	     (new Sequence
		  `(,(new Vassig (new Decl tmp-return) expr)
		    ,(new Vassig
			  (new Var-ref id)
			  (new Binary
			       (new Var-ref id)
			       (new Var-ref (if (eq? op '++) '+ '-))
			       (new Number "1")))
		    ,(new Var-ref tmp-return)))))))
