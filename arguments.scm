(module arguments
   (include "protobject.sch")
   (include "nodes.sch")
   (option (loadq "protobject-eval.sch"))
   (import protobject
	   nodes
	   verbose)
   (export (arguments tree::pobject)))

(define (arguments tree)
   (verbose "arguments")
   (overload traverse arguments (Node
				 Fun
				 Var-ref)
	     (tree.traverse)))

(define-pmethod (Node-arguments)
   (this.traverse0))

(define-pmethod (Fun-arguments)
   (when (and this.eval?
	      ;; don't add it, if the var is shadowed.
	      (eq? (hashtable-get this.locals-table 'arguments)
		   this.arguments-decl.var))
      (set! this.arguments-decl.var.arguments-used? #t))
   ;; don't go into arguments-decl (nor args)
   (this.body.traverse))

(define-pmethod (Var-ref-arguments)
   (when this.var.arguments?
      (set! this.var.arguments-used? #t)))
