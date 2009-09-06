(module fun-bindings
   (include "protobject.sch")
   (include "tools.sch")
   (include "nodes.sch")
   (option (loadq "protobject-eval.sch"))
   (import protobject
	   nodes
	   verbose)
   (export (fun-bindings! tree::pobject)))

;; move fun-bindings (declarations) to beginning of functions.
;; order matters. (do not inverse declarations)

(define (fun-bindings! tree::pobject)
   (verbose "fun-bindings")
   (overload traverse! fb! (Node
			    Scope
			    Named-fun
			    Fun-binding)
	     (tree.traverse! #f)))

(define-pmethod (Node-fb! scope)
   (this.traverse1! scope))

(define-pmethod (Scope-fb! scope)
   (set! this.rev-fun-bindings '())
   (let* ((new-body (this.body.traverse! this))
	  (bindings (reverse! this.rev-fun-bindings)))
      (delete! this.rev-fun-bindings)
      (if (null? bindings)
	  (set! this.body new-body)
	  (set! this.body (new-node Block (append! bindings (list new-body)))))
      this))

(define-pmethod (Named-fun-fb! scope)
   (this.traverse1! scope))

(define-pmethod (Fun-binding-fb! scope)
   (this.traverse1! scope)
   (set! scope.rev-fun-bindings (cons this scope.rev-fun-bindings))
   (new-node NOP))
