(module fun-bindings
   (include "protobject.sch")
   (include "tools.sch")
   (include "nodes.sch")
   (option (loadq "protobject-eval.sch"))
   (import protobject
	   nodes
	   verbose)
   (export (fun-bindings! tree::pobject)))

;; move fun-bindings (declarations) to beginning of functions

(define (fun-bindings! tree::pobject)
   (verbose "fun-bindings")
   (overload traverse! fb! (Node
			    Scope
			    Named-fun
			    Fun-binding)
	     (tree.traverse! #f)))

(define-pmethod (Node-fb! ht)
   (this.traverse1! ht))

(define-pmethod (Scope-fb! ht)
   (let* ((bindings-ht (make-eq-hashtable))
	  (new-body (this.body.traverse! bindings-ht))
	  (bindings (hashtable-key-list bindings-ht)))
      (if (not (null? bindings))
	  (set! this.body (new Block (append! bindings (list new-body)))))
      this))

(define-pmethod (Named-fun-fb! ht)
   (this.traverse1! ht))

(define-pmethod (Fun-binding-fb! ht)
   (this.traverse1! ht)
   (hashtable-put! ht this #t)
   (new NOP))
