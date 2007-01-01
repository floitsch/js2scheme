(module expand2
   (include "protobject.sch")
   (include "nodes.sch")
   (option (loadq "protobject-eval.sch"))
   (import protobject
	   verbose
	   nodes)
   (export (expand2! tree::pobject)))

;; - replace Named-Fun with Vassig
(define (expand2! tree)
   (verbose " expand2")
   (overload traverse! expand! (Node
				Named-fun)
	     (tree.traverse!)))

(define-pmethod (Node-expand!)
   (this.traverse0!))

(define-pmethod (Named-fun-expand!)
   (this.traverse0!)
   (new Vassig this.decl this.fun))
