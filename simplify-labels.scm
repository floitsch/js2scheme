(module simplify-labels
   (include "protobject.sch")
   (include "nodes.sch")
   (option (loadq "protobject-eval.sch"))
   (import protobject
	   nodes
	   label
	   verbose)
   (export (simplify-labels! tree::pobject)))

;; combine nested labelled-statements.
;; they should (in theory) share the same label.
(define (simplify-labels! tree)
   (verbose " simplify labels")
   (overload traverse simplify (Node
				Labelled)
	     (tree.traverse))
   tree)

(define-pmethod (Node-simplify)
   (this.traverse0))

(define-pmethod (Labelled-simplify)
   (if (inherits-from? this.body Labelled)
       (let ((this-label this.label)
	     (body-label this.body.label))
	  [assert (this-label body-label) (eq? this-label body-label)]
	  (set! this.body this.body.body)
	  (this.traverse))
       (this.traverse0)))
