(module expand3
   (include "protobject.sch")
   (include "nodes.sch")
   (option (loadq "protobject-eval.sch"))
   (import protobject
	   label
	   verbose
	   nodes)
   (export (expand3! tree::pobject)))

;; - replace Named-Fun with Vassig
(define (expand3! tree)
   (verbose " expand3")
   (overload traverse! expand! (Node
				For
				While
				(Do While-expand!) ; same as for While
				(For-in While-expand!)) ; same as for While
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

