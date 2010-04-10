(module simplify
   (include "protobject.sch")
   (include "nodes.sch")
   (option (loadq "protobject-eval.sch"))
   (import protobject
	   nodes
	   verbose)
   (export (simplify! tree::pobject)
	   *integrate-Var-decl-lists*))

(define *integrate-Var-decl-lists* #t)

;; - nested Begins are merged into one.
;; - Begins with only one element are discarded.
;; - NOPs within a Begin are discarded.
(define (simplify! tree)
   (verbose "simplify")
   (overload traverse! simplify! (Node
				  Begin
				  Var-decl-list)
	     (tree.traverse!)))

(define-pmethod (Node-simplify!)
   (this.traverse0!))

(define-pmethod (Begin-simplify!)
   ;; integrate Begins,
   ;; remove NOPs.
   ;; and remove dead code after breaks, continues and returns.
   (define (simplify-els els)
      (let loop ((els els)
		 (rev-result '()))
	 (if (null? els)
	     (reverse! rev-result)
	     (let ((fst (car els))
		   (rest (cdr els)))
		(cond
		   ((and (inherits-from? fst (node 'Begin))
			 (or *integrate-Var-decl-lists*
			     (not (inherits-from? fst (node 'Var-decl-list)))))
		    ;; "integrate" nested Begin
		   (let liip ((nested-els fst.els)
			      (rev-result rev-result))
		      ;; nested-els has at least 2 els. Otherwise the Begin
		      ;; would have been discarded.
		      (if (null? (cdr nested-els))
			  ;; Add the last element of the nested begin to the
			  ;; 'els' (and not to the result). If it is a break or
			  ;; return we treat it again, and can discard dead
			  ;; code.
			  (loop (cons (car nested-els) rest)
				rev-result)
			  (liip (cdr nested-els)
				(cons (car nested-els) rev-result)))))
		   ;; discard NOPs
		   ((inherits-from? fst (node 'NOP))
		    (loop rest rev-result))
		   ;; remove dead code
		   ((or (inherits-from? fst (node 'Break))
			(inherits-from? fst (node 'Continue))
			(inherits-from? fst (node 'Return)))
		    (loop '() (cons fst rev-result)))
		   (else
		    (loop rest (cons fst rev-result))))))))
   (this.traverse0!)
   (let ((els (simplify-els this.els)))
      (cond
	 ((null? els)
	  ;; probably happened, cause we removed a list of NOPs.
	  (new-node NOP))
	 ((null? (cdr els)) ;; discard Begin
	  ((car els).traverse!))
	 (else
	  (set! this.els els)
	  this))))

(define-pmethod (Var-decl-list-simplify!)
   (this.traverse0!))
