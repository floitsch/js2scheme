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
   (define (simplify-els! els)
      (let loop ((els els)
		 (new-head '()))
	 (define (continue-loop rest can-els-be-head?)
	    (if (and (null? new-head)
		     can-els-be-head?)
		(loop rest els) ;; els becomes new head
		(loop rest new-head)))
	 (if (null? els)
	     new-head
	     (let ((fst ((car els).traverse!))
		   (rest (cdr els)))
		(cond
		   ((and (inherits-from? fst Begin)
			 (or *integrate-Var-decl-lists*
			     (not (inherits-from? fst Var-decl-list))))
		    ;; "integrate" nested Begin
		    (let ((other-els fst.els))
		       ;; other-els has at least 2 els. otherwise the Begin
		       ;; would have been discarded
		       (set-car! els (car other-els))
		       (set-cdr! els (cdr other-els))
		       (let ((lp (last-pair els)))
			  ;; append rest to the inserted els of the nested begin.
			  (set-cdr! lp rest)
			  ;; "revisit" last element of nested begin, in case it
			  ;; was a break/continue or return.
			  (continue-loop lp #t))))
		   ;; discard NOPs
		   ((inherits-from? fst NOP)
		    (loop rest new-head))
		   ;; remove dead code
		   ((or (inherits-from? fst Break)
			(inherits-from? fst Continue)
			(inherits-from? fst Return))
		    (continue-loop '() #t))
		   (else
		    (continue-loop rest #t)))))))
   (this.traverse0!)
   (let ((els (simplify-els! this.els)))
      (cond
	 ((null? els)
	  ;; probably happened, cause we removed a list of NOPs.
	  (new NOP))
	 ((null? (cdr els)) ;; discard Begin
	  ((car els).traverse!))
	 (else
	  (set! this.els els)
	  this))))

(define-pmethod (Var-decl-list-simplify!)
   (this.traverse0!))
