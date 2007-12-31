(module bind-exit
   (include "protobject.sch")
   (include "nodes.sch")
   (option (loadq "protobject-eval.sch"))
   (import protobject
	   nodes
	   label
	   simplify
	   verbose)
   (export (bind-exit! tree::pobject)))

(define (bind-exit! tree)
   (verbose " bind-exit")
   (bind-exit-introduction! tree)
   (bind-exit-removal! tree)
   )

;; replace Labelled with Bind-exits.
;; replace Continue Break and Returns by respective Bind-exit-invocations
(define (bind-exit-introduction! tree)
   (verbose "  intro")
   (overload traverse! intro! (Node
			      Labelled
			      (Continue Continue/Break-intro!)
			      (Break Continue/Break-intro!)
			      Return)
	     (tree.traverse!)))

(define-pmethod (Node-intro!)
   (this.traverse0!))

(define-pmethod (Labelled-intro!)
   (new-node Bind-exit this.label (this.body.traverse!)))

(define-pmethod (Continue/Break-intro!)
   (new-node Bind-exit-invoc this.label (new-node Undefined)))

(define-pmethod (Return-intro!)
   (new-node Bind-exit-invoc this.label this.expr))

(define (bind-exit-removal! tree)
   (verbose "  removal (optim)")
   (remove! tree) ;; start by removing all labels that are not used at all.
   (simplify! tree) ;; we don't want nested begins
   (hoist! tree)
   (remove! tree))

;; only applies to 'if's: if one branch interrupts
;; (break/continue/return/throw), the elements following the 'if' are added to
;; the alternative branch.
(define (hoist! tree)
   (overload traverse hoist (Node
			     Begin
			     If
			     Bind-exit-invoc
			     Throw
			     Bind-exit
			     Try)
	     (set! (node 'Node).proto.default-traverse-value #f)
	     (tree.traverse #f)))

(define *throw-const* (cons 'throw 'throw))

;; sequence: either #f, or a list of nodes. The list are the elements of the
;; surrounding sequence stripped of the already processed elements.
;; It still contains the current node in it. As such 'If's can simply set the
;; 'cdr' to '() to remove the following nodes.
;;
;; all methods return either #f or a list of labels. If a list of labels is
;; returned, then all sub-branches terminate due to one of these labels.
;; If a Bind-exit with one of these labels is reached at least one branch
;; continues, and we can't continue removing dead-code.

(define-pmethod (Node-hoist sequence)
   (this.traverse1 #f)
   #f)

;; the Lskip-first? indicates that the first element in the node has already
;; been processed and can be skipped.
(define-pmethod (Begin-hoist sequence . Lskip-first?)
   (let loop ((els (if (null? Lskip-first?)
		       this.els
		       (cdr this.els))))
      (if (null? els)
	  #f
	  (let ((interrupted-by ((car els).traverse (if (null? (cdr els))
							#f
							els))))
	     (if interrupted-by
		 (begin
		    (set-cdr! els '())
		    interrupted-by)
		 (loop (cdr els)))))))

(define-pmethod (If-hoist sequence)
   (let* ((else-interrupted (this.else.traverse #f))
	  ;; if else is interrupted, then we can directly pass
	  ;; sequence to the then-branch.
	  (then-interrupted (this.then.traverse (and else-interrupted
						     sequence))))
      
      (cond
	 ((and then-interrupted else-interrupted)
	  ;; we don't care if some elements are duplicated.
	  (append! then-interrupted else-interrupted))
	 (then-interrupted
	  (if sequence
	      (begin
		 (set! this.else (new-node Sequence
					   (cons this.else (cdr sequence))))
		 ;; truncate the sequence-list.
		 (set-cdr! sequence '())
		 ;; skip first, which has already been processed:
		 (let ((new-else-interrupted (this.else.traverse #f #t)))
		    (if new-else-interrupted
			(append! then-interrupted new-else-interrupted)
			#f)))
	      #f))
	 (else-interrupted
	  (if sequence
	      (begin
		 ;; as we passed 'sequence' to the then-branch, we might have
		 ;; already truncated 'sequence'. Doesn't matter though, as in
		 ;; this case the new Sequence will just have one element.
		 (set! this.then (new-node Sequence
					   (cons this.then (cdr sequence))))
		 ;; truncate the sequence-list.
		 (set-cdr! sequence '())
		 ;; skip first, which has already been processed:
		 (let ((new-then-interrupted (this.then.traverse #f #t)))
		    (if new-then-interrupted
			(append! new-then-interrupted else-interrupted)
			#f)))
	      #f))
	 (else
	  #f))))

(define-pmethod (Throw-hoist sequence)
   (list *throw-const*))

(define-pmethod (Bind-exit-invoc-hoist sequence)
   (list this.label))

(define-pmethod (Bind-exit-hoist sequence)
   (let ((interrupted-by (this.body.traverse #f)))
      (and interrupted-by
	   (if (memq this.label interrupted-by)
	       #f
	       interrupted-by))))

(define-pmethod (Try-hoist sequence)
   (let ((interrupted-by (this.body.traverse #f)))
      (and interrupted-by
	   (if (memq *throw-const* interrupted-by)
	       #f
	       interrupted-by))))

(define (remove! tree)
  (overload traverse! remove! (Node
			       Begin
			       If
			       With
			       Try
			       Bind-exit
			       Bind-exit-invoc)
	    (tree.traverse! '())))

;; every node receives a list of labels, that are directly surrounding the
;; node. If we reach a throw/return, ... and the label is directly surrounding
;; we don't need to invoke it.
;;
;; by default every node cuts of the enclosing bind-exit. If I forget a node it
;; won't damage now.
(define-pmethod (Node-remove! enclosing-labels)
   (this.traverse1! '()))

(define-pmethod (Begin-remove! enclosing-labels)
   (if (not (null? enclosing-labels))
       (let loop ((els this.els))
	  (cond
	     ((null? els)
	      (warning "Begin-remove! Bind-exits" "Begin without elements" #f)
	      (new-node NOP))
	     ((null? (cdr els))
	      (set-car! els ((car els).traverse! enclosing-labels))
	      this)
	     (else
	      (set-car! els ((car els).traverse! '()))
	      (loop (cdr els)))))
       (this.traverse1! enclosing-labels)))

(define-pmethod (If-remove! enclosing-labels)
   (set! this.test (this.test.traverse! '()))
   (set! this.then (this.then.traverse! enclosing-labels))
   (set! this.else (this.else.traverse! enclosing-labels))
   this)

(define-pmethod (With-remove! enclosing-labels)
   (set! this.obj (this.obj.traverse! '()))
   (set! this.body (this.body.traverse! enclosing-labels))
   this)

(define-pmethod (Try-remove! enclosing-labels)
   (set! this.body (this.body.traverse! enclosing-labels))
   (when this.catch (set! this.catch (this.catch.traverse! '())))
   (when this.finally (set! this.finally (this.finally.traverse! '())))
   this)

(define-pmethod (Bind-exit-remove! enclosing-labels)
   (let* ((label this.label)
	  (new-body (this.body.traverse! (cons label enclosing-labels))))
      (if (not label.used?)
	  new-body
	  (begin
	     (set! this.body new-body)
	     (delete! label.used?)
	     this))))

(define-pmethod (Bind-exit-invoc-remove! enclosing-labels)
   (this.traverse1! '())
   (if (memq this.label enclosing-labels)
       this.expr
       (begin
	  (set! this.label.used? #t)
	  this)))
