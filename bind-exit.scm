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
	     (tree.traverse!)
	     ;; deprecate usage of replaced classes
	     (deprecate! Labelled)
	     (deprecate! Continue)
	     (deprecate! Break)
	     (deprecate! Return)))

(define-pmethod (Node-intro!)
   (this.traverse0!))

(define-pmethod (Labelled-intro!)
   (new Bind-exit this.label (this.body.traverse!)))

(define-pmethod (Continue/Break-intro!)
   (new Bind-exit-invoc this.label (new Undefined)))

(define-pmethod (Return-intro!)
   (new Bind-exit-invoc this.label this.expr))

(define (bind-exit-removal! tree)
   (verbose "  removal (optim)")
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
	     (set! Node.proto.default-traverse-value #f)
	     (tree.traverse #f)))

(define *throw-const* (cons 'throw 'throw))

(define-pmethod (Node-hoist sequence)
   (this.traverse1 #f)
   #f)

(define-pmethod (Begin-hoist sequence)
   (let loop ((els this.els))
      (if (null? els)
	  #f
	  (let ((interrupted-by ((car els).traverse (if (null? (cdr els))
							#f
							(cdr els)))))
	     (if interrupted-by
		 (begin
		    (set-cdr! els '())
		    interrupted-by)
		 (loop (cdr els)))))))

(define-pmethod (If-hoist sequence)
   (let* ((else-interrupted (this.else.traverse #f))
	  (then-interrupted (this.then.traverse (and else-interrupted
						     sequence))))
      (cond
	 ((and then-interrupted else-interrupted)
	  ;; we don't care if some elements are duplicated.
	  (append! then-interrupted else-interrupted))
	 (then-interrupted
	  (if sequence
	      (set! this.else (new Sequence (cons this.else sequence))))
	  then-interrupted)
	 (else-interrupted
	  (if sequence
	      (set! this.then (new Sequence (cons this.then sequence))))
	  then-interrupted)
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
			       Fun
			       Bind-exit
			       Bind-exit-invoc)
	    (tree.traverse! #f)))

(define-pmethod (Node-remove! enclosing)
   (this.traverse1! enclosing))

(define-pmethod (Begin-remove! enclosing)
   (if enclosing
       (let loop ((els this.els))
	  (cond
	     ((null? els)
	      (warning "Begin-remove! Bind-exits" "Begin without elements" #f)
	      (new NOP))
	     ((null? (cdr els))
	      (set-car! els ((car els).traverse! enclosing))
	      this)
	     (else
	      (set-car! els ((car els).traverse! #f))
	      (loop (cdr els)))))
       (this.traverse1! enclosing)))

(define-pmethod (Fun-remove! enclosing)
   (this.traverse1! #f))

(define-pmethod (Bind-exit-remove! enclosing)
   (let* ((label this.label)
	  (new-body (this.body.traverse! label)))
      (if (not label.used?)
	  new-body
	  (begin
	     (set! this.body new-body)
	     (delete! label.used?)
	     this))))

(define-pmethod (Bind-exit-invoc-remove! enclosing)
   (this.traverse1! #f)
   (if (eq? enclosing this.label)
       this.expr
       (begin
	  (set! this.label.used? #t)
	  this)))
