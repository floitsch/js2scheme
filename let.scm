(module let
   (include "protobject.sch")
   (include "nodes.sch")
   (include "tools.sch")
   (option (loadq "protobject-eval.sch"))
   (import protobject
	   verbose
	   nodes
	   var)
   (export (let-intro! tree::pobject)))

;; adds let*-nodes.

(define (let-intro! tree)
   (verbose "let-intro!")
   (reverse-liveness tree)
   (add-nodes! tree))

;; at this moment of the compilation all (non-global) vars have a begin-nesting
;; and a end-nesting stack.
;; this pass reverses this, and assigns to each respective node the
;; variables that start to be live at this node (and the same for the end).
;; If the node is a 'begin' the start/end is assigned to the respective
;; subnodes.
;; Ex:
;; suppose a variable x is live in the 'test' and 'then' parts of an 'if'. Then
;; the 'if' node records the beginning and end of the liveness of x.
;; On the other hand. suppose x is used in a begin:
;; (begin
;;    n1
;;    n2 uses x
;;    n3
;     n4 uses x
;; )
;; then we make an exception, and record x's liveness in n2 and n4 (instead of
;; the surrounding begin).
(define (reverse-liveness tree)
   (verbose " let-liveness")
   (overload traverse rev-live (Node
				Program
				Fun
				Decl
				Param
				This-decl
				Arguments-decl)
	     (tree.traverse #f)))

;; 'surrounding-fun' is maybe not the correct name (as it could be the "Program too").
(define-pmethod (Node-rev-live surrounding-fun)
   (this.traverse1 surrounding-fun))

(define-pmethod (Program-rev-live surrounding-fun)
   (this.traverse1 this))
(define-pmethod (Fun-rev-live surrounding-fun)
   (this.traverse1 this))

(define-pmethod (Decl-rev-live surrounding-fun)
   (define (mark-live-begin node var)
      (set! node.live-begins (cons var (or node.live-begins '()))))
   (define (mark-live-end node var)
      (set! node.live-ends (cons var (or node.live-ends '()))))

   (define (transitive-with-var var)
      (if (inherits-from? var (node 'With-var))
	  (transitive-with-var var.intercepted)
	  var))
      
   (let* ((var (transitive-with-var this.var))
	  (begin-stack var.live-begin-stack)
	  (end-stack var.live-end-stack))
      (cond
	 (var.global? ;; we are printing those directly in Program-out.
	  'do-nothing)
	 (var.no-let? ;; in particular Decl-With variables.
	  'do-nothing)
	 ((not begin-stack) ;; imported, runtime or whatever...
	  'do-nothing)
	 (var.escapes?
	  (mark-live-begin surrounding-fun.body var)
	  (mark-live-end surrounding-fun.body var))
	 ((inherits-from? (cadr begin-stack) (node 'Begin))
	  (mark-live-begin (car begin-stack) var)
	  (mark-live-end (car end-stack) var))
	 (else
	  (mark-live-begin (cadr begin-stack) var)
	  (mark-live-end (cadr end-stack) var)
	  [assert (begin-stack end-stack)
		  (eq? (cadr begin-stack) (cadr end-stack))]
	  ))
      (delete! var.live-begin-stack)
      (delete! var.live-end-stack)))

(define-pmethod (Param-rev-live surrounding-fun)
   'do-nothing)
(define-pmethod (This-decl-rev-live surrounding-fun)
   'do-nothing)
(define-pmethod (Arguments-decl-rev-live surrounding-fun)
   'do-nothing)

(define (add-nodes! tree)
   (verbose " add-let-nodes")
   (overload traverse! intro! (Node
			       Begin
			       Let*)
	     (tree.traverse!)))

(define (make-let* assigs body)
   (for-each (lambda (assig)
		(set! assig.lhs.var.in-let? #t))
	     assigs)
   (new-node Let* assigs body))

(define (intro! n)
   (delete! n.live-ends)
   (let ((lives n.live-begins))
      (if lives
	  (let ((v (car lives)))
	     (if (null? (cdr lives))
		 (delete! n.live-begins)
		 (set! n.live-begins (cdr lives)))
	     ((make-let* (list (v.assig (new-node Undefined))) n).traverse!))
	  (n.traverse0!))))
   
(define-pmethod (Node-intro!)
   (intro! this))

(define-pmethod (Let*-intro!)
   (this.traverse0!)
   (when (inherits-from? this.body (node 'Let*))
      (set! this.assigs (append! this.vassigs this.body.vassigs))
      (set! this.body this.body.body))
   this)
       
(define-pmethod (Begin-intro!)
   (delete! this.live-ends)
   (if this.live-begins
       (intro! this)
       (let loop ((els this.els))
	  (if (null? els)
	      this
	      (let* ((el (car els))
		     (live-begins (or el.live-begins '()))
		     (live-ends (or el.live-ends '()))
		     (long-v (any (lambda (v)
				     (and (not (memq v live-ends))
					  v))
				  live-begins))
		     (filtered-l (filter!
				  (lambda (v)
				     (not (eq? v long-v)))
				  live-begins)))
		 (if (null? filtered-l)
		     (delete! el.live-begins)
		     (set! el.live-begins filtered-l))
		 (cond
		    ((and long-v
			  (inherits-from? el (node 'Vassig))
			  (eq? el.lhs.var long-v))
		     (let ((let-n (make-let* (list el)
					     (new-node Sequence (cdr els)))))
			(set-car! els (let-n.traverse!))
			(set-cdr! els '())))
		    (long-v
		     (let ((let-n (make-let*
				   `(,(long-v.assig (new-node Undefined)))
				   (new-node Sequence
					     (cons (car els) (cdr els))))))
			(set-car! els (let-n.traverse!))
			(set-cdr! els '())))
		    (else
		     (set-car! els (el.traverse!))))
		 (loop (cdr els)))))))
