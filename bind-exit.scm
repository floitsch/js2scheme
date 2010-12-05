(module bind-exit
   (import walk
	   nodes
	   label
	   simplify
	   verbose)
   (export (bind-exit! tree::Program)))

(define (bind-exit! tree)
   (verbose " bind-exit")
   (bind-exit-introduction! tree)
   (bind-exit-removal! tree)
   )

;; replace Labelled with Bind-exits.
;; replace Continue Break and Returns by respective Bind-exit-invocations
(define (bind-exit-introduction! tree)
   (verbose "  intro")
   (intro! tree #f))

(define-nmethod (Node.intro!)
   (default-walk! this))

(define-nmethod (Labelled.intro!)
   (with-access::Labelled this (label body)
      (instantiate::Bind-Exit
	 (label label)
	 (body (walk! body)))))

(define-nmethod (Continue.intro!)
   (with-access::Continue this (label)
      (instantiate::Bind-Exit-Invoc
	 (label label)
	 (expr (new-undefined)))))

(define-nmethod (Break.intro!)
   (with-access::Break this (label)
      (instantiate::Bind-Exit-Invoc
	 (label label)
	 (expr (new-undefined)))))

(define-nmethod (Return.intro!)
   (with-access::Return this (label expr)
      (instantiate::Bind-Exit-Invoc
	 (label label)
	 (expr expr))))

(define (bind-exit-removal! tree)
   (verbose "  removal (optim)")
   (remove! tree #f '()) ;; start by removing all labels that are not used at all.
   (simplify tree) ;; we don't want nested begins
   (hoist tree #f #f)
   (remove! tree #f '()))

(define *throw-const* (cons 'throw 'throw))

;; hoist:
;; only applies to 'if's: if one branch interrupts
;; (break/continue/return/throw), the elements following the 'if' are added to
;; the alternative branch.

;; sequence: either #f, or a list of nodes. The list are the elements of the
;; surrounding sequence stripped of the already processed elements.
;; It still contains the current node in it. As such 'If's can simply set the
;; 'cdr' to '() to remove the following nodes.
;;
;; all methods return either #f or a list of labels. If a list of labels is
;; returned, then all sub-branches terminate due to one of these labels.
;; If a Bind-Exit with one of these labels is reached at least one branch
;; continues, and we can't continue removing dead-code.

(define-nmethod (Node.hoist sequence)
   (error #f
	  "Internal Error. Forgot node type"
	  (class-name (object-class this))))

(define-nmethod (Ref.hoist sequence) (default-walk this #f) #f)
(define-nmethod (Scope.hoist sequence) (walk (Scope-body this) #f))
(define-nmethod (Fun.hoist sequence) (default-walk this #f) #f)
(define-nmethod (NOP.hoist sequence) (default-walk this #f) #f)
;; If a loop is not taken, then it might not be interrupted.
;; A Do-loop always executes its body, but we don't care for this case.
(define-nmethod (Loop.hoist sequence) (default-walk this #f) #f)
(define-nmethod (Decl-Intercept.hoist sequence) (default-walk this #f) #f)
;; Labelled just have been removed.
(define-nmethod (Assig.hoist sequence) (default-walk this #f) #f)
(define-nmethod (Call.hoist sequence)  (default-walk this #f) #f)
(define-nmethod (Access.hoist sequence) (default-walk this #f) #f)
(define-nmethod (Literal.hoist sequence) (default-walk this #f) #f)
(define-nmethod (Array.hoist sequence) (default-walk this #f) #f)
(define-nmethod (Array-Element.hoist sequence) (default-walk this #f) #f)
(define-nmethod (Property-Init.hoist sequence) (default-walk this #f) #f)
(define-nmethod (Reg-Exp.hoist sequence) (default-walk this #f) #f)

(define (Begin-hoist this::Begin walk sequence skip-first?)
   (with-access::Begin this (els)
      (let loop ((els (if skip-first?
			  (cdr els)
			  els)))
	 (if (null? els)
	     #f
	     (let ((interrupted-by (walk (car els)  (if (null? (cdr els))
							#f
							els))))
		(if interrupted-by
		    (begin
		       (set-cdr! els '())
		       interrupted-by)
		    (loop (cdr els))))))))
   
;; the Lskip-first? indicates that the first element in the node has already
;; been processed and can be skipped.
(define-nmethod (Begin.hoist sequence)
   (Begin-hoist this walk sequence #f))

(define-nmethod (If.hoist sequence)
   (with-access::If this (then else)
      (let* ((else-interrupted (walk else #f))
	     ;; if else is interrupted, then we can directly pass
	     ;; sequence to the then-branch.
	     (then-interrupted (walk then (and else-interrupted
					       sequence))))
	 (cond
	    ((and then-interrupted else-interrupted)
	     ;; we don't care if some elements are duplicated.
	     (append! then-interrupted else-interrupted))
	    (then-interrupted
	     (if sequence
		 (begin
		    (set! else (instantiate::Sequence
				  (els (cons else (cdr sequence)))))
		    ;; truncate the sequence-list.
		    (set-cdr! sequence '())
		    ;; skip first, which has already been processed:
		    (let ((new-else-interrupted (Begin-hoist else walk #f #t)))
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
		    (set! then (instantiate::Sequence
				  (els (cons then (cdr sequence)))))
		    ;; truncate the sequence-list.
		    (set-cdr! sequence '())
		    ;; skip first, which has already been processed:
		    (let ((new-then-interrupted (Begin-hoist then walk #f #t)))
		       (if new-then-interrupted
			   (append! new-then-interrupted else-interrupted)
			   #f)))
		 #f))
	    (else
	     #f)))))

(define-nmethod (Throw.hoist sequence)
   (list *throw-const*))

(define-nmethod (Bind-Exit-Invoc.hoist sequence)
   (with-access::Bind-Exit-Invoc this (label)
      (list label)))

(define-nmethod (Bind-Exit.hoist sequence)
   (with-access::Bind-Exit this (body label)
      (let ((interrupted-by (walk body #f)))
	 (and interrupted-by
	      (if (memq label interrupted-by)
		  #f
		  interrupted-by)))))

(define-nmethod (With.hoist sequence)
   (walk (With-obj this) #f)
   (walk (With-body this) #f)) ;; Return the result of the body.

(define-nmethod (Try.hoist sequence)
   (with-access::Try this (body)
      (let ((interrupted-by (walk body #f)))
	 (and interrupted-by
	      (if (memq *throw-const* interrupted-by)
		  #f
		  interrupted-by)))))


;; every node receives a list of labels that are directly surrounding the
;; node. If we reach a throw/return, ... and the label is directly surrounding
;; we don't need to invoke it.
;;
;; by default every node cuts of the enclosing bind-exit. If I forget a node it
;; won't damage now.
(define-nmethod (Node.remove! enclosing-labels)
   (default-walk! this '()))

(define-nmethod (Begin.remove! enclosing-labels)
   (with-access::Begin this (els)
      (if (not (null? enclosing-labels))
	  (let loop ((els els))
	     (cond
		((null? els)
		 (warning "Begin-remove! Bind-exits" "Begin without elements" #f)
		 (instantiate::NOP))
		((null? (cdr els))
		 (set-car! els (walk! (car els) enclosing-labels))
		 this)
		(else
		 (set-car! els (walk! (car els) '()))
		 (loop (cdr els)))))
	  (default-walk! this enclosing-labels))))

(define-nmethod (If.remove! enclosing-labels)
   (with-access::If this (test then else)
      (set! test (walk! test '()))
      (set! then (walk! then enclosing-labels))
      (set! else (walk! else enclosing-labels))
      this))

(define-nmethod (With.remove! enclosing-labels)
   (with-access::With this (obj body)
      (set! obj (walk! obj '()))
      (set! body (walk! body enclosing-labels))
      this))

(define-nmethod (Try.remove! enclosing-labels)
   (with-access::Try this (body catch finally)
      (set! body (walk! body enclosing-labels))
      (when catch (set! catch (walk! catch '())))
      (when finally (set! finally (walk! finally '())))
      this))

(define-nmethod (Bind-Exit.remove! enclosing-labels)
   (with-access::Bind-Exit this (label body)
      (let ((new-body (walk! body (cons label enclosing-labels))))
	 (with-access::Label label (used?)
	    (if (not used?)
		new-body
		(begin
		   (set! body new-body)
		   ;; (delete! label.used?)
		   this))))))

(define-nmethod (Bind-Exit-Invoc.remove! enclosing-labels)
   (default-walk! this '())
   (with-access::Bind-Exit-Invoc this (label expr)
      (if (memq label enclosing-labels)
	  expr
	  (with-access::Label label (used?)
	     (set! used? #t)
	     this))))
