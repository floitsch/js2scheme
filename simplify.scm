(module simplify
   (import nodes
	   verbose
	   walk)
   (export (simplify tree::Program)
	   *integrate-Var-decl-lists*))

(define *integrate-Var-decl-lists* #t)

;; - nested Begins are merged into one.
;; - Begins with only one element are discarded.
;; - NOPs within a Begin are discarded.
(define (simplify tree)
   (verbose "simplify")
   (simplify! tree #f))

(define-nmethod (Node.simplify!)
   (default-walk! this))

(define-nmethod (Begin.simplify!)
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
		   ((and (Begin? fst)
			 (or *integrate-Var-decl-lists*
			     (not (Var-Decl-List? fst))))
		    ;; "integrate" nested Begin
		   (let liip ((nested-els (Begin-els fst))
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
		   ((NOP? fst)
		    (loop rest rev-result))
		   ;; remove dead code
		   ((or (Break? fst)
			(Continue? fst)
			(Return? fst))
		    (loop '() (cons fst rev-result)))
		   (else
		    (loop rest (cons fst rev-result))))))))
   (default-walk! this)
   (with-access::Begin this (els)
      (set! els (simplify-els els))
      (cond
	 ((null? els)
	  ;; probably happened, cause we removed a list of NOPs.
	  (instantiate::NOP))
	 ((null? (cdr els)) ;; discard Begin
	  (car els))
	 (else
	  this))))

(define-nmethod (Var-Decl-List.simplify!)
   (default-walk! this))
