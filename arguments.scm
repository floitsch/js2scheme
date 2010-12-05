(module arguments
   (import walk
	   nodes
	   verbose)
   (export (arguments tree::Program)))

(define (arguments tree)
   (verbose "arguments")
   (args tree #f))

(define-nmethod (Node.args)
   (default-walk this))

(define-nmethod (Fun.args)
   (with-access::Fun this (eval? arguments-decl locals-table body)
      (when (and eval?
		 ;; don't add it, if the var is shadowed.
		 (eq? (hashtable-get locals-table 'arguments)
		      (Ref-var arguments-decl)))
	 (with-access::Var (Ref-var arguments-decl) (arguments-used?)
	    (set! arguments-used? #t)))
      ;; don't go into arguments-decl (nor args)
      (walk body)))

(define-nmethod (Ref.args)
   (with-access::Ref this (var)
      (with-access::Var var (arguments? arguments-used?)
	 (when arguments?
	    (set! arguments-used? #t)))))
