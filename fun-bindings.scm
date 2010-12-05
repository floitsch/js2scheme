(module fun-bindings
   (import nodes
	   walk
	   verbose)
   (export (fun-bindings! tree::Program)))

;; move fun-bindings (declarations) to beginning of functions.
;; order matters. (do not inverse declarations)

(define (fun-bindings! tree)
   (verbose "fun-bindings")
   (fb! tree #f #f))

(define-nmethod (Node.fb! rev-fun-container)
   (default-walk! this rev-fun-container))

(define-nmethod (Scope.fb! rev-fun-container)
   (with-access::Scope this (body)
      (let* ((container (list '*rev-fun-container*))
	     (new-body (walk! body container))
	     (bindings (reverse! (cdr container))))
	 (if (null? bindings)
	     (set! body new-body)
	     (set! body (instantiate::Block
			   (els (append! bindings (list new-body))))))
	 this)))

(define-nmethod (Fun-Binding.fb! rev-fun-container)
   (default-walk! this rev-fun-container)
   (set-cdr! rev-fun-container (cons this (cdr rev-fun-container)))
   (instantiate::NOP))
