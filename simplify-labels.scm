(module simplify-labels
   (import walk
	   nodes
	   label
	   verbose)
   (export (simplify-labels! tree::Program)))

;; combine nested labelled-statements.
;; they should (in theory) share the same label.
(define (simplify-labels! tree)
   (verbose " simplify labels")
   (simplify tree #f))

(define-nmethod (Node.simplify)
   (default-walk this))

(define-nmethod (Labelled.simplify)
   (with-access::Labelled this (body label)
      (if (Labelled? body)
	  (let ((this-label label)
		(body-label (Labelled-label body)))
	     [assert (this-label body-label) (eq? this-label body-label)]
	     (set! body (Labelled-body body))
	     (walk this))
	  (default-walk this))))
