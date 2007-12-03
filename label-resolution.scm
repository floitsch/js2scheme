(module label-resolution
   (include "protobject.sch")
   (include "nodes.sch")
   (option (loadq "protobject-eval.sch"))
   (import protobject
	   label
	   verbose
	   nodes)
   (export (label-resolution tree::pobject)))

(define (label-resolution tree)
   (verbose " label-resolution")
   (overload traverse label-res (Node
				 Program
				 Fun
				 Continue
				 Break
				 Return
				 Labelled
				 Loop
				 Switch-clause)
	     (tree.traverse #unspecified #unspecified)))

(define-pmethod (Node-label-res labels direct-enclosing)
   (this.traverse2 labels #f))

(define-pmethod (Program-label-res labels direct-enclosing)
   (this.traverse2 (cons (make-label-table)
			 (make-label-table))
		   #f))

(define-pmethod (Fun-label-res labels direct-enclosing)
   (let* ((break-labels (make-label-table))
	  (return-label (new-node Break-label)))
      (return-label.add-id! *return-label-id*)
      (label-set! break-labels *return-label-id* return-label)
      (set! this.return-label return-label)
      (this.traverse2 (cons break-labels (make-label-table)) #f)))
	     
(define-pmethod (Continue-label-res labels direct-enclosing)
   (let* ((continue-labels (cdr labels))
	  (label-id (or this.id *default-continue-label-id*))
	  (entry (label-get continue-labels label-id)))
      (if (not entry)
	  (error #f "Continue statement without matching label: " label-id)
	  (set! this.label entry))))
	  
(define-pmethod (Break-label-res labels direct-enclosing)
   (let* ((break-labels (car labels))
	  (label-id (or this.id *default-break-label-id*))
	  (entry (label-get break-labels label-id)))
      (if (not entry)
	  (error #f "Break statement without matching label: " label-id)
	  (set! this.label entry))))

(define-pmethod (Return-label-res labels direct-enclosing)
   (let* ((break-labels (car labels))
	  (label-id (or this.id *return-label-id*))
	  (entry (label-get break-labels label-id)))
      (if (not entry)
	  (error #f "Return statement at top-level." #f)
	  (set! this.label entry))))
	  
(define-pmethod (Labelled-label-res labels direct-enclosing)
   (let* ((break-labels (car labels))
	  (entry (label-get break-labels this.id)))
      (if entry
	  (error #f "Nested Label-statement with same id: " this.id)
	  (if direct-enclosing
	      (let ((enclosing-break-label (car direct-enclosing))
		    (enclosing-ids (cdr direct-enclosing)))
		 (enclosing-break-label.add-id! this.id)
		 (set! this.label enclosing-break-label)
		 (set-cdr! direct-enclosing
			   (cons this.id enclosing-ids))
		 (this.traverse2 labels direct-enclosing)
		 (label-remove! break-labels this.id))
	      (let* ((break-label (new-node Break-label)))
		 (if (not break-label.add-id!)
		     (print (pobject-name break-label)))
		 (break-label.add-id! this.id)
		 (set! this.label break-label)
		 (label-set! break-labels this.id break-label)
		 (this.traverse2 labels (cons break-label
					      (list this.id)))
		 (label-remove! break-labels this.id))))))

(define-pmethod (Loop-label-res labels direct-enclosing)
   (let* ((break-labels (car labels))
	  (old-default-break-label (label-get break-labels
					      *default-break-label-id*))
	  (break-label (if direct-enclosing
			   (car direct-enclosing)
			   (new-node Break-label)))
	  (continue-labels (cdr labels))
	  (old-default-continue-label (label-get continue-labels
						 *default-continue-label-id*))
	  (continue-label (new-node Continue-label)))
      (break-label.add-id! *default-break-label-id*)
      (label-set! break-labels *default-break-label-id* break-label)
      (set! this.break-label break-label)

      (continue-label.add-id! *default-continue-label-id*)
      (label-set! continue-labels *default-continue-label-id* continue-label)
      (set! this.continue-label continue-label)
      (if direct-enclosing ;; add continue-labels
	  (for-each (lambda (id)
		       (continue-label.add-id! id)
		       (label-set! continue-labels id continue-label))
		    (cdr direct-enclosing)))
      ;; recursively run through the loop
      ;; the continue-label becomes the break-label for nested Labelled or
      ;; Loops, but no ids are propagated.
      ;; 'this.traverse2' will visit the tests... with this information too,
      ;; but we know, that this will not be a problem, as they are expressions
      ;; and not statements.
      (this.traverse2 labels (cons continue-label '()))

      ;; remove the labels
      (label-set! break-labels
		  *default-break-label-id*
		  old-default-break-label)
      (label-set! continue-labels
		  *default-continue-label-id*
		  old-default-continue-label)
      (if direct-enclosing
	  (for-each (lambda (id)
		       (label-remove! continue-labels id))
		    (cdr direct-enclosing)))))

(define-pmethod (Switch-clause-label-res labels direct-enclosing)
   (let* ((break-labels (car labels))
	  (old-default-break-label (label-get break-labels
					      *default-break-label-id*))
	  (break-label (if direct-enclosing
			   (car direct-enclosing)
			   (new-node Break-label))))
      (break-label.add-id! *default-break-label-id*)
      (label-set! break-labels *default-break-label-id* break-label)
      (set! this.break-label break-label)
      (this.traverse2 labels #f)
      ;; remove the label
      (label-set! break-labels
		  *default-break-label-id*
		  old-default-break-label)))
