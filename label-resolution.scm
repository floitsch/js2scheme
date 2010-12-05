(module label-resolution
   (import label
	   verbose
	   nodes
	   walk)
   (export (label-resolution tree::Program)))

(define (label-resolution tree)
   (verbose " label-resolution")
   (label-res tree #f #unspecified #unspecified))

;; label-maps. pair of maps. car: break-labels. cdr: continue-labels.
(define-nmethod (Node.label-res label-maps direct-enclosing)
  (default-walk this label-maps #f))

(define-nmethod (Program.label-res label-maps direct-enclosing)
   (default-walk this
                 (cons (make-label-table)
		       (make-label-table))
		 #f))

(define-nmethod (Fun.label-res label-maps direct-enclosing)
   (let* ((break-labels (make-label-table))
	  (return-label (instantiate::Break-Label)))
      (label-add-id! return-label *return-label-id*)
      (label-set! break-labels *return-label-id* return-label)
      (Fun-return-label-set! this return-label)
      (default-walk this (cons break-labels (make-label-table)) #f)))
	     
(define-nmethod (Continue.label-res label-maps direct-enclosing)
   (with-access::Continue this (id label)
      (let* ((continue-labels (cdr label-maps))
	     (label-id (or id *default-continue-label-id*))
	     (entry (label-get continue-labels label-id)))
	 (if (not entry)
	     (error #f "Continue statement without matching label: " label-id)
	     (set! label entry)))))
	  
(define-nmethod (Break.label-res label-maps direct-enclosing)
   (with-access::Break this (id label)
      (let* ((break-labels (car label-maps))
	     (label-id (or id *default-break-label-id*))
	     (entry (label-get break-labels label-id)))
	 (if (not entry)
	     (error #f "Break statement without matching label: " label-id)
	     (set! label entry)))))

(define-nmethod (Return.label-res label-maps direct-enclosing)
   (with-access::Return this (label)
	 (let* ((break-labels (car label-maps))
		(label-id *return-label-id*)
		(entry (label-get break-labels label-id)))
	    (if (not entry)
		(error #f "Return statement at top-level." #f)
		(set! label entry)))))
	  
(define-nmethod (Labelled.label-res label-maps direct-enclosing)
   (with-access::Labelled this (id label)
      (let* ((break-labels (car label-maps))
	     (entry (label-get break-labels id)))
	 (if entry
	     (error #f "Nested Label-statement with same id: " id)
	     (if direct-enclosing
		 (let ((enclosing-break-label (car direct-enclosing))
		       (enclosing-ids (cdr direct-enclosing)))
		    (label-add-id! enclosing-break-label id)
		    (set! label enclosing-break-label)
		    (set-cdr! direct-enclosing
			      (cons id enclosing-ids))
		    (default-walk this label-maps direct-enclosing)
		    (label-remove! break-labels id))
		 (let* ((break-label (instantiate::Break-Label)))
		    (label-add-id! break-label id)
		    (set! label break-label)
		    (label-set! break-labels id break-label)
		    (default-walk this label-maps (cons break-label (list id)))
		    (label-remove! break-labels id)))))))

(define-nmethod (Loop.label-res label-maps direct-enclosing)
   (let* ((break-labels (car label-maps))
	  (old-default-break-label (label-get break-labels
					      *default-break-label-id*))
	  (break-label (if direct-enclosing
			   (car direct-enclosing)
			   (instantiate::Break-Label)))
	  (continue-labels (cdr label-maps))
	  (old-default-continue-label (label-get continue-labels
						 *default-continue-label-id*))
	  (continue-label (instantiate::Continue-Label)))
      (label-add-id! break-label *default-break-label-id*)
      (label-set! break-labels *default-break-label-id* break-label)
      (Loop-break-label-set! this break-label)

      (label-add-id! continue-label *default-continue-label-id*)
      (label-set! continue-labels *default-continue-label-id* continue-label)
      (Loop-continue-label-set! this continue-label)
      (if direct-enclosing ;; add continue-labels
	  (for-each (lambda (id)
		       (label-add-id! continue-label id)
		       (label-set! continue-labels id continue-label))
		    (cdr direct-enclosing)))
      ;; recursively run through the loop
      ;; the continue-label becomes the break-label for nested Labelled or
      ;; Loops, but no ids are propagated.
      ;; 'default-walk' will visit the tests... with this information too,
      ;; but we know, that this will not be a problem, as they are expressions
      ;; and not statements.
      (default-walk this label-maps (cons continue-label '()))

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

(define-nmethod (Switch-Clause.label-res label-maps direct-enclosing)
   (let* ((break-labels (car label-maps))
	  (old-default-break-label (label-get break-labels
					      *default-break-label-id*))
	  (break-label (if direct-enclosing
			   (car direct-enclosing)
			   (instantiate::Break-Label))))
      (label-add-id! break-label *default-break-label-id*)
      (label-set! break-labels *default-break-label-id* break-label)
      (Switch-Clause-break-label-set! this break-label)
      (default-walk this label-maps #f)
      ;; remove the label
      (label-set! break-labels
		  *default-break-label-id*
		  old-default-break-label)))
