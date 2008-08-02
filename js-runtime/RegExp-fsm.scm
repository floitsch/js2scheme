(module jsre-RegExp-fsm
   (import jsre-RegExp-classes
	   mset)
   (export
    (class FSM
       entry::FSM-node
       exit::FSM-node
       nb-nodes::bint
       nb-clusters::bint
       nb-backref-clusters::bint)
    (class FSM-node
       (id::bint read-only)
       next::FSM-node) ;; final node points to itself.
    (class FSM-consuming::FSM-node) ;; consumes char to pass
    (class FSM-0-cost::FSM-node) ;; just used for propagation
    (final-class FSM-final::FSM-node) ;; neither consuming nor 0-cost
    (final-class FSM-disjunction::FSM-0-cost
       ;; the first alternative is inside the 'next'-field.
       (alternatives::pair-nil (default '())))

    (class FSM-loop-exit::FSM-0-cost
       ;; next becomes 'exit'.
       (greedy?::bool read-only)
       loop-body::FSM-node
       (clusters-begin::bint (default -1))
       (clusters-end::bint read-only))
    (final-class FSM-?::FSM-0-cost
       ;; body is in 'next'.
       (exit::FSM-node read-only) ;; the short-cut.
       (greedy?::bool read-only))
    (final-class FSM-*-exit::FSM-loop-exit)
    (final-class FSM-repeat-entry::FSM-0-cost
       (repeat-exit::FSM-repeat-exit read-only))
    (final-class FSM-repeat-exit::FSM-loop-exit
       (min::bint read-only)
       ;; max must not be 0.
       (max read-only)) ;; either bint or #f
    (final-class FSM-non-empty::FSM-0-cost
       (other::FSM-node read-only))

    (final-class FSM-backref::FSM-node ;; might consume, but not always
       backref-nb::bint
       (case-sensitive?::bool read-only))
    
    (final-class FSM-char::FSM-consuming
       (c::char read-only))
    (final-class FSM-class::FSM-consuming
       (class read-only))

    (final-class FSM-assert::FSM-0-cost
       ;; does not consume any char!
       (condition::procedure read-only))

    (final-class FSM-condition::FSM-node ;; might consume, but not always
       ;; returns #f or the number of consumed chars.
       (condition::procedure read-only))

    (final-class FSM-cluster-entry::FSM-0-cost
       (cluster-index::bint (default -1)))
    (final-class FSM-cluster-exit::FSM-0-cost
       (cluster-index::bint (default -1))
       (backref-exit-index (default #f)))

    ;; a cluster-assert does not consume any char, but its contained fsm must
    ;; be matched first to continue.
    (final-class FSM-cluster-assert::FSM-0-cost
       entry::FSM-node
       (negative?::bool read-only))) ;; when set, then #f to continue.

   (export (scm-regexp->fsm scm-re)))

(define *tmp-node*
   (co-instantiate ((t (instantiate::FSM-node (id -1) (next t))))
      t))

;; pray, that I did not forget other container-res.
(define (count-clusters-and-search-backrefs scm-re cluster-count-box mset)
   (define (recurse scm-re)
      (count-clusters-and-search-backrefs scm-re cluster-count-box mset))

   (match-case scm-re
      ((:or . ?alternatives)
       (for-each recurse alternatives))
      ((:seq . ?els)
       (for-each recurse els))
      ;; assertions
      (((or :quantified :between) ?greedy? ?n1 ?n2 ?atom)
       (recurse atom))
      (((and (or :pos-lookahead-cluster :lookahead
		 :neg-lookahead-cluster :neg-lookahead)
	     ?pos/neg?)
	?d)
       (recurse d))
      (((or :cluster :sub) ?d)
       (set-car! cluster-count-box (+fx (car cluster-count-box) 1))
       (recurse d))
      ((:backref ?n)
       (mset-put! mset n))
      (else
       'do-nothing)))
   
(define (scm-regexp->fsm scm-re)
   (co-instantiate ((entry (instantiate::FSM-node
			      (id -1)
			      (next *tmp-node*)))
		    (exit (instantiate::FSM-final
			     (id 0)
			     (next exit))))
      (let ((backrefs-mset (make-mset))
	    (cluster-count-box (list 0)))
	 (count-clusters-and-search-backrefs scm-re cluster-count-box
					     backrefs-mset)
	 (let ((backrefs-map (map cons
				  (sort <fx (mset->list backrefs-mset))
				  (iota (mset-size backrefs-mset)))))
	    (receive (nb-nodes c-nb)
	       (scm-re->fsm scm-re entry exit
			    1  ;; exit-node got '0'-id.
			    (car cluster-count-box)
			    #t ;; case-sensitive
			    #f ;; no multi-line
			    backrefs-map)
	       [assert (c-nb) (zero? c-nb)]
	       (instantiate::FSM
		  (entry (FSM-node-next entry))
		  (exit exit)
		  (nb-nodes nb-nodes)
		  (nb-clusters (car cluster-count-box))
		  (nb-backref-clusters (mset-size backrefs-mset))))))))

;; clusters-nb == nb of remaining open parenthesis.
;; IMPORTANT: we construct the fsm from right to left. -> clusters-nb is 0 at
;;            the end and not at the beginning.
;;
;; scm-re->fsm receives an entry and an exit, and constructs the scm-re inside
;; these two nodes. It will update the 'next'-field of 'entry', and will point
;; all leaving 'next's to 'exit'.
;; returns the next nodes-nb and clusters-nb.
(define (scm-re->fsm scm-re entry exit nodes-nb clusters-nb
		     case-sensitive? multi-line?
		     backrefs-map)

   (define (recurse scm-re entry exit nodes-nb clusters-nb) ;; shorter to type
      (scm-re->fsm scm-re entry exit nodes-nb clusters-nb
		   case-sensitive? multi-line?
		   backrefs-map))
	 

   (define (assertion-test-fun assert-id)
      ;; 15.10.2.6
      (define (word-boundary str index)
	 (define (word-char? c)
	    (or (char-alphabetic? c)
		(char-numeric? c)
		(char=? c #\_)))

	 (let ((len (string-length str)))
	    (cond
	       ((zerofx? len)
		#f)
	       ((or (zerofx? index)
		    (=fx index (-fx len 1)))
		(word-char? (string-ref str index)))
	       ((word-char? (string-ref str (-fx index 1)))
		(not (word-char? (string-ref str index))))
	       (else ;; index-1 is not word-char
		(word-char? (string-ref str index))))))

      (define (terminator-char? c)
	 (let ((n (char->integer c)))
	    (or (=fx n #xA) ;; Linefeed
		(=fx n #xD) ;; Carriage Return
		;; following entries can't happen unless we
		;; have switched to UCS2
		(=fx n #x2028) ;; Line separator
		(=fx n #x2029)))) ;; Paragraph separator


      (case assert-id
	 ((:bol :bos :^)
	  (if multi-line?
	      (lambda (str index)
		 (or (zerofx? index)
			  (terminator-char? (string-ref str (-fx index 1)))))
	      (lambda (str index)
		 (zerofx? index))))
	 ((:eol :eos :$)
	  (if multi-line?
	      (lambda (str index)
		 (or (=fx index (string-length str))
		     (terminator-char? (string-ref str index))))
	      (lambda (str index)
		 (=fx index (string-length str)))))
	 ((:word-boundary :wbdry)
	  word-boundary)
	 ((:not word-boundary :not-wbdry)
	  (lambda (str current-pos)
	     (not (word-boundary str current-pos))))))
   
   (match-case scm-re
      ((:or . ?alternatives)
       (cond
	  ((null? alternatives)
	   (error "RegExp-FSM-construction"
		  "Disjunction must have at least one alternative"
		  '()))
	  ((null? (cdr alternatives))
	   (recurse (car alternatives) entry exit nodes-nb clusters-nb))
	  (else
	   ;; alternatives must be processed in reverse order (to make the
	   ;; clusters-nb work).
	   ;; all but the first of the alternatives are stored in the
	   ;; alternatives-field. The first one is directly stored in the
	   ;; next-field.
	   ;; As we reverse the list first, it is the last one that receives
	   ;; special care.
	   (let ((dis-node (instantiate::FSM-disjunction
			      (id nodes-nb)
			      (next *tmp-node*))))
	      (with-access::FSM-node entry (next)
		 (set! next dis-node))
	      (let loop ((rev-alts (reverse alternatives))
			 (n-nb (+fx nodes-nb 1))
			 (c-nb clusters-nb))
		 (if (null? (cdr rev-alts))
		     (recurse (car rev-alts) dis-node exit n-nb c-nb)
		     (receive (new-n-nb new-c-nb)
			(recurse (car rev-alts)
				 dis-node exit
				 n-nb c-nb)
			(with-access::FSM-disjunction dis-node (next
								alternatives)
			   ;; move the alt-node from the next-field to the
			   ;; alternatives field.
			   (set! alternatives (cons next alternatives))
			   (loop (cdr rev-alts)
				 new-n-nb
				 new-c-nb)))))))))
      ((:seq . ?els)
       (if (null? els) ;; not even sure if that's possible
	   (with-access::FSM-node entry (next)
	      (set! next exit)
	      (values nodes-nb clusters-nb))
	   ;; we must build the sequences in reverse-order (actually this
	   ;; production was the whole reason for all the trouble with the
	   ;; clusters-nb).
	   (let loop ((exit exit)
		      (rev-els (reverse els))
		      (n-nb nodes-nb)
		      (c-nb clusters-nb))
	      (if (null? (cdr rev-els))
		  (recurse (car rev-els) entry exit n-nb c-nb)
		  (receive (new-n-nb new-c-nb)
		     (recurse (car rev-els)
			      entry exit n-nb c-nb)
		     (with-access::FSM-node entry (next)
			(loop next
			      (cdr rev-els)
			      new-n-nb
			      new-c-nb)))))))
      ;; assertions
      ((and (or :^ :bol :bos
		:$ :eol :eos
		:wbdry :word-boundary
		:not-wbdry :not-word-boundary) ?assertion)
       (let* ((test-fun (assertion-test-fun assertion))
	      (assert-node (instantiate::FSM-assert
			      (id nodes-nb)
			      (condition test-fun)
			      (next exit))))
	  (with-access::FSM-node entry (next)
	     (set! next assert-node))
	  (values (+fx nodes-nb 1) clusters-nb)))
      ;; quantified
      (((or :quantified :between) ?greedy? ?n1 ?n2 ?atom)
       (cond
	  ((and (zero? n1)
		(not n2)) ;; x* {0, #f}
	   (co-instantiate ((?-entry (instantiate::FSM-?
					(id nodes-nb)
					(next non-empty)
					(exit exit)
					(greedy? greedy?)))
			    (non-empty (instantiate::FSM-non-empty
					  (id (+fx nodes-nb 1))
					  (next *tmp-node*)
					  (other *-exit)))
			    (*-exit (instantiate::FSM-*-exit
				       (id (+fx nodes-nb 2))
				       (loop-body non-empty)
				       (greedy? greedy?)
				       (clusters-end (*fx clusters-nb 2))
				       (next exit))))
	      (with-access::FSM-node entry (next)
		 (set! next ?-entry))
	      (receive (n-nb c-nb)
		 (recurse atom non-empty *-exit (+fx nodes-nb 3) clusters-nb)
		 (with-access::FSM-*-exit *-exit (clusters-begin)
		    (set! clusters-begin (*fx c-nb 2)))
		 (values n-nb c-nb))))
	  ((and (=fx n1 1)
		(not n2)) ;; x+ {1, #f}
	   ;; the first iteration starts at the node inside the non-empty ->
	   ;; the first iteration might be empty.
	   ;; Only in the second iteration is the non-empty used.
	   (co-instantiate ((non-empty (instantiate::FSM-non-empty
					  (id nodes-nb)
					  (next *tmp-node*)
					  (other *-exit)))
			    (*-exit (instantiate::FSM-*-exit
				       (id (+fx nodes-nb 1))
				       (loop-body non-empty)
				       (greedy? greedy?)
				       (clusters-end (*fx clusters-nb 2))
				       (next exit))))
	      (receive (n-nb c-nb)
		 (recurse atom non-empty *-exit (+fx nodes-nb 2) clusters-nb)
		 (with-access::FSM-node entry (next)
		    (set! next (FSM-node-next non-empty)))
		 (with-access::FSM-*-exit *-exit (clusters-begin)
		    (set! clusters-begin (*fx c-nb 2)))
		 (values n-nb c-nb))))
	  ((and (zero? n1)
		(=fx n2 1)) ;; x? {0, 1}
	   (let* ((non-empty (instantiate::FSM-non-empty
				(id nodes-nb)
				(next *tmp-node*)
				(other exit)))
		  (?-entry (instantiate::FSM-?
			      (id (+fx nodes-nb 1))
			      (next non-empty)
			      (exit exit)
			      (greedy? greedy?))))
	      (with-access::FSM-node entry (next)
		 (set! next ?-entry))
	      (recurse atom non-empty exit (+fx nodes-nb 2) clusters-nb)))
	  ((and (zero? n1)
		(zero? n2)) ;; nonsensical
	   ;; we still need to recurse to get the correct clusters
	   (receive (n-nb c-nb)
	      (recurse atom
		       entry
		       exit
		       nodes-nb
		       clusters-nb)
	      ;; now shortcut the unnecessary nodes.
	      (with-access::FSM-node entry (next)
		 (set! next exit))
	      ;; we can reuse the old nodes-nb, but we have to keep the new
	      ;; clusters-nb.
	      ;; this is the reason we have to do the cluster-numbering at the
	      ;; same time as the fsm-construction.
	      (values nodes-nb c-nb)))
	  (else
	   (co-instantiate ((rep-entry (instantiate::FSM-repeat-entry
					  (id nodes-nb)
					  (next *tmp-node*)
					  (repeat-exit rep-exit)))
			    (rep-exit (instantiate::FSM-repeat-exit
					 (id (+fx nodes-nb 1))
					 (next exit)
					 (loop-body *tmp-node*)
					 (min n1)
					 (max n2)
					 (greedy? greedy?)
					 (clusters-end (*fx 2 clusters-nb)))))
	      (let ((new-nodes-nb (if (zero? min)
				      (+fx nodes-nb 4) ;; will be used shortly
				      (+fx nodes-nb 2))))
		 
		 (if (zero? min)
		     (let* ((non-empty (instantiate::FSM-non-empty
					  (id (+fx nodes-nb 2))
					  (next rep-entry)
					  (other rep-exit)))
			    (?-entry (instantiate::FSM-?
				       (id (+fx nodes-nb 3))
				       (next non-empty)
				       (exit exit)
				       (greedy? greedy?))))
			(with-access::FSM-node entry (next)
			   (set! next ?-entry)))
		     (with-access::FSM-node entry (next)
			(set! next rep-entry)))
		 (receive (n-nb c-nb)
		    (recurse atom rep-entry rep-exit new-nodes-nb clusters-nb)
		    (with-access::FSM-repeat-exit rep-exit (loop-body
							    clusters-begin)
		       (set! loop-body (FSM-node-next rep-entry))
		       (set! clusters-begin (*fx c-nb 2)))
		    (values n-nb c-nb)))))))
      (((or :cluster :sub) ?d)
       (let* ((cluster-entry (instantiate::FSM-cluster-entry
				(id nodes-nb)
				(next *tmp-node*)))
	      (cluster-exit (instantiate::FSM-cluster-exit
			       (id (+fx nodes-nb 1))
			       (next exit))))
	  (with-access::FSM-node entry (next)
	     (set! next cluster-entry))
	  (receive (n-nb c-nb)
	     (recurse d cluster-entry cluster-exit
		      (+fx nodes-nb 2) clusters-nb)
	     (with-access::FSM-cluster-entry cluster-entry (cluster-index)
		(set! cluster-index (*fx (-fx c-nb 1) 2)))
	     (with-access::FSM-cluster-exit cluster-exit (cluster-index
							  backref-exit-index)
		(set! cluster-index (+fx (*fx (-fx c-nb 1) 2) 1))

		(let (;; backrefs start at 1
		      ;; clusters-nb starts at 0
		      (backref-entry (assq (+fx clusters-nb 1) backrefs-map)))
		   ;; backref-cluster is only updated at exit: this allows for
		   ;; example to match: /(b\1|ab)*/.exec('abbab')
		   ;; the matcher will automatically copy the opening-index
		   ;; from the cluster-vector to the backref-vector.
		   (when backref-entry
		      (set! backref-exit-index (+fx (*fx backref-entry 2) 1))))

		;; note that we decrease the clusters-nb here
		(values n-nb (-fx c-nb 1))))))
      (((and (or :pos-lookahead-cluster :lookahead
		 :neg-lookahead-cluster :neg-lookahead)
	     ?pos/neg?)
	?d)
       (let ((negative? (or (eq? pos/neg? ':neg-lookahead-cluster)
			    (eq? pos/neg? ':neg-lookahead))))
	  (co-instantiate ((d-entry (instantiate::FSM-cluster-assert
				       (id nodes-nb)
				       (next *tmp-node*)
				       (negative? negative?)
				       (entry *tmp-node*)))
			   (d-exit (instantiate::FSM-final
				      (id (+fx nodes-nb 1))
				      (next d-exit))))
	     (receive (n-nb c-nb)
		;; temporarily assign the entry to the 'next'-field.
		(recurse d d-entry d-exit (+fx nodes-nb 2) clusters-nb)
		(with-access::FSM-cluster-assert d-entry (next entry)
		   (set! entry next)
		   (set! next exit))))))
      ((:backref ?n)
       ;; note that 'n' starts counting from 1.
       ;; 'clusters-nb' starts from 0.
       ;; if clusters-nb == 1 then there is one open parenthesis.
       ;; we allow n == clusters-nb. for instance (\1) is ok.
       (if (not (<=fx n clusters-nb))
	   (error "fsm"
		  "backref references bad cluster"
		  n))
       
       (let ((br (instantiate::FSM-backref
		    (id nodes-nb)
		    (next exit)
		    (backref-nb (-fx n 1)) ;; adjust for 0-offset.
		    (case-sensitive? case-sensitive?))))
	  (with-access::FSM-node entry (next)
	     (set! next br))
	  (values (+fx nodes-nb 1) clusters-nb)))
      ((or (? char?)
	   ((or :any
		:digit :not-digit
		:space :not-space
		:word :not-word
		:xdigit :not-xdigit
		:neg-char
		:one-of-chars)
	    . ?-))
       (let* ((class-or-c (RegExp-class-create scm-re case-sensitive?))
	      (t (if (char? class-or-c)
		     (instantiate::FSM-char
			(id nodes-nb)
			(next exit)
			(c class-or-c))
		     (instantiate::FSM-class
			(id nodes-nb)
			(next exit)
			(class class-or-c)))))
	  (with-access::FSM-node entry (next)
	     (set! next t))
	  (values (+fx nodes-nb 1) clusters-nb)))
      (:empty
       (with-access::FSM-node entry (next)
	  (set! next exit))
       (values nodes-nb clusters-nb))
      (else
       (error "fsm"
	      "could not match scm-re"
	      scm-re))))
	     
