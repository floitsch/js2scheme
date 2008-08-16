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
    (class FSM-sleeping::FSM-node) ;; might have states sleeping on the nodes
    (final-class FSM-start::FSM-0-cost)
    (final-class FSM-final::FSM-node) ;; neither consuming nor 0-cost
    (final-class FSM-disjunction::FSM-node
       ;; the first alternative is inside the 'next'-field.
       (alternatives::pair-nil (default '()))
       (all-consuming?::bool (default #f)))

    (final-class FSM-repeat-entry::FSM-0-cost
       (repeat-exit::FSM-repeat-exit read-only))
    (final-class FSM-repeat-exit::FSM-0-cost
       (greedy?::bool read-only)
       loop-body::FSM-node
       (min::bint read-only)
       ;; max must not be 0.
       (max read-only)) ;; either bint or #f
    (final-class FSM-non-empty::FSM-0-cost
       (other::FSM-node read-only))

    (final-class FSM-backref::FSM-sleeping ;; might consume, but not always
       backref-nb::bint
       (case-sensitive?::bool read-only))
    
    (final-class FSM-char::FSM-consuming
       (c::char read-only))
    (final-class FSM-every-char::FSM-consuming) ;; everything matches.
    (final-class FSM-class::FSM-consuming
       (class read-only))

    (final-class FSM-assert::FSM-0-cost
       ;; does not consume any char!
       (condition::procedure read-only))

    (final-class FSM-condition::FSM-sleeping ;; might consume, but not always
       ;; returns #f or the number of consumed chars.
       (condition::procedure read-only))

    (final-class FSM-cluster::FSM-0-cost
       (cluster-index::bint (default -1))
       (backref-cluster-index (default #f)))

    (final-class FSM-cluster-clear::FSM-0-cost
       (start-index::bint read-only)
       (stop-index::bint read-only)
       (backref-start-index (default #f))
       (backref-stop-index (default #f)))

    ;; a cluster-assert does not consume any char, but its contained fsm must
    ;; be matched first to continue.
    (final-class FSM-cluster-assert::FSM-0-cost
       entry::FSM-node
       (contains-clusters?::bool (default #f))
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
      ((or :bos :bol :^
	  :eol :eos :$
	  :word-boundary :wbdry
	  :not-word-boundary :not-wbdry)
       ;; assertions
       'do-nothing)
      ((or (? char?)
	   ((or :any
	       :digit :not-digit
	       :space :not-space
	       :word :not-word
	       :xdigit :not-xdigit
	       :neg-char :one-of-chars)
	    ???-))
       ;; classes
       'do-nothing)
      (:empty
       'do-nothing)
      (else
       (error "count-clusters-and-search-backrefs"
	      "invalid clause"
	      scm-re))))

(define (never-empty? scm-re)
   (match-case scm-re
      ((:or . ?alternatives)
       (every? never-empty? alternatives))
      ((:seq . ?els)
       (any? never-empty? els))
      ;; assertions
      (((or :quantified :between) ?greedy? ?n1 ?n2 ?atom)
       (and (not (zero? n1))
	    (never-empty? atom)))
      (((and (or :pos-lookahead-cluster :lookahead
		 :neg-lookahead-cluster :neg-lookahead)
	     ?pos/neg?)
	?d)
       #f)
      (((or :cluster :sub) ?d)
       (never-empty? d))
      ((:backref ?n)
       #f)
      ((or :bos :bol :^
	  :eol :eos :$
	  :word-boundary :wbdry
	  :not-word-boundary :not-wbdry)
       ;; assertions
       #f)
      ((or (? char?)
	   ((or :any
	       :digit :not-digit
	       :space :not-space
	       :word :not-word
	       :xdigit :not-xdigit
	       :neg-char :one-of-chars)
	    ???-))
       ;; classes
       #t)
      (:empty
       #f)
      ((or :every-internal)
       #t)
      ((or :start-internal)
       #f)
      (else
       (error "never-empty?"
	      "forgot clause"
	      scm-re))))

(define (scm-regexp->fsm scm-re)
   (define multi-line? #f)
   (define case-sensitive? #t)

   (define (add-implicit-loop scm-re)
      (define (needs-implicit-loop? scm-re)
	 ;;   if a regexp starts with '^' and it's not a multi-line, then
	 ;;   we do not need the implicit 'everything*?' in front of the
	 ;;   regexp.
	 (match-case scm-re
	    ((:seq (or :bos :bol :^) ???-)
	     ;; no need for loop
	     #f)
	    (else #t)))

      (if (needs-implicit-loop? scm-re)
	  `(:seq (:quantified #f 0 #f :every-internal)
		 :start-internal
		 ,scm-re)
	  `(:seq :start-internal ,scm-re)))

   (co-instantiate ((tmp-entry (instantiate::FSM-node
				  (id -1)
				  (next *tmp-node*)))
		    (exit (instantiate::FSM-final
			     (id 0)
			     (next exit))))
      (let ((backrefs-mset (make-mset))
	    (cluster-count-box (list 0)))
	 (count-clusters-and-search-backrefs scm-re cluster-count-box
					     backrefs-mset)
	 (let ((backrefs-map (map (lambda (backref-index compressed-index)
				     ;; we prefer to count clusters from 0 and
				     ;; not 1. -> - 1
				     (cons (-fx backref-index 1)
					   compressed-index))
				  (sort <fx (mset->list backrefs-mset))
				  (iota (mset-size backrefs-mset))))
	       (loop-scm-re (add-implicit-loop scm-re)))
	    (receive (nb-nodes c-nb)
	       (scm-re->fsm loop-scm-re tmp-entry exit
			    1  ;; exit-node got '0'-id.
			    (car cluster-count-box)
			    case-sensitive?
			    multi-line?
			    backrefs-map)
	       [assert (c-nb) (zero? c-nb)]
	       (instantiate::FSM
		  ;; real entry is 'next' of tmp-entry
		  (entry (FSM-node-next tmp-entry))
		  (exit exit)
		  (nb-nodes nb-nodes)
		  (nb-clusters (car cluster-count-box))
		  (nb-backref-clusters (mset-size backrefs-mset))))))))

;; return a FSM-cluster-clear if from < to. Otherwise non is needed and #f is
;; returned.
;; Looks for backref-clusters too. (if a cluster inside the interval is used as
;; backref-cluster then the corresponding backref-cluster entry must be cleared
;; too.
(define (create-cluster-clear from to backrefs-map id next)
   (and (<fx from to)
	(let* ((br-start (let loop ((i from))
			    (cond
			       ((>= i to)
				#f)
			       ((assq i backrefs-map)
				=>
				(lambda (t)
				   (*fx (cdr t) 2)))
			       (else
				(loop (+fx i 1))))))
	       (br-stop (and br-start
			     (let loop ((i (-fx to 1)))
				(cond
				   ((< i from)
				    #f)
				   ((assq i backrefs-map)
				    =>
				    ;; add 1 so this entry is not ignored.
				    ;; only [br-start; br-stop[ is cleared.
				    (lambda (t)
				       (*fx (+fx (cdr t) 1) 2)))
				   (else
				    (loop (-fx i 1))))))))
	   (instantiate::FSM-cluster-clear
	      (next next)
	      (id id)
	      (start-index (*fx from 2))
	      (stop-index (*fx to 2))
	      (backref-start-index br-start)
	      (backref-stop-index br-stop)))))

;; if all alternatives are consuming, then set the bool.
(define (check-consuming-alternatives dis-node::FSM-disjunction)
   (with-access::FSM-disjunction dis-node (next alternatives all-consuming?)
      (when (and (FSM-consuming? next)
	       (every? FSM-consuming? alternatives))
	 (set! all-consuming? #t))))

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
	 ((:not-word-boundary :not-wbdry)
	  (lambda (str current-pos)
	     (not (word-boundary str current-pos))))))
   
   (match-case scm-re
      ;; internal clauses.
      (:start-internal
       (with-access::FSM-node entry (next)
	  (set! next (instantiate::FSM-start
			(id nodes-nb)
			(next exit))))
       (values (+fx nodes-nb 1) clusters-nb))
      (:every-internal
       (with-access::FSM-node entry (next)
	  (set! next (instantiate::FSM-every-char
			(id nodes-nb)
			(next exit))))
       (values (+fx nodes-nb 1) clusters-nb))
      ((:non-empty-internal ?atom)
       (if (never-empty? atom)
	   (receive (n-nb c-nb)
	      (recurse atom entry exit nodes-nb clusters-nb)
	      (values n-nb c-nb #f))
	   (let ((non-empty (instantiate::FSM-non-empty
			       (id nodes-nb)
			       (next *tmp-node*)
			       (other exit))))
	      (with-access::FSM-node entry (next)
		 (set! next non-empty))
	      (receive (n-nb c-nb)
		 (recurse atom non-empty exit (+fx nodes-nb 1) clusters-nb)
		 (values n-nb c-nb #t)))))

      ;; public clauses.
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
		 (receive (new-n-nb new-c-nb)
		    (recurse (car rev-alts) dis-node exit n-nb c-nb)
		    (cond
		       ((null? (cdr rev-alts))
			(check-consuming-alternatives dis-node)
			(values new-n-nb new-c-nb))
		       (else
			(with-access::FSM-disjunction dis-node (next
								alternatives)
			   ;; move the alt-node from the next-field to the
			   ;; alternatives field.
			   (set! alternatives (cons next alternatives))
			   (loop (cdr rev-alts)
				 new-n-nb
				 new-c-nb))))))))))
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
	  ((and (or (=fx n1 0)
		    (=fx n1 1))
		(not n2)) ;; x+ {1, #f} | x* {0, #f}
	   (let ((*-exit (instantiate::FSM-disjunction
			    (id (+fx nodes-nb 1))
			    (next *tmp-node*))))
	      (receive (n-nb c-nb created-non-empty?)
		 (recurse `(:non-empty-internal ,atom)
			  *-exit *-exit
			  (+fx nodes-nb 1) clusters-nb)
		 ;; if the body could be empty then the first iteration starts
		 ;; at the node inside the non-empty -> the first iteration
		 ;; might be empty.
		 ;; Only in the second iteration is the non-empty used.
		 (with-access::FSM-node entry (next)
		    (cond
		       ((=fx n1 0)
			(set! next *-exit))
		       (created-non-empty?
			(let* ((non-empty (FSM-node-next *-exit))
			       (non-empty-body (FSM-node-next non-empty)))
			   (set! next non-empty-body)))
		       (else
			(set! next (FSM-node-next *-exit)))))
		 (with-access::FSM-disjunction *-exit (next alternatives)
		    (let* ((cluster-clear (create-cluster-clear c-nb clusters-nb
								backrefs-map
								n-nb next))
			   (new-n-nb (if cluster-clear (+fx n-nb 1) n-nb))
			   (loop-body (or cluster-clear next)))
		       (if greedy?
			   (begin
			      (set! next loop-body)
			      (set! alternatives (list exit)))
			   (begin
			      (set! next exit)
			      (set! alternatives (list loop-body))))
		       (check-consuming-alternatives *-exit)
		       (values new-n-nb c-nb))))))
	  ((and (=fx n1 1)
		(=fx n2 1)) ;; {1, 1}
	   (recurse atom entry exit nodes-nb clusters-nb))
	  ((and (zero? n1)
		(=fx n2 1)) ;; x? {0, 1}
	   (let* ((?-entry (instantiate::FSM-disjunction
			      (id (+fx nodes-nb 1))
			      (next *tmp-node*))))
	      (with-access::FSM-node entry (next)
		 (set! next ?-entry))
	      (receive (n-nb c-nb created-non-empty?)
		 (recurse `(:non-empty-internal ,atom)
			  ?-entry exit
			  (+fx nodes-nb 2) clusters-nb)
		 (with-access::FSM-disjunction ?-entry (next alternatives)
		    (if greedy?
			(set! alternatives (list exit))
			(begin
			   (set! alternatives (list next))
			   (set! next exit))))
		 (check-consuming-alternatives ?-entry)
		 (values n-nb c-nb))))
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
	  ((zero? n1) ;; {0, n2}. simplify to '{1, n2}?'
	   (recurse `(:quantified ,greedy? 0 1
				  (:quantified ,greedy? 1 ,n2 ,atom))
		    entry exit nodes-nb clusters-nb))
	  (else ;; {1, n2}
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
					 (greedy? greedy?))))
	      (with-access::FSM-node entry (next)
		 (set! next rep-entry))
	      (receive (n-nb c-nb)
		 (recurse atom rep-entry rep-exit (+fx nodes-nb 2) clusters-nb)
		 (with-access::FSM-repeat-exit rep-exit (loop-body)
		    (let ((cluster-clear (create-cluster-clear
					  c-nb clusters-nb
					  backrefs-map
					  n-nb
					  (FSM-node-next rep-entry))))
		       (if cluster-clear
			   (begin
			      (set! loop-body cluster-clear)
			      (values (+fx n-nb 1) c-nb))
			   (begin
			      (set! loop-body (FSM-node-next rep-entry))
			      (values n-nb c-nb))))))))))
      (((or :cluster :sub) ?d)
       (let* ((cluster-entry (instantiate::FSM-cluster
				(id nodes-nb)
				(next *tmp-node*)))
	      (cluster-exit (instantiate::FSM-cluster
			       (id (+fx nodes-nb 1))
			       (next exit))))
	  (with-access::FSM-node entry (next)
	     (set! next cluster-entry))
	  (receive (n-nb c-nb)
	     (recurse d cluster-entry cluster-exit
		      (+fx nodes-nb 2) clusters-nb)
	     (let* ((this-c-nb (-fx c-nb 1)) ;; id of this cluster.
		    (backref-entry (assq this-c-nb backrefs-map)))
		(with-access::FSM-cluster cluster-entry (cluster-index
							 backref-cluster-index)
		   (set! cluster-index (*fx this-c-nb 2))
		   (when backref-entry
		      (set! backref-cluster-index
			    (*fx (cdr backref-entry) 2))))
		(with-access::FSM-cluster cluster-exit (cluster-index
							backref-cluster-index)
		   (set! cluster-index (+fx (*fx this-c-nb 2) 1))
		   (when backref-entry
		      (set! backref-cluster-index
			    (+fx (*fx (cdr backref-entry) 2) 1))))
		   
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
	     (with-access::FSM-node entry (next)
		(set! next d-entry))
	     (receive (n-nb c-nb)
		;; temporarily assign the entry to the 'next'-field.
		(recurse d d-entry d-exit (+fx nodes-nb 2) clusters-nb)
		(with-access::FSM-cluster-assert d-entry
		      (next entry contains-clusters?)
		   (set! entry next)
		   (set! next exit)
		   (set! contains-clusters? (not (=fx clusters-nb c-nb))))
		(values n-nb c-nb)))))
      ((:backref ?n)
       ;; note that 'n' starts counting from 1.
       ;; 'clusters-nb' starts from 0.
       ;; if clusters-nb == 1 then there is one open parenthesis.
       ;; we allow n == clusters-nb. for instance (\1) is ok.
       (if (not (<=fx n clusters-nb))
	   (error "fsm"
		  "backref references bad cluster"
		  n))
       
       ;; backrefs-map starts from 0. -> -1
       (let* ((backref-index (cdr (assq (-fx n 1) backrefs-map)))
	      (br (instantiate::FSM-backref
		    (id nodes-nb)
		    (next exit)
		    (backref-nb backref-index)
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
		:one-of-chars
		)
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
      ((:case-sensitive  ?re)
       (scm-re->fsm re entry exit nodes-nb clusters-nb
		    #t multi-line?
		    backrefs-map))
      ((:case-insensitive  ?re)
       (scm-re->fsm re entry exit nodes-nb clusters-nb
		    #f multi-line?
		    backrefs-map))
      ((:multi-line ?re)
       (scm-re->fsm re entry exit nodes-nb clusters-nb
		    case-sensitive? #t
		    backrefs-map))
      ((:no-multi-line ?re)
       (scm-re->fsm re entry exit nodes-nb clusters-nb
		    case-sensitive? #f
		    backrefs-map))
      (else
       (error "fsm"
	      "could not match scm-re"
	      scm-re))))
	     
