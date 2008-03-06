(module jsre-RegExp-fsm
   (import jsre-RegExp-classes
	   mset)
   (export
    (class FSM
       entry::FSM-node
       exit::FSM-node
       nb-clusters::bint
       nb-backref-clusters::bint)
    (class FSM-node
       ;; TODO: this currently means, that we can't match strings longer than
       ;; 'bint'
       (forbidden?::bool (default #f)) ;; used for "not-empty"...
       (occupied-by::pair-nil (default '())))
    (final-class FSM-final::FSM-node)
    (class FSM-simple::FSM-node
       ;; transition to other FSM-node
       (transit (default #f))
       ;; transition that does not consume chars.
       (O-cost-transit (default #f)))
    (final-class FSM-disjunction::FSM-node
       ;; alternatives (O-cost)
       (alternatives::pair-nil read-only))

    (final-class FSM-*-entry::FSM-node
       (body::FSM-simple read-only)
       (*-exit::FSM-*-exit read-only)
       (greedy?::bool read-only))
    (final-class FSM-*-exit::FSM-node
       (*-entry::FSM-*-entry read-only)
       (exit::FSM-node read-only))
    (final-class FSM-non-empty::FSM-simple
       (exit::FSM-node read-only))
    (final-class FSM-?::FSM-node
       (body::FSM-simple read-only)
       (exit::FSM-node read-only) ;; the short-cut.
       (greedy?::bool read-only))
    (final-class FSM-backref::FSM-node
       (exit::FSM-node read-only)
       backref-nb)
    
    (class FSM-transit
       (target::FSM-node read-only)
       (dot-info (default ""))
       )
    (final-class FSM-char-transit::FSM-transit
       (c::char read-only))
    (final-class FSM-class-transit::FSM-transit
       (class read-only))
    (final-class FSM-assert-transit::FSM-transit
       (condition::procedure read-only))
    (final-class FSM-condition-transit::FSM-transit
       (condition::procedure read-only))
    (class FSM-cluster::FSM-transit
       (cluster-index::bint read-only))
    (final-class FSM-backref-cluster-exit::FSM-cluster
       (backref-index::bint read-only))
    ;; a cluster-assert does not consume any char, but simply asserts, that the
    ;; cluster is executable. entry and exit represent the sub-RE that needs to
    ;; be matched.
    (final-class FSM-cluster-assert::FSM-transit
       (entry::FSM-node read-only)
       (exit::FSM-node read-only)
       (negative?::bool read-only)) ;; when set, then result must be #f to continue.
    (final-class FSM-state
       clusters::vector
       backref-clusters::vector ;; duplicate of the relevant entries.
       (final-index (default #f))
       (collision?::bool (default #f))
       node::FSM-node)
    (wide-class FSM-sleeping-state::FSM-state
       cycles-to-sleep::bint))
   (export (scm-regexp->fsm scm-re case-sensitive? multi-line?)))


;; We transform a regexp into a finite-state-machine (FSM).
;; Biggest problem: how to merge several incoming transitions. We have adopted
;; the following schema:
;; - every iteration only one char is matched.
;; - every iteration a 'time'-var is increased. -> time == nb of chars matched.
;; - every FSM-node has a 'last-visited' field which saves the time it was last
;;   visited by a state. If there is already another state at the node the
;;   second state is simply discarded. The first to reach a node wins.
;; - All states are in a (priority)-list. First in list is first to
;;   act. Whenever a node has several exits the splitted nodes are sorted by
;;   priority (usually depending on 'greedy'-parameter of the RE). This new
;;   sorting is done by the nodes (inheritance...).
;; - The final-node cycles. Once the state with the highest priority (the first
;;   in list) has reached the final-node we can stop looking.
;;
;; Another difficulty: for x+ and x*. if x is of length, then x is considered
;; as 'not matched'. Example:
;;   (a?)+ matched with "". a? would match "", but (a?)+ does not match the
;;   empty string and the RE hence does not match the string.
;; we (ab)use the last-visited field to avoid empty matches. a small hack...

;; TODO: optimization.
;;       we can remove any cluster-changes for the first iterations of
;;       repetitions.
;;       Ex: (a|b){2,} becomes (non-empty (a|b))(non-empty (a|b))...
;;       the non-emptys share the same cluster-ids. -> the first non-empty does
;;       not need to update any cluster.

(define (terminator-char? c)
   (let ((n (char->integer c)))
      (or (=fx n #xA) ;; Linefeed
	  (=fx n #xD) ;; Carriage Return
	  ;; following entries can't happen unless we
	  ;; have switched to UCS2
	  (=fx n #x2028) ;; Line separator
	  (=fx n #x2029)))) ;; Paragraph separator

(define (word-char? c)
   (or (char-alphabetic? c)
       (char-numeric? c)
       (char=? c #\_)))

(define (char-normalize c)
   ;; TODO: char-normalize
   (char-downcase c))

(define (search-backrefs scm-re mset)
   (match-case scm-re
      ((disjunction . ?alternatives)
       (for-each (lambda (alt) (search-backrefs alt mset)) alternatives))
      ((sequence . ?els)
       (for-each (lambda (alt) (search-backrefs alt mset)) els))
      ;; assertions
      ((quantified ?atom . ?-)
       (search-backrefs atom mset))
      (((or pos-lookahead-cluster
	    neg-lookahead-cluster)
	?d)
       (search-backrefs d mset))
      ((back-ref ?n)
       (mset-put! mset n))
      (else
       'do-nothing)))
   
(define (scm-regexp->fsm scm-re case-sensitive? multi-line?)
   (define *backrefs-map* #unspecified) ;; will be defined later on

   (define (assertion-test-fun assert-id)
      ;; 15.10.2.6
      (define (word-boundary str start-pos current-pos)
	 (let ((len (string-length str)))
	    (cond
	       ((and (=fx start-pos current-pos)
		     (=fx current-pos len))
		#f)
	       ((=fx start-pos current-pos)
		(word-char? (string-ref str current-pos)))
	       ((=fx current-pos len)
		(word-char? (string-ref str (-fx current-pos 1))))
	       ((word-char? (string-ref str (-fx current-pos 1)))
		(not (word-char? (string-ref str current-pos))))
	       (else
		(word-char? (string-ref str current-pos))))))

      (case assert-id
	 ((^)
	  (if multi-line?
	      (lambda (str start-pos current-pos)
		 (or (=fx start-pos current-pos)
		     (terminator-char? (string-ref str (-fx current-pos 1)))))
	      (lambda (str start-pos current-pos)
		 (=fx start-pos current-pos))))
	 (($)
	  (if multi-line?
	      (lambda (str start-pos current-pos)
		 (or (=fx current-pos (string-length str))
		     (terminator-char? (string-ref str current-pos))))
	      (lambda (str start-pos current-pos)
		 (=fx current-pos (string-length str)))))
	 ((word-boundary)
	  word-boundary)
	 ((not-word-boundary)
	  (lambda (str start-pos current-pos)
	     (not (word-boundary str start-pos current-pos))))))
   
   ;; cluster-nb == nb of open parenthesis to the left.
   ;; precondition:
   ;;    - entry is a FSM-simple
   ;;    - entry and exit do not have any transits.
   ;; modifies the nodes to create the FSM
   ;; returns the next cluster-nb.
   (define (scm-re->fsm scm-re entry exit cluster-nb)
      (match-case scm-re
	 ((disjunction . ?alternatives)
	  (cond
	     ((null? alternatives)
	      (error "RegExp-FSM-construction"
		     "Disjunction must have at least one alternative"
		     '()))
	     ((null? (cdr alternatives))
	      (scm-re->fsm (car alternatives) entry exit cluster-nb))
	     (else
	      (let loop ((alternatives alternatives)
			 (rev-alt-nodes '())
			 (cluster-nb cluster-nb))
		 (if (null? alternatives)
		     (let ((dis-node (instantiate::FSM-disjunction
					(alternatives
					 (reverse! rev-alt-nodes)))))
			(with-access::FSM-simple entry (O-cost-transit)
			   (set! O-cost-transit (instantiate::FSM-transit
						   (target dis-node))))
			cluster-nb)
		     (let ((alt-entry (instantiate::FSM-simple)))
			(loop (cdr alternatives)
			      (cons alt-entry rev-alt-nodes)
			      (scm-re->fsm (car alternatives)
					   alt-entry exit
					   cluster-nb))))))))
	 ((sequence . ?els)
	  (if (null? els) ;; not even sure if that's possible
	      (with-access::FSM-simple entry (O-cost-transit)
		 (set! O-cost-transit (instantiate::FSM-transit
					 (target exit)))
		 cluster-nb)
	      (let loop ((entry entry)
			 (els els)
			 (cluster-nb cluster-nb))
		 (if (null? (cdr els))
		     (scm-re->fsm (car els) entry exit cluster-nb)
		     (let ((middle (instantiate::FSM-simple)))
			(loop middle
			      (cdr els)
			      (scm-re->fsm (car els)
					   entry middle
					   cluster-nb)))))))
	 ;; assertions
	 (((and (or ^ $ word-boundary not-word-boundary) ?assertion))
	  (let* ((test-fun (assertion-test-fun assertion))
		 (transit (instantiate::FSM-assert-transit
			     (dot-info assertion)
			     (condition test-fun)
			     (target exit))))
	     (with-access::FSM-simple entry (O-cost-transit)
		(set! O-cost-transit transit))
	     cluster-nb))
	 ;; quantified
	 ((quantified ?atom (repetitions ?n1 ?n2) ?greedy?)
	  ;; creates the required nodes. for instance {2, 5} will create 2
	  ;; copies of the atom. Each one must be non-empty.
	  ;; returns the new cluster-nb.
	  ;; nb must be > 0.
	  (define (required nb atom entry exit cluster-nb)
	     (let loop ((nb nb)
			(entry entry))
		(let ((non-empty (instantiate::FSM-non-empty
				    (exit exit))))
		   (with-access::FSM-simple entry (O-cost-transit)
		      (set! O-cost-transit (instantiate::FSM-transit
					      (target non-empty))))
		   (if (= nb 1)
		       (scm-re->fsm atom non-empty exit cluster-nb)
		       (let ((middle (instantiate::FSM-simple)))
			  (scm-re->fsm atom non-empty middle cluster-nb)
			  (loop (-fx nb 1) middle))))))

	  (define (unlimited greedy? atom entry exit cluster-nb)
	     ;; unlimited repetitions.
	     (co-instantiate ((*-entry (instantiate::FSM-*-entry
					  (body non-empty)
					  (*-exit *-exit)
					  (greedy? greedy?)))
			      (non-empty (instantiate::FSM-non-empty
					    (exit *-exit)))
			      (*-exit (instantiate::FSM-*-exit
					 (*-entry *-entry)
					 (exit exit))))
		(with-access::FSM-simple entry (O-cost-transit)
		   (set! O-cost-transit (instantiate::FSM-transit
					   (target *-entry))))
		(scm-re->fsm atom non-empty *-exit cluster-nb)))
	     
	  ;; creates the optional nodes. for instance for {2, 5} it will create
	  ;; 3 optional copies. If considered to be a match it must be
	  ;; non-empty.
	  ;; returns the cluster-nb.
	  ;; nb must be > 0.
	  (define (optional nb greedy? atom entry exit cluster-nb)
	     ;;
	     ;; following diagram should help
	     ;; (NE=non-empty, S=simple node, A=atom)
	     ;; for nb=2
	     ;;
	     ;;                                /->[NE]->[[A]]-\
	     ;;       /->[NE]->[[A]]->[S]->[?]-\               |
	     ;;      /                          v              v
	     ;; ->[?]---------------------------------------->[exit]
	     (let loop ((nb nb)
			(entry entry))
		(let* ((atom-exit (if (=fx nb 1)
				      exit
				      (instantiate::FSM-simple)))
		       (non-empty (instantiate::FSM-non-empty
				     (exit atom-exit)))
		       (?-entry (instantiate::FSM-?
				   (body non-empty)
				   (exit exit) ;; do not use the atom-exit
				   (greedy? greedy?))))
		   (with-access::FSM-simple entry (O-cost-transit)
		      (set! O-cost-transit (instantiate::FSM-transit
					      (target ?-entry))))
		   (if (=fx nb 1)
		       (scm-re->fsm atom non-empty atom-exit cluster-nb)
		       (begin
			  (scm-re->fsm atom non-empty atom-exit cluster-nb)
			  (loop (-fx nb 1) atom-exit))))))

	  (cond
	     ((and (zero? n1)
		   (not n2))
	      (unlimited greedy? atom entry exit cluster-nb))
	     ((zero? n1)
	      (optional n2 greedy? atom entry exit cluster-nb))
	     ((not n2)
	      (let ((middle (instantiate::FSM-simple)))
		 (required n1 atom entry middle cluster-nb)
		 (unlimited greedy? atom middle exit cluster-nb)))
	     ((=fx n1 n2)
	      (required n1 atom entry exit cluster-nb))
	     (else
	      (let ((middle (instantiate::FSM-simple)))
		 (required n1 atom entry middle cluster-nb)
		 (optional (-fx n2 n1) greedy? atom middle exit cluster-nb)))))
	 ((cluster ?d)
	  (let* ((d-entry (instantiate::FSM-simple))
		 (d-exit (instantiate::FSM-simple))
		 ;; backrefs start at 1
		 ;; cluster-nb starts at 0
		 (backref-entry (assq (+fx cluster-nb 1)
				      *backrefs-map*))
		 ;; note that we increase the cluster-nb here
		 (new-cluster-nb (scm-re->fsm d d-entry d-exit
					      (+fx cluster-nb 1)))
		 (cluster-index (*fx cluster-nb 2)))
	     ;; backref-cluster is only updated at exit: this allows for
	     ;; example to match: /(b\1|ab)*/.exec('abbab')
	     
	     ;; entry is the same for both.
	     (with-access::FSM-simple entry (O-cost-transit)
		(set! O-cost-transit
		      (instantiate::FSM-cluster
			 (target d-entry)
			 (cluster-index cluster-index))))
	     ;; but exit isn't.
	     (if backref-entry
		 (let ((backref-index (*fx (cdr backref-entry) 2)))
		    (with-access::FSM-simple d-exit (O-cost-transit)
		       (set! O-cost-transit
			     (instantiate::FSM-backref-cluster-exit
				(target exit)
				(cluster-index (+fx cluster-index 1))
				(backref-index (+fx backref-index 1))))))
		 (with-access::FSM-simple d-exit (O-cost-transit)
		    (set! O-cost-transit
			  (instantiate::FSM-cluster
			     (target exit)
			     (cluster-index (+fx cluster-index 1))))))
	     new-cluster-nb))
	 (((and (or pos-lookahead-cluster
		    neg-lookahead-cluster)
		?pos/neg?)
	   ?d)
	  (let* ((d-entry (instantiate::FSM-simple))
		 (d-exit (instantiate::FSM-final))
		 (new-cluster-nb (scm-re->fsm d d-entry d-exit cluster-nb)))
	     (with-access::FSM-simple entry (O-cost-transit)
		(set! O-cost-transit
		      (instantiate::FSM-cluster-assert
			 (target exit)
			 (entry d-entry)
			 (exit d-exit)
			 (negative? (eq? pos/neg?
					 'neg-lookahead-cluster)))))
	     new-cluster-nb))
	 (((and (or any number not-number white-space not-white-space
		    a-zA-Z0-9_ not-a-zA-Z0-9_)
		?kind))
	  (let* ((co (case kind
			((any) (lambda (c) (and c (not (terminator-char? c)))))
			((number) (lambda (c) (and c (char-numeric? c))))
			((not-number)
			 (lambda (c) (and c (not (char-numeric? c)))))
			((white-space)
			 (lambda (c) ;; TODO: char-whitespace?
			    (and c (char-whitespace? c))))
			((not-white-space)
			 (lambda (c) ;; TODO: char-whitespace?
			    (and c (not (char-whitespace? c)))))
			((a-zA-Z0-9_) (lambda (c) (and c (word-char? c))))
			((not-a-zA-Z0-9_)
			 (lambda (c) (and c (not (word-char? c)))))))
		 (t (instantiate::FSM-condition-transit
		       (dot-info (case kind
				    ((any) ".")
				    (else kind)))
		       (target exit)
		       (condition co))))
	     (with-access::FSM-simple entry (transit)
		(set! transit t))
	     cluster-nb))
	 ((back-ref ?n)
	  ;; note that 'n' starts counting from 1.
	  ;; 'cluster-nb' starts from 0.
	  ;; if cluster-nb == 1 then there is one open parenthesis.
	  ;; we allow n == cluster-nb. for instance (\1) is ok.
	  (if (not (<=fx n cluster-nb))
	      (error "fsm"
		     "back-ref references bad cluster"
		     n))
	  
	  (let ((br (instantiate::FSM-backref
		       (exit exit)
		       (backref-nb (-fx n 1))))) ;; adjust for 0-offset.
	     (with-access::FSM-simple entry (O-cost-transit)
		(set! O-cost-transit (instantiate::FSM-transit
					(target br))))
	     cluster-nb))
	 ((class ?chars/ranges ?invert?)
	  (let ((t (instantiate::FSM-class-transit
			      (target exit)
			      (dot-info (format "[~a~a]" (if invert? "^" "") chars/ranges))
			      (class (RegExp-class-condition chars/ranges
							     invert?
							     case-sensitive?)))))
	     (with-access::FSM-simple entry (transit)
		(set! transit t))
	     cluster-nb))
	 ((char ?c)
	  (let* ((ci (if case-sensitive?
			 c
			 (char-normalize c)))
		 (t (instantiate::FSM-char-transit
		       (target exit)
		       (c ci))))
	     (with-access::FSM-simple entry (transit)
		(set! transit t))
	     cluster-nb))
	 (else
	  (error "fsm"
		 "could not match scm-re"
		 scm-re))))
	     
   (let* ((entry (instantiate::FSM-simple))
	  (exit (instantiate::FSM-final))
	  (backrefs-mset (make-mset)))
      (search-backrefs scm-re backrefs-mset)
      (set! *backrefs-map* (map cons
				(sort <fx (mset->list backrefs-mset))
				(iota (mset-size backrefs-mset))))
      (let ((cluster-count (scm-re->fsm scm-re entry exit 0)))
	 (instantiate::FSM
	    (entry entry)
	    (exit exit)
	    (nb-clusters cluster-count)
	    (nb-backref-clusters (mset-size backrefs-mset))))))
