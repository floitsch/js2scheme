(module jsre-RegExp-match
   (import jsre-RegExp-classes
	   jsre-RegExp-fsm
	   jsre-RegExp-dot
	   jsre-RegExp-state)
   (import multi-top-level)
   (export (regexp-match fsm::FSM str::bstring start-index::bint)
	   (regexp-test fsm::FSM str::bstring start-index::bint)
	   *debug*) ;; only for now...
   (static
    (wide-class FSM-opt-disjunction::FSM-disjunction
       (all-consuming?::bool (default #f)))
    (class Node-use
       (occupied-by::pair-nil (default '()))
       (forbidden?::bool (default #f)))
    (class Globals
       *fsm*
       *node-uses*
       *frozen-states*::pair-nil
       *nb-frozen-layers*::bint
       *rev-pushed-states*::pair-nil)
    ))
(define-macro (receive . L) `(multiple-value-bind ,@L))
(define *debug* #f)

;; for a description of the process look at RegExp-fsm

(define (regexp-match fsm str start-index)
   (exec fsm str start-index #f))

(define (regexp-test fsm str start-index)
   (exec fsm str start-index #t))

;; outside of multi-top-level
;;  -> every call to these procedures create new top-levels
(define (fresh-recursive-match fsm state str start-index)
   (recursive-exec fsm state str start-index #f))
(define (fresh-recursive-test fsm state str start-index)
   (recursive-exec fsm state str start-index #t))

(multi-top-level (class Globals
 		    *fsm*
 		    *node-uses*
 		    *frozen-states*::pair-nil
 		    *nb-frozen-layers*::bint
 		    *rev-pushed-states*::pair-nil)
		 
(define *fsm* #unspecified)

(define *node-uses* #unspecified)

(define (node-uses-init! nb-nodes)
   (set! *node-uses* (make-vector nb-nodes))
   (let loop ((i 0))
      (when (<fx i nb-nodes)
	 (vector-set! *node-uses* i (instantiate::Node-use))
	 (loop (+fx i 1)))))

(define (forbidden? n::FSM-node)
   (with-access::FSM-node n (id)
      (with-access::Node-use (vector-ref *node-uses* id) (forbidden?)
	 forbidden?)))
(define (forbidden?-set! n::FSM-node new-val)
   (with-access::FSM-node n (id)
      (with-access::Node-use (vector-ref *node-uses* id) (forbidden?)
	 (set! forbidden? new-val))))

(define (occupied-by n::FSM-node)
   (with-access::FSM-node n (id)
      (with-access::Node-use (vector-ref *node-uses* id) (occupied-by)
	 occupied-by)))
(define (occupied-by-set! n::FSM-node new-val)
   (with-access::FSM-node n (id)
      (with-access::Node-use (vector-ref *node-uses* id) (occupied-by)
	 (set! occupied-by new-val))))
(define (occupied-by-push! n::FSM-node val)
   (with-access::FSM-node n (id)
      (with-access::Node-use (vector-ref *node-uses* id) (occupied-by)
	 (set! occupied-by (cons val occupied-by)))))
(define (occupied-by-clear! n::FSM-node)
   (with-access::FSM-node n (id)
      (with-access::Node-use (vector-ref *node-uses* id) (occupied-by)
	 (set! occupied-by '()))))

(define (exec fsm str start-index only-test?)
   (set! *fsm* fsm)
   (when *debug*
      (with-output-to-file "fsm.dot"
	 (lambda ()
	    (regexp->dot *fsm*))))
   (with-access::FSM fsm (entry nb-nodes nb-clusters nb-backref-clusters)
      (let* ((state (instantiate::FSM-state
		       (clusters (make-vector (* nb-clusters 2) #f))
		       (backref-clusters (make-vector (* nb-backref-clusters 2)
						      #f))
		       (node entry))))
	 (node-uses-init! nb-nodes)
	 (let ((match (run (list state) str start-index only-test?)))
	    (and match
		 (or only-test?
		     (with-access::FSM-state match (start-index final-index
								clusters)
			(list start-index final-index clusters))))))))

;; state must already point to start of nested fsm.
(define (recursive-exec fsm state str start-index only-test?)
   (set! *fsm* fsm)
   (with-access::FSM fsm (nb-nodes)
      (node-uses-init! nb-nodes)
      (run (list state) str start-index only-test?)))

(define *frozen-states* '())
(define *nb-frozen-layers* 0)

;; freezes all but 'keep' states.
;; if the kept states don't match, we come back and unfreeze them.
;; keep must not be 0.
(define (freeze! states index keep)
   (cond
      ((null? states) ;; should not happen, but we don't care.
       'do-nothing)
      ((and (=fx keep 1)
	    (null? (cdr states)))
       'do-nothing)
      ((=fx keep 1)
       (set! *frozen-states* (cons (cons (cdr states) index)
				   *frozen-states*))
       (set! *nb-frozen-layers* (+fx *nb-frozen-layers* 1))
       (set-cdr! states '()))
      (else (freeze! (cdr states) index (-fx keep 1)))))

;; *frozen-states* must not be '()
(define (restore-waiting-states!)
   (let ((first (car *frozen-states*)))
      (set! *frozen-states* (cdr *frozen-states*))
      (set! *nb-frozen-layers* (-fx *nb-frozen-layers* 1))
      (let ((states (car first))
	    (index (cdr first)))
	 (values states index))))

(define *rev-pushed-states* '())
(define (push-state! s)
   (set! *rev-pushed-states* (cons s *rev-pushed-states*)))

(define (clear-visitors!)
   (for-each
    (lambda (state)
       (with-access::FSM-state state (node)
	  (occupied-by-clear! node)))
    *rev-pushed-states*))


(define *max-parallel-states* 1000)

(define (next-round-states! index) ;; get pushed states
   (let* ((next-round-states (reverse! *rev-pushed-states*))
	  (len (length next-round-states)))
      (set! *rev-pushed-states* '())

      ;; HACK: hard-coded ad-hoc algo to cut next-round-states...
      (when (>fx (bit-lsh len *nb-frozen-layers*)
	       *max-parallel-states*)
	 (let ((keep-nb (max (bit-rsh *max-parallel-states*
				      (+ *nb-frozen-layers* 1))
			     1)))
	    (freeze! next-round-states index keep-nb)))

      (if (and (null? next-round-states)
	       (not (zero? *nb-frozen-layers*)))
	  (restore-waiting-states!)
	  (values next-round-states index))))

(define (duplicate-state::FSM-state state::FSM-state)
   (with-access::FSM-state state (sharing-clusters? sharing-br-clusters?)
      (set! sharing-clusters? #t)
      (set! sharing-br-clusters? #t))
   (duplicate::FSM-state state
      (still-needed? #f)
      (occupying? #f)))

(define (modifiable-state::FSM-state state::FSM-state)
   (with-access::FSM-state state (still-needed? occupying?)
      (if (or still-needed? occupying?)
	  (duplicate-state state)
	  state)))

(define (modifiable?::bool state::FSM-state)
   (with-access::FSM-state state (still-needed? occupying?)
      (not (or still-needed? occupying?))))
   
;; run one iteration at a time
(define (run states str index only-test?)
   (when *debug*
      (with-output-to-file (format "~aa.dot" index)
	 (lambda ()
	    (running->dot *fsm* states *frozen-states*
			  str index))))
   ;; first propagate states.
   (let ((reached-final?
	     (any? (lambda (state)
			  (with-access::FSM-state state (node still-needed?
							      occupying?)
			     (set! still-needed? #f)
			     (set! occupying? #f)
			     (propagate node state str index)))
		       states)))

      (when reached-final?
	 ;; clear frozen nodes. we have reached the final node, and all
	 ;; frozen ones are of lower priority.
	 (set! *frozen-states* '()))
	 
      ;; then clear any information we left at the nodes.
      (clear-visitors!)

      ;; get the states for the next round.
      ;; this might defrost some old states if no state was left.
      (receive (new-states new-index)
	 (next-round-states! index)
	 (when *debug*
	    (with-output-to-file (format "~ab.dot" new-index)
	       (lambda ()
		  (running->dot *fsm* new-states *frozen-states*
				str new-index))))
	 (cond
	    ((and only-test? reached-final?)
	     #t) ;; we reached a final node. no need to look which state.
	    ((null? new-states)
	     #f) ;; no match
	    ((FSM-final? (FSM-state-node (car new-states)))
	     ;; first state in priority-list is final. Can't get any better...
	     (car new-states))
	    ((>=fx new-index (string-length str))
	     ;; end of string. If there's a state pointing to the final-node we
	     ;; have a winner.
	     (let ((winner (any (lambda (state)
				   (and (FSM-final? (FSM-state-node state))
					state))
				new-states)))
		;; if there's a winner return it. otherwise see if there are still
		;; states waiting (by rerunning the procedure).
		(or winner
		    (run '() str new-index only-test?))))
	    (else ;; consume thus advancing to the next index
	     (for-each (lambda (state)
			  (with-access::FSM-state state (node)
			     (advance node state str new-index)))
		       new-states)
	     (let ((live-states (reverse! *rev-pushed-states*)))
		(set! *rev-pushed-states* '())
		(run live-states str (+fx new-index 1) only-test?)))))))

;; target is off-limit if it is occupied by a state with same backref-cluster,
;; or if it is forbidden.
(define (off-limit? n::FSM-node state::FSM-state)
   ;; defender is the state occupying the node
   ;; attacker is the new state.
   ;; --------

   (define (same-brefs? backrefs-defending backrefs-attacker)
      (equal? backrefs-attacker backrefs-defending))

   ;; either the same or defending beats attacker.
   ;; in this case the attacker just can't get better than the defender.
   (define (better-loops? loops-defending loops-attacker)
      (or (eq? loops-defending loops-attacker)
	  (every? (lambda (l-d l-a)
		     (with-access::FSM-loop-info l-a (loop-exit)
			(with-access::FSM-repeat-exit loop-exit (min max
								     greedy?)
			   ;; both must be in same loops. so no need to get
			   ;; loop-exit from l-d
			   (let ((it-defend (FSM-loop-info-iterations l-d))
				 (it-attack (FSM-loop-info-iterations l-a)))
			      (or (=fx it-defend it-attack)
				  (and greedy?
				       (not max)
				       (>fx it-defend it-attack))
				  (and (not greedy?)
				       (>=fx it-defend min)
				       (<fx it-defend it-attack)))))))
		  loops-defending
		  loops-attacker)))

   (or (forbidden? n)
       (any? (lambda (other-state)
		(and (same-brefs? (FSM-state-backref-clusters other-state)
				  (FSM-state-backref-clusters state))
		     (better-loops? (FSM-state-loops other-state)
				    (FSM-state-loops state))))
	     (occupied-by n))))

(define (occupy! n::FSM-node state::FSM-state)
   (with-access::FSM-state state (node occupying?)
      (cond
	 (occupying?
	  (occupy! n (duplicate-state state)))
	 (else
	  (set! node n)
	  (set! occupying? #t)
	  (occupied-by-push! n state)
	  (push-state! state)))))


(define (propagate-check n::FSM-node state::FSM-state
			 str::bstring index::bint)
   (unless (forbidden? n)
      (propagate n state str index)))

(define-generic (propagate n::FSM-node state::FSM-state
			   str::bstring index::bint)
   (error "FSM-match"
	  "forgot node-type"
	  n))

(define-method (propagate n::FSM-consuming state str index)
   (unless (off-limit? n state)
      (occupy! n state))
   #f)

(define-method (propagate n::FSM-start state str index)
   (with-access::FSM-start n (next offset)
      (with-access::FSM-state state (start-index)
	 ;; start-index may be updated without duplicating.
	 ;; CARE: this might not be true anymore when we implement Knuth-Morris
	 ;; algo.
	 (set! start-index (-fx index offset)))
      (propagate-check next state str index)))

(define-method (propagate n::FSM-final state str index)
   (unless (off-limit? n state) ;; final node could be 'forbidden'
      (with-access::FSM-state state (still-needed?)
	 ;; still-needed? is irrelevant, as we will abort propagation.
	 ;; yes. this is a hack...
	 (set! still-needed? #f)
	 (let ((my-state (modifiable-state state)))
	    (with-access::FSM-state my-state (final-index)
	       ;; do not modify if we have already set the index.
	       (unless (>= final-index 0)
		  (set! final-index index)))
	    ;; there cannot be any other node here. so just put us into the
	    ;; queue.
	    (occupy! n my-state)
	    ;; return #t to indicate we reached final ...
	    #t))))

(define (FSM-disjunction->FSM-opt-disjunction! n::FSM-disjunction)
   (define (consuming? n)
      (when (and (FSM-disjunction? n)
		 (not (FSM-opt-disjunction? n)))
	 (FSM-disjunction->FSM-opt-disjunction! n))

      (or (FSM-consuming? n)
	  (and (FSM-opt-disjunction? n)
	       (with-access::FSM-opt-disjunction n (all-consuming?)
		  all-consuming?))))

   (widen!::FSM-opt-disjunction n)
   (with-access::FSM-opt-disjunction n (next alternatives all-consuming?)
      (when (and (consuming? next)
		 (every? consuming? alternatives))
	 (set! all-consuming? #t))))
   
(define-method (propagate n::FSM-disjunction state str index)
   (FSM-disjunction->FSM-opt-disjunction! n)
   (propagate n state str index))
   
(define-method (propagate n::FSM-opt-disjunction state str index)
   (with-access::FSM-opt-disjunction n (next alternatives all-consuming?)
      (cond
	 ((and all-consuming?
	       (not (forbidden? next))
	       (not (any? forbidden? alternatives)))
	  (occupy! n state)
	  #f)
	 (else
	  (with-access::FSM-state state (still-needed?)
	     (let ((old-still-needed? still-needed?))
		(set! still-needed? #t)
		(or (propagate-check next state str index)
		    (let loop ((alts alternatives))
		       (cond
			  ((null? alts)
			   ;; should never happen.
			   (set! still-needed? old-still-needed?)
			   'done)
			  ((null? (cdr alts))
			   (set! still-needed? old-still-needed?)
			   (propagate-check (car alts) state str index))
			  (else
			   (or (propagate-check (car alts) state str index)
			       (loop (cdr alts)))))))))))))

(define-method (propagate n::FSM-repeat-entry state str index)
   (with-access::FSM-repeat-entry n (repeat-exit next)
      (let ((my-state (modifiable-state state)))
	 (with-access::FSM-state my-state (loops)
	    (set! loops (cons (instantiate::FSM-loop-info
				 (iterations 0)
				 (index-time index)
				 (loop-exit repeat-exit))
			      loops))
	    (propagate-check next my-state str index)))))

(define-method (propagate n::FSM-repeat-exit state str index)
   (with-access::FSM-repeat-exit n (loop-body next min max greedy?)

      (define (repeat state new-count)
	 (let ((my-state (modifiable-state state)))
	    (with-access::FSM-state my-state (loops)
	       ;; replace first loop-info
	       (set! loops (cons (instantiate::FSM-loop-info
				    (iterations new-count)
				    (index-time index)
				    (loop-exit n))
				 (cdr loops)))
	       (propagate-check loop-body my-state str index))))

      (define (leave state)
	 (let ((my-state (modifiable-state state)))
	    (with-access::FSM-state my-state (loops)
	       ;; we are leaving. -> remove loop-info
	       (set! loops (cdr loops))
	       (propagate-check next my-state str index))))

      (with-access::FSM-state state (loops)
	 (with-access::FSM-loop-info (car loops) (iterations index-time)
	    (let* ((empty-match? (=fx index-time index))
		   (new-count (+fx iterations 1))
		   (reached-min? (>=fx new-count min))
		   (over-min? (>fx new-count min))
		   (reached-max? (and max
				      (=fx new-count max))))
	       (cond
		  ((and empty-match? over-min?)
		   ;; empty matches are only allowed when we have not yet
		   ;; matched the minimal number of repetitions
		   ;;
		   ;; I think this can not happen anymore, but the test is cheap.
		   'do-nothing)
		  ((and empty-match?
			(< new-count min)) ;; at least 1 empty iter is allowed.
		   
		   ;; ok. special treatment...
		   ;; let e be the empty-string and m a match of one (or more
		   ;; chars). then there is no difference between
		   ;; between e{i}me{j} and e{k}me{l} (with j and l > 0)
		   ;; (for any i,j,k,l) with both matching min-iterations.
		   ;; in other words: we can repeat the empty-string before or
		   ;; after matches in any order. there is however a difference
		   ;; for e{x}m (again getting min-iterations). As this time
		   ;; clusters might have different values.
		   ;; Hence: one repetition with i=0. Another with i=max-1, one
		   ;; with i=max, and one leaving.
		   ;; if we are greedy then the order is:
		   ;;     i=0, i=max-1, i=max, leaving, i=max-1, i=0
		   ;;
		   ;; Due to our treatment, we can forbid this node and avoid
		   ;; further repetitions.
		   (with-access::FSM-state state (still-needed?)
		      (let ((old-still-needed? still-needed?))
			 (set! still-needed? #t)
			 ;; the final node cannot be inside the loop.
			 ;; -> as we set the forbidden here only leaving can
			 ;; lead to the final node.
			 (forbidden?-set! n #t)
			 (repeat state new-count)
			 (unless (=fx new-count (-fx min 1))
			    (repeat state (-fx min 1)))
			 (if greedy?
			     (begin
				(repeat state min)
				(forbidden?-set! n #f)
				(set! still-needed? old-still-needed?)
				(leave state))
			     (begin
				(forbidden?-set! n #f)
				;; leave could reach the final node.
				(or (leave state)
				    (begin
				       (forbidden?-set! n #t)
				       (set! still-needed? old-still-needed?)
				       (repeat state min)
				       (forbidden?-set! n #f))))))))
		  ((not reached-min?)
		   ;; we have not yet reached the min-amount.
		   ;; -> we have to repeat.
		   (repeat state new-count))
		  (reached-max?
		   ;; reached max nb of iterations. -> can't repeat.
		   (leave state))
		  (else
		   (with-access::FSM-state state (still-needed?)
		      (let ((old-still-needed? still-needed?))
			 (set! still-needed? #t)
			 (if greedy?
			     (or (repeat state new-count)
				 (begin
				    (set! still-needed? old-still-needed?)
				    (leave state)))
			     (begin
				(or (leave state)
				    (begin
				       (set! still-needed? old-still-needed?)
				       (repeat state new-count))))))))))))))

(define-method (propagate n::FSM-non-empty state str index)
   (with-access::FSM-non-empty n (next other)
      (let ((old-forbidden? (forbidden? other)))
	 (forbidden?-set! other #t)
	 (let ((tmp (propagate-check next state str index)))
	    (forbidden?-set! other old-forbidden?)
	    tmp))))

(define-method (propagate n::FSM-backref state str index)
   (if (FSM-sleeping-state? state)
       ;; do not occupy (as this would conflict with other states) but still
       ;; push the state, as it is still alive.
       (begin
	  (push-state! state)
	  #f)
       (with-access::FSM-backref n (backref-nb next case-sensitive?)
	  (with-access::FSM-state state (backref-clusters)
	     (let* ((tmp (*fx backref-nb 2))
		    (start (vector-ref backref-clusters tmp))
		    (stop (vector-ref backref-clusters (+fx tmp 1)))
		    (prefix? (if case-sensitive?
				 string-prefix?
				 string-prefix-ci?)))
		;; as the start is only updated when there is a stop
		;; (see FSM-backref-cluster-exit) then there must be a stop
		;; if there's a start.
		(cond
		   ((or (not start)
			(not stop)
			(=fx start stop)) ;; empty string
		    (propagate-check next state str index))
		   ((>=fx index (string-length str))
		    ;; can't match, but string-prefix? would crash
		    'do-nothing)
		   ((prefix? str str start stop index)
		    ;; node has now to wait stop-start before it can continue.
		    (widen!::FSM-sleeping-state state
		       (cycles-to-sleep (-fx stop start)))
		    ;; for the first iteration we can say, that we occupy this
		    ;; node. after that no.
		    (unless (off-limit? n state)
		       (occupy! n state))
		    #f)
		   (else
		    ;; no match. nothing to do
		    'do-nothing)))))))

(define-method (propagate n::FSM-assert state str index)
   (with-access::FSM-assert n (next condition)
      (when (condition str index)
	 (propagate-check next state str index))))

(define-method (propagate n::FSM-condition state str index)
   (unless (FSM-sleeping-state? state)
      (with-access::FSM-condition n (next condition)
	 (let ((res (condition str index)))
	    (cond
	       ((not res)
		#f) ;; do nothing
	       ((=fx res 0)
		(propagate-check next state str index))
	       (else
		(widen!::FSM-sleeping-state state
		   (cycles-to-sleep res))
		;; for the first iteration we can occupy this
		;; node. after that no.
		(unless (off-limit? n state)
		   (occupy! n state))
		#f))))))

(define (state-with-modifiable-clusters::FSM-state s::FSM-state)
   (with-access::FSM-state s (clusters sharing-clusters?)
      (cond
	 ((not sharing-clusters?)
	  s)
	 ((not (modifiable? s))
	  (state-with-modifiable-clusters (modifiable-state s)))
	 (else
	  (set! clusters (copy-vector clusters (vector-length clusters)))
	  s))))
(define (state-with-modifiable-br-clusters::FSM-state s::FSM-state)
   (with-access::FSM-state s (backref-clusters sharing-br-clusters?)
      (cond
	 ((not sharing-br-clusters?)
	  s)
	 ((not (modifiable? s))
	  (state-with-modifiable-clusters (modifiable-state s)))
	 (else
	  (set! backref-clusters
		(copy-vector backref-clusters
			     (vector-length backref-clusters)))
	  s))))

(define-method (propagate n::FSM-cluster state str index)
   (with-access::FSM-cluster n (next cluster-index backref-cluster-index)
      (let ((my-state (state-with-modifiable-clusters state)))
	 (with-access::FSM-state my-state (clusters backref-clusters)
	    (vector-set! clusters cluster-index index)
	    (if backref-cluster-index
		(let ((my-state2 (state-with-modifiable-br-clusters my-state)))
		   (vector-set! backref-clusters backref-cluster-index index)
		   (propagate-check next my-state2 str index))
		(propagate-check next my-state str index))))))

(define-method (propagate n::FSM-cluster-clear state str index)
   ;; search for the first entry in vector v[from..to] that is not #f
   (define (find-not-false v from to)
      (cond
	 ((>= from to)
	  #f)
	 ((vector-ref v from)
	  from)
	 (else
	  (find-not-false v (+fx from 1) to))))

   ;; set v[from..to] to false
   (define (set-to-false v from to)
      (cond
	 ((>= from to)
	  'done)
	 (else
	  (vector-set! v from #f)
	  (set-to-false v (+fx from 1) to))))

   (with-access::FSM-cluster-clear n (next start-index stop-index
					   backref-start-index
					   backref-stop-index)
      (let ((my-state state))
	 (let ((i (find-not-false (FSM-state-clusters my-state)
				  start-index stop-index)))
	    (when i
	       (set! my-state (state-with-modifiable-clusters my-state))
	       (set-to-false (FSM-state-clusters my-state)
			     i stop-index)))
	 (when backref-start-index
	    (let ((i (find-not-false (FSM-state-backref-clusters my-state)
				     backref-start-index
				     backref-stop-index)))
	       (when i
		  (set! my-state (state-with-modifiable-br-clusters my-state))
		  (set-to-false (FSM-state-backref-clusters my-state)
				i backref-stop-index))))
      (propagate-check next my-state str index))))

(define-method (propagate n::FSM-cluster-assert state str index)
   (with-access::FSM-cluster-assert n (entry next contains-clusters? negative?)
      ;; perform test of next node, to avoid expensive checks.
      (unless (if (or negative? (not contains-clusters?))
		  ;; if negative, then backrefs won't change, and we can use
		  ;; target-off-limit?.
		  (off-limit? next state)
		  (forbidden? next)) ;; otherwise only look at 'forbidden?'
	 (let ((my-state (modifiable-state state)))
	    ;; we do not need to duplicate if the state is just 'still-needed'
	    ;; but I don't care...
	    (with-access::FSM-state my-state (node)
	       (set! node entry)
	       
	       (if (or negative?
		       (not contains-clusters?))
		   (let ((matched? (fresh-recursive-test *fsm* my-state
							 str index)))
		      (cond
			 ((or (and negative? (not matched?))
			      (and (not negative?) matched?))
			  (propagate-check next my-state str index))
			 (else
			  #f))) ;; do nothing. assert failed
		   ;; we need the real state. -> more expensive...
		   (let ((match-state (fresh-recursive-match *fsm* my-state
							     str index)))
		      (when match-state
			 (propagate-check next match-state str index)))))))))

;; just consumed a character. now waiting for propagation in the next round.
(define (wait-at n::FSM-node state::FSM-state)
   (with-access::FSM-state state (node)
      (set! node n)
      (push-state! state)))

;; 'advance' may _not_ change a state! we have propagate for that...
(define (advance n::FSM-node state::FSM-state str::bstring index::bint)
   (let ((next (consume n state str index)))
      (when next
	 (wait-at next state))))
   
;; returns #f if can't consume.
;; otherwise returns the target where the state should go.
;; allows for instance for dispatching nodes.
;; must _not_ change a state! we have propagate for that...
(define-generic (consume n::FSM-node state::FSM-state str::bstring index::bint)
   (error "FSM-match"
	  "forgot node-type"
	  n))

;; we are here, if all alternatives are consuming.
;; we will only duplicate the state if the state can actually consume a
;; character. this should considerably reduce the duplications.
;;
;; currently a bit hacky as it does the job of 'wait-at' too...
(define-method (consume n::FSM-disjunction state str index)
   (with-access::FSM-disjunction n (next alternatives)
      (let ((first-next (consume next state str index)))
	 (when first-next
	    (wait-at first-next state))
	 (let loop ((choices alternatives)
		    (need-to-dupl? first-next))
	    (cond
	       ((null? choices)
		#f) ;; do not return any node
	       ((consume (car choices) state str index)
		=>
		(lambda (choice-next)
		   (if need-to-dupl?
		       (let ((dupl (duplicate::FSM-state state)))
			  (wait-at choice-next dupl))
		       (wait-at choice-next state))
		   (loop (cdr choices)
			 #t)))
	       (else
		(loop (cdr choices)
		      need-to-dupl?)))))))

(define-method (consume n::FSM-KMP state str index)
   (with-access::FSM-KMP n (next fail)
      ;; try to consume 'next'. If it does not work try fail.
      (or (consume next state str index)
	  (consume fail state str index))))

(define-method (consume n::FSM-final state str index)
   ;; always consumes a char, and puts the state back to this node.
   n)

(define-method (consume n::FSM-0-cost state str index)
   (error "FSM-match"
	  "0-cost node to try to consume"
	  n))

(define-method (consume n::FSM-sleeping state str index)
   ;; must be a sleeping node.
   (with-access::FSM-node n (next)
      (with-access::FSM-sleeping-state state (cycles-to-sleep)
	 (cond
	    ((=fx cycles-to-sleep 1)
	     ;; we match the last character and propagate.
	     (shrink! state)
	     next)
	    (else
	     ;; just update the sleep-cycles and put us back into the queue.
	     (set! cycles-to-sleep (-fx cycles-to-sleep 1))
	     ;; wait here again.
	     n)))))

(define-method (consume n::FSM-char state str index)
   (with-access::FSM-char n (next c)
      (and (char=? c (string-ref str index))
	   next)))

(define-method (consume n::FSM-everything state str index)
   (with-access::FSM-char n (next)
      next))

(define-method (consume n::FSM-class state str index)
   (with-access::FSM-class n (next class)
      (and (RegExp-class-match class (string-ref str index))
	   next)))
)

