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
    (class Node-use
       (occupied-by::pair-nil (default '()))
       (forbidden?::bool (default #f)))
    (class Globals
       *fsm*
       *node-uses*
       *frozen-states*::pair-nil
       *nb-frozen-layers*::bint
       *rev-pushed-states*::pair-nil
       *reached-final*)
    ))

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
 		    *rev-pushed-states*::pair-nil
		    *reached-final*)
		 
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
			(cons (substring str start-index final-index)
			      clusters))))))))

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
      (when (> (bit-lsh len *nb-frozen-layers*)
	       *max-parallel-states*)
	 (let ((keep-nb (max (bit-rsh *max-parallel-states*
				      (+ *nb-frozen-layers* 1))
			     1)))
	    (freeze! next-round-states index keep-nb)))

      (if (and (null? next-round-states)
	       (not (zero? *nb-frozen-layers*)))
	  (restore-waiting-states!)
	  (values next-round-states index))))

(define *reached-final* #unspecified)

;; run one iteration at a time
(define (run states str index only-test?)
   ;; first propagate states.
   (let ((reached-final?
	  (bind-exit (reached-final)
	     (set! *reached-final* reached-final)
	     ;; 
	     (for-each (lambda (state)
			  (with-access::FSM-state state (node)
			     (propagate node state str index)))
		       states)
	     #f)))
      
      ;; then clear any information we left at the nodes.
      (clear-visitors!)

      ;; get the states for the next round.
      ;; this might defrost some old states if no state was left.
      (receive (new-states new-index)
	 (next-round-states! index)
	 (when *debug*
	    (with-output-to-file (format "~a.dot" index)
	       (lambda ()
		  (running->dot *fsm* new-states *frozen-states*
				str index))))
	 (cond
	    ((and only-test? reached-final?)
	     #t) ;; we reached a final node. no need to look which state.
	    ((null? new-states)
	     #f) ;; no match
	    ((FSM-final? (FSM-state-node (car new-states)))
	     ;; first state in priority-list is final. Can't get any better...
	     (car new-states))
	    ((>=fx index (string-length str))
	     ;; end of string. If there's a state pointing to the final-node we
	     ;; have a winner.
	     (let ((winner (any (lambda (state)
				   (and (FSM-final? (FSM-state-node state))
					state))
				new-states)))
		;; if there's a winner return it. otherwise see if there are still
		;; states waiting (by rerunning the procedure).
		(or winner
		    (run '() str index only-test?))))
	    (else ;; consume thus advancing to the next index
	     (for-each (lambda (state)
			  (with-access::FSM-state state (node)
			     (advance node state str index)))
		       new-states)
	     (let ((live-states (reverse! *rev-pushed-states*)))
		(set! *rev-pushed-states* '())
		(run live-states str (+fx index 1) only-test?)))))))

;; target is off-limit if it is occupied by a state with same backref-cluster,
;; or if it is forbidden.
(define (off-limit? n::FSM-node state::FSM-state)
   ;; defender is the state occupying the node
   ;; attacker is the new state.
   ;; --------

   (define (same-brefs? backrefs-defending backrefs-attacker)
      (equal? backrefs-attacker backrefs-defending))

   ;; either the same or defending beats attacker.
   ;; in this case the attacker just can't get better then the defender.
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
   (with-access::FSM-state state (node)
      (set! node n)
      (occupied-by-push! n state)
      (push-state! state)))


(define (propagate-check n::FSM-node state::FSM-state
			 str::bstring index::bint)
   (unless (forbidden? n)
      (propagate n state str index)))

(define (propagate-in-order choices state str index)
   ;; be careful: during propagation the nodes might change.

   (let loop ((choices choices))
      (cond
	 ((null? choices) ;; empty choices list...
	  'done)
	 ((null? (cdr choices)) ;; last entry, so we can use orig now.
	  (propagate-check (car choices) state str index))
	 (else
	  (let ((dupl (duplicate::FSM-state state)))
	     (propagate-check (car choices) dupl str index))
	     (loop (cdr choices))))))

(define-generic (propagate n::FSM-node state::FSM-state
			   str::bstring index::bint)
   (error "FSM-match"
	  "forgot node-type"
	  n))

(define-method (propagate n::FSM-consuming state str index)
   (unless (off-limit? n state)
      (occupy! n state)))

(define-method (propagate n::FSM-start state str index)
   (with-access::FSM-start n (next)
      (with-access::FSM-state state (start-index)
	 (set! start-index index))
      (propagate-check next state str index)))

(define-method (propagate n::FSM-final state str index)
   (unless (off-limit? n state) ;; final node could be 'forbidden'
      (with-access::FSM-state state (final-index)
	 (set! final-index index))
      ;; clear frozen nodes. we have reached the final node, and all frozen
      ;; ones are of lower priority.
      (set! *frozen-states* '())
      ;; there cannot be any other node here. so just put us into the queue.
      (occupy! n state)
      (*reached-final* #t)))

(define-method (propagate n::FSM-disjunction state str index)
   (with-access::FSM-disjunction n (next alternatives)
      (propagate-in-order (cons next alternatives) state str index)))

(define-method (propagate n::FSM-? state str index)
   (with-access::FSM-? n (next exit greedy?)
      (let ((choices (if greedy?
			 (list next exit)
			 (list exit next))))
	 (propagate-in-order choices state str index))))

(define-method (propagate n::FSM-*-exit state str index)
   (with-access::FSM-*-exit n (loop-body greedy? next)
      (define (repeat state)
	 (forbidden?-set! n #t) ;; we don't allow empty iterations.
	 (propagate-check loop-body state str index)
	 (forbidden?-set! n #f))

      (define (leave state)
	 (propagate-check next state str index))

      (let ((dupl (duplicate::FSM-state state)))
	  (if greedy?
	      (begin
		 (repeat state)
		 (leave dupl))
	      (begin
		 (leave state)
		 (repeat dupl))))))

(define-method (propagate n::FSM-repeat-entry state str index)
   (with-access::FSM-repeat-entry n (repeat-exit next)
      (with-access::FSM-state state (loops)
	 (set! loops (cons (instantiate::FSM-loop-info
			      (iterations 0)
			      (index-time index)
			      (loop-exit repeat-exit))
			   loops))
	 (propagate-check next state str index))))

(define-method (propagate n::FSM-repeat-exit state str index)
   (with-access::FSM-repeat-exit n (loop-body next min max greedy?)

      (define (repeat state new-count)
	 (with-access::FSM-state state (loops)
	    ;; replace first loop-info
	    (set! loops (cons (instantiate::FSM-loop-info
				 (iterations new-count)
				 (index-time index)
				 (loop-exit n))
			      (cdr loops)))
	    (propagate-check loop-body state str index)))

      (define (leave state)
	 (with-access::FSM-state state (loops)
	    ;; we are leaving. -> remove loop-info
	    (set! loops (cdr loops))
	    (propagate-check next state str index)))

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
		   (forbidden?-set! n #t)
		   (let ((dupl (duplicate::FSM-state state)))
		      (repeat dupl new-count))
		   (unless (=fx new-count (-fx min 1))
		      (let ((dupl (duplicate::FSM-state state)))
			 (repeat dupl (-fx min 1))))
		   (let ((dupl (duplicate::FSM-state state)))
		      (if greedy?
			  (begin
			     (repeat state min)
			     (forbidden?-set! n #f)
			     (leave dupl))
			  (begin
			     (forbidden?-set! n #f)
			     (leave state)
			     (forbidden?-set! n #t)
			     (repeat dupl min)
			     (forbidden?-set! n #f)))))
		  ((not reached-min?)
		   ;; we have not yet reached the min-amount.
		   ;; -> we have to repeat.
		   (repeat state new-count))
		  (reached-max?
		   ;; reached max nb of iterations. -> can't repeat.
		   (leave state))
		  (else
		   (let ((dupl (duplicate::FSM-state state)))
		      (if greedy?
			  (begin
			     (repeat state new-count)
			     (leave dupl))
			  (begin
			     (leave state)
			     (repeat dupl new-count)))))))))))

(define-method (propagate n::FSM-non-empty state str index)
   (with-access::FSM-non-empty n (next other)
      (let ((old-forbidden? (forbidden? other)))
	 (forbidden?-set! other #t)
	 (propagate-check next state str index)
	 (forbidden?-set! other old-forbidden?))))

(define-method (propagate n::FSM-backref state str index)
   (if (FSM-sleeping-state? state)
       ;; do not occupy (as this would conflict with other states) but still
       ;; push the state, as it is still alive.
       (push-state! state)
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
		       (occupy! n state)))
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
		'do-nothing)
	       ((=fx res 0)
		(propagate-check next state str index))
	       (else
		(widen!::FSM-sleeping-state state
		   (cycles-to-sleep res))
		;; for the first iteration we can occupy this
		;; node. after that no.
		(unless (off-limit? n state)
		   (occupy! n state))))))))

(define-method (propagate n::FSM-cluster state str index)
   (with-access::FSM-cluster n (next cluster-index backref-cluster-index)
      (with-access::FSM-state state (clusters backref-clusters)
	 ;; copy on write
	 (set! clusters (copy-vector clusters (vector-length clusters)))
	 (vector-set! clusters cluster-index index)
	 (when backref-cluster-index
	    ;; copy on write
	    (set! backref-clusters
		  (copy-vector backref-clusters
			       (vector-length backref-clusters)))
	    (vector-set! backref-clusters backref-cluster-index index))
	 (propagate-check next state str index))))

(define-method (propagate n::FSM-cluster-clear state str index)
   (with-access::FSM-cluster-clear n (next start-index stop-index
					   backref-start-index
					   backref-stop-index)
      (with-access::FSM-state state (clusters backref-clusters)
	 ;; lazy copy. If none of the clusters is set, no need to duplicate the
	 ;; vector.
	 (let loop ((i start-index)
		    (copied? #f))
	    (cond
	       ((>= i stop-index)
		'done)
	       ((and (not copied?)
		     (vector-ref clusters i))
		(set! clusters
		      (copy-vector clusters (vector-length clusters)))
		(loop i #t))
	       (else
		(vector-set! clusters i #f)
		(loop (+fx i 1) copied?))))
	 
	 (when backref-start-index
	    (let loop ((i backref-start-index)
		       (copied? #f))
	       (cond
		  ((>= i backref-stop-index)
		   'done)
		  ((and (not copied?)
			(vector-ref backref-clusters i))
		   (set! backref-clusters
			 (copy-vector backref-clusters
				      (vector-length backref-clusters)))
		   (loop i #t))
		  (else
		   (vector-set! backref-clusters i #f)
		   (loop (+fx i 1) copied?))))))
      (propagate-check next state str index)))

(define-method (propagate n::FSM-cluster-assert state str index)
   (with-access::FSM-cluster-assert n (entry next contains-clusters? negative?)
      ;; perform test of next node, to avoid expensive checks.
      (unless (if (or negative? (not contains-clusters?))
		  ;; if negative, then backrefs won't change, and we can use
		  ;; target-off-limit?.
		  (off-limit? next state)
		  (forbidden? next)) ;; otherwise only look at 'forbidden?'
	 (with-access::FSM-state state (node)
	    (set! node entry)

	    (if (or negative?
		    (not contains-clusters?))
		(let ((matched? (fresh-recursive-test *fsm* state str index)))
		   (cond
		      ((or (and negative? (not matched?))
			   (and (not negative?) matched?))
		       (propagate-check next state str index))
		      (else
		       'nothing-to-do))) ;; assert failed
		;; we need the real state. -> more expensive...
		(let ((match-state (fresh-recursive-match *fsm* state str index)))
		   (when match-state
		      (propagate-check next match-state str index))))))))

;; just consumed a character. now waiting for propagation in the next round.
(define (wait-at n::FSM-node state::FSM-state)
   (with-access::FSM-state state (node)
      (set! node n)
      (push-state! state)))

(define-generic (advance n::FSM-node state::FSM-state str::bstring index::bint)
   (error "FSM-match"
	  "forgot node-type"
	  n))

(define-method (advance n::FSM-final state str index)
   ;; just put us back in the waiting-list of this node.
   ;; there might be several nodes waiting now. but during propagate only one
   ;; will survive.
   (wait-at n state))

(define-method (advance n::FSM-0-cost state str index)
   (error "FSM-match"
	  "0-cost node to be advanced"
	  n))

(define-method (advance n::FSM-backref state str index)
   ;; must be a sleeping node.
   (with-access::FSM-backref n (next)
      (with-access::FSM-sleeping-state state (cycles-to-sleep)
	 (if (=fx cycles-to-sleep 1)
	     (begin
		;; we match the last character and propagate.
		(shrink! state)
		(wait-at next state))
	     (begin
		;; just update the sleep-cycles and put us back into the queue.
		(set! cycles-to-sleep (-fx cycles-to-sleep 1))
		;; wait here again.
		(wait-at n state))))))

(define-method (advance n::FSM-char state str index)
   (with-access::FSM-char n (next c)
      (when (char=? c (string-ref str index))
	 (wait-at next state))))

(define-method (advance n::FSM-every-char state str index)
   (with-access::FSM-char n (next)
      (wait-at next state)))

(define-method (advance n::FSM-class state str index)
   (with-access::FSM-class n (next class)
      (when (RegExp-class-match class (string-ref str index))
	 (wait-at next state))))

(define-method (advance n::FSM-condition state str index)
   (with-access::FSM-condition n (next)
      (with-access::FSM-sleeping-state state (cycles-to-sleep)
	 (if (=fx cycles-to-sleep 1)
	     (begin
		;; we match the last character and propagate.
		(shrink! state)
		(wait-at next state))
	     (begin
		;; just update the sleep-cycles and put us back into the queue.
		(set! cycles-to-sleep (-fx cycles-to-sleep 1))
		(push-state! state))))))
)
