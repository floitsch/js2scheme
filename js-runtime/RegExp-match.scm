(module jsre-RegExp-match
   (import jsre-RegExp-classes
	   jsre-RegExp-fsm
	   jsre-RegExp-dot
	   jsre-RegExp-state)
   (import multi-top-level)
   (export (regexp-run fsm::FSM str::bstring))
   (static
    (class Node-use
       (occupied-by::pair-nil (default '()))
       (forbidden?::bool (default #f)))
    (class Globals
       *fsm*
       *node-uses*
       *frozen-states*::pair-nil
       *nb-frozen-layers*::bint
       *rev-next-round*::pair-nil
       *reached-final*)
    ))

;; for a description of the process look at RegExp-fsm

(multi-top-level (class Globals
		    *fsm*
		    *node-uses*
		    *frozen-states*::pair-nil
		    *nb-frozen-layers*::bint
		    *rev-next-round*::pair-nil
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

(define (regexp-run fsm str)
   (set! *fsm* fsm)
   (with-access::FSM fsm (entry nb-nodes nb-clusters nb-backref-clusters)
      (let* ((state (instantiate::FSM-state
		       (clusters (make-vector (* nb-clusters 2) #f))
		       (backref-clusters (make-vector (* nb-backref-clusters 2)
						      #f))
		       (node entry))))
	 (node-uses-init! nb-nodes)
	 (let ((match (run (list state) str 0)))
	    (and match
		 (cons (substring str 0 (FSM-state-final-index match))
		       (FSM-state-clusters match)))))))

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

(define *rev-next-round* '())
(define (push-state! s)
   (set! *rev-next-round* (cons s *rev-next-round*)))

(define (clear-visitors!)
   (for-each
    (lambda (state)
       (with-access::FSM-state state (node)
	  (occupied-by-clear! node)))
    *rev-next-round*))


(define *max-parallel-states* 1000)

(define (next-round-states! index) ;; get pushed states
   (let* ((next-round-states (reverse! *rev-next-round*))
	  (len (length next-round-states)))
      (set! *rev-next-round* '())

      ;; HACK: hard-coded ad-hoc algo to cut next-round-states...
      (when (> (bit-lsh len *nb-frozen-layers*)
	       *max-parallel-states*)
	 (let ((keep-nb (max (bit-rsh *max-parallel-states*
				      (+ *nb-frozen-layers* 1))
			     1)))
	    (freeze! next-round-states index keep-nb)))

      (if (null? next-round-states)
	  (restore-waiting-states!)
	  (values next-round-states index))))

(define *debug* #f)

(define *reached-final* #unspecified)

;; run one iteration at a time
(define (run states str index)
   (when *debug*
      (with-output-to-file (format "~a.dot" index)
	 (lambda ()
	    (running->dot *fsm* states *frozen-states*
			  (substring str 0 index)))))

   ;; first propagate states.
   (bind-exit (reached-final)
      (set! *reached-final* reached-final)
      ;; 
      (for-each (lambda (state)
		   (with-access::FSM-state state (node)
		      (propagate node state str index)))
		states))
   ;; then clear any information we left at the nodes.
   (clear-visitors!)

   ;; get the states for the next round.
   ;; this might defrost some old states if no state was left.
   (receive (new-states new-index)
      (next-round-states! index)
      (cond
	 ((null? new-states)
	  #f) ;; no match
	 ((FSM-final? (FSM-state-node (car states)))
	  ;; first state in priority-list is final. Can't get any better...
	  (car states))
	 ((>=fx index (string-length str))
	  ;; end of string. If there's a state pointing to the final-node we
	  ;; have a winner.
	  (let ((winner (any (lambda (state)
				(and (FSM-final? (FSM-state-node state))
				     state))
			     states)))
	     ;; if there's a winner return it. otherwise see if there are still
	     ;; states waiting (by rerunning the procedure).
	     (or winner
		 (run '() str index))))
	 (else ;; consume thus advancing to the next index
	  (for-each (lambda (state)
		       (with-access::FSM-state state (node)
			  (advance node state str index)))
		    states)
	  (run new-states str (+fx index 1))))))

;; target is off-limit if it is occupied by a state with same backref-cluster,
;; or if it is forbidden.
(define (off-limit? n::FSM-node state::FSM-state)
   (define (same-brefs? backrefs1 backrefs2)
      (equal? backrefs1 backrefs2))
      
   (define (better-loops? loops1 loops2) ;; either the same or l1 beats l2.
      (or (eq? loops1 loops2)
	  (every? (lambda (l1 l2)
		     (with-access::FSM-loop-info l1 (loop-exit)
			(with-access::FSM-repeat-exit loop-exit (min max
								     greedy?)
			   ;; both must be in same loops. so no need to get
			   ;; loop-exit from l2
			   (let ((it1 (FSM-loop-info-iterations l1))
				 (it2 (FSM-loop-info-iterations l2)))
			      (or (=fx it1 it2)
				  (and greedy?
				       (not max)
				       (>fx it1 it2))
				  (and (not greedy?)
				       (>=fx it1 min)
				       (<fx it1 it2)))))))
		  loops1
		  loops2)))

   (or (forbidden? n)
       (any? (lambda (other-state)
		(and (same-brefs? (FSM-state-backref-clusters state)
				  (FSM-state-backref-clusters other-state))
		     (better-loops? (FSM-state-loops state)
				    (FSM-state-loops other-state))))
	     (occupied-by n))))

(define (occupy! n::FSM-node state::FSM-state)
   (with-access::FSM-state state (node)
      (set! node n)
      (occupied-by-push! n state)
      (push-state! state)))


;; At the beginning of every iteration all contained clusters must be reset.
(define (clear-clusters! state::FSM-state loop-exit::FSM-loop-exit)
   (with-access::FSM-loop-exit loop-exit (clusters-begin clusters-end)
      (with-access::FSM-state state (clusters)
	 ;; lazy copy. If none of the clusters is set, no need to duplicate the
	 ;; vector.
	 (let loop ((i clusters-begin)
		    (copied? #f))
	    (cond
	       ((>= i clusters-end)
		'done)
	       ((and (not copied?)
		     (vector-ref clusters i))
		(set! clusters
		      (copy-vector clusters (vector-length clusters)))
		(loop i #t))
	       (else
		(vector-set! clusters i #f)
		(loop (+fx i 1) copied?)))))))
   

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
	     (loop choices)))))

(define-generic (propagate n::FSM-node state::FSM-state
			   str::bstring index::bint)
   (error "FSM-match"
	  "forgot node-type"
	  n))

(define-method (propagate n::FSM-consuming state str index)
   (unless (off-limit? n state)
      (occupy! n state)))

(define-method (propagate n::FSM-final state str index)
   (unless (off-limit? n state)
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
	 (clear-clusters! state n)
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
	    (clear-clusters! state n)
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
      (let ((old-forbidden? forbidden?))
	 (forbidden?-set! other #t)
	 (propagate-check next state str index)
	 (forbidden?-set! other old-forbidden?))))

(define-method (propagate n::FSM-backref state str index)
   (unless (FSM-sleeping-state? state)
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

(define-method (propagate n::FSM-cluster-entry state str index)
   (with-access::FSM-cluster-entry n (next cluster-index)
      (with-access::FSM-state state (clusters)
	 ;; copy on write
	 (set! clusters (copy-vector clusters (vector-length clusters)))
	 (vector-set! clusters cluster-index index)
	 (propagate-check next state str index))))

(define-method (propagate n::FSM-cluster-exit state str index)
   (with-access::FSM-cluster-exit n (next cluster-index backref-exit-index)
      (with-access::FSM-state state (clusters backref-clusters)
	 ;; copy on write
	 (set! clusters (copy-vector clusters (vector-length clusters)))
	 (vector-set! clusters cluster-index index)

	 (when backref-exit-index
	    ;; copy on write
	    (set! backref-clusters
		  (copy-vector backref-clusters
			       (vector-length backref-clusters)))
	    ;; update the exit-index
	    (vector-set! backref-clusters backref-exit-index index)
	    ;; and update the entry-index too.
	    ;; we can't update the backref-cluster at entering, as this would
	    ;; not allow to match for instance /(b\1|ab)*/.exec('abbab')
	    (vector-set! backref-clusters (-fx backref-exit-index 1)
			 (vector-ref clusters (-fx cluster-index 1))))

	 (propagate-check next state str index))))

(define-method (propagate n::FSM-cluster-assert state str index)
   (with-access::FSM-cluster-assert n (entry next negative?)
      ;; perform test of next node, to avoid expensive checks.
      (unless (if negative?
		  ;; if negative, then backrefs won't change, and we can use
		  ;; target-off-limit?.
		  (off-limit? next state)
		  (forbidden? next)) ;; otherwise only look at 'forbidden?'
	 (with-access::FSM-state state (node)
	    ;; TODO: this must not be here!!
	    (let ((old-frozen *frozen-states*)
		  (old-rev-next-round *rev-next-round*))
	       (set! *frozen-states* '())
	       (set! *rev-next-round* '())
	       (set! node entry)
	       (let ((match (run (list state) str index)))
		  (set! *frozen-states* old-frozen)
		  (set! *rev-next-round* old-rev-next-round)
		  (cond
		     ((or (and negative? (not match))
			  (and (not negative?) match))
		      (propagate next state str index)) ;; no need for check.
		     (else
		      'nothing-to-do)))))))) ;; assert failed

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
   ;; there cannot be any other node here. so just put us into the queue.
   ;; there can not be any other node in the queue. so nothing else to do.
   (occupy! n state))

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
		(push-state! state))))))

(define-method (advance n::FSM-char state str index)
   (with-access::FSM-char n (next c)
      (when (char=? c (string-ref str index))
	 (wait-at next state))))

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
