(module jsre-RegExp-match
   (import jsre-RegExp-classes
	   jsre-RegExp-fsm
	   jsre-RegExp-dot
	   jsre-RegExp-state)
   (export (regexp-run fsm::FSM str::bstring)))

(define (make-eq-hashtable)
   (make-hashtable 5 #unspecified eq?))

;; for a description of the process look at RegExp-fsm

(define *contains-backrefs?* #f)

(define *fsm* #unspecified)
(define (regexp-run fsm str)
   (set! *fsm* fsm)
   (with-access::FSM fsm (entry nb-clusters nb-backref-clusters)
      (set! *contains-backrefs?* (not (zero? nb-backref-clusters)))
      (let* ((state (instantiate::FSM-state
		       (clusters (make-vector (* nb-clusters 2) #f))
		       (backref-clusters (make-vector (* nb-backref-clusters 2)
						      #f))
		       (node entry))))
	 (let ((match (run state str 0)))
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
   (for-each (lambda (state)
		(with-access::FSM-state state (node)
		   (with-access::FSM-node node (occupied-by)
		      (set! occupied-by '()))))
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
      next-round-states))

(define *debug* #f)

(define *reached-final* #unspecified)

(define (run state str index)
   (with-access::FSM-state state (node)
      (bind-exit (reached-final)
	 (set! *reached-final* reached-final)
	 (propagate node state str index)))
   (clear-visitors!)
   (let ((init-states (next-round-states! (-fx index 1))))
      (advance init-states str index)))
   
(define (advance states str index)
   (when *debug*
      (with-output-to-file (format "~a.dot" index)
	 (lambda ()
	    (running->dot *fsm* states *frozen-states*
			  (substring str 0 index)))))
   (cond
      ((and (null? states)
	    (null? *frozen-states*))
       #f) ;; no match
      ((null? states) ;; there are still waiting states.
       (receive (states index)
	  (restore-waiting-states!)
	  (advance states str (+fx index 1)))) ;; next round so increment
      ((FSM-final? (FSM-state-node (car states)))
       ;; first state in priority-list is final. Can't get any better...
       (car states))
      ((>=fx index (string-length str))
       ;; end of string. If there's a state pointing to the final-node we have
       ;; a winner.
       (let ((winner (any (lambda (state)
			     (and (FSM-final? (FSM-state-node state))
				  state))
			  states)))
	  ;; if there's a winner return it. otherwise see if there are still
	  ;; states  waiting (by rerunning the procedure).
	  (or winner
	      (advance '() str index))))
      (else ;; advance states
       (bind-exit (reached-final)
	  (set! *reached-final* reached-final)
	  (for-each (lambda (state)
		       (with-access::FSM-state state (node)
			  (node-advance node state str index)))
		    states))
       (clear-visitors!)
       (advance (next-round-states! index) str (+fx index 1)))))

(define-generic (node-advance n::FSM-node state::FSM-state
			    str::bstring index::bint)
   (error "FSM-match"
	  "forgot node-type"
	  n))

(define-method (node-advance n::FSM-final state str index)
   (with-access::FSM-node n (occupied-by)
      ;; clear frozen nodes. we have reached the final node, and all frozen
      ;; ones are of lower priority.
      (set! *frozen-states* '())
      ;; there cannot be any other node here. so just put us into the queue.
      (occupy-node! n state)
      (*reached-final* #t)
      ))

(define-method (node-advance n::FSM-simple state str index)
   ;; don't look for O-cost-transits. They are not relevant here.
   (with-access::FSM-simple n (transit)
      (when transit
	 (take-transit transit state str index))))

(define-method (node-advance n::FSM-non-empty state str index)
   ;; don't look for O-cost-transits. They are not relevant here.
   (with-access::FSM-non-empty n (exit transit O-cost-transit)
      (let ((t0 transit) (t1 O-cost-transit))
	 [assert (t0 t1) (not (and t0 t1))])
      (when transit
	  (take-transit transit state str index))))

(define-method (node-advance n::FSM-backref state str index)
   (with-access::FSM-backref n (exit)
      (with-access::FSM-sleeping-state state (cycles-to-sleep)
	 (if (=fx cycles-to-sleep 1)
	     (begin
		;; we match the last character and propagate.
		(shrink! state)
		(propagate exit state str (+fx index 1)))
	     (begin
		;; just update the sleep-cycles and put us back into the queue.
		(set! cycles-to-sleep (-fx cycles-to-sleep 1))
		(push-state! state))))))

;; FSM-disjunction, FSM-*-exit and FSM-? do not
;; have any node-advance. (propagation should never leave a state on one of these
;; nodes.

;; target is off-limit if it is occupied by a state with same backref-cluster,
;; or if it is forbidden.
(define (target-off-limit? n::FSM-node state::FSM-state)
   (define (same-brefs? backrefs1 backrefs2)
      (or (not *contains-backrefs?*)
	  (equal? backrefs1 backrefs2)))
      
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

   (with-access::FSM-node n (forbidden? occupied-by)
      (or forbidden?
	  (any? (lambda (other-state)
		   (and (same-brefs? (FSM-state-backref-clusters state)
				     (FSM-state-backref-clusters other-state))
			(better-loops? (FSM-state-loops state)
				       (FSM-state-loops other-state))))
		occupied-by))))

(define-generic (take-transit t::FSM-transit state str index)
   ;; O-cost-transit.
   (with-access::FSM-transit t (target)
      (unless (target-off-limit? target state)
	 (propagate target state str index))))

(define-method (take-transit t::FSM-char-transit state str index)
   (with-access::FSM-char-transit t (target c)
      (when (and (let ((c2 (string-ref str index)))
		    ;; char=? is cheaper than target-off-limit -> do it first
		    (char=? c c2))
		 (not (target-off-limit? target state)))
	 (propagate target state str (+fx index 1)))))

(define-method (take-transit t::FSM-class-transit state str index)
   (with-access::FSM-class-transit t (target class)
      (when (and (let ((c2 (string-ref str index)))
		    ;; RegExp-class-match should be faster than
		    ;; target-off-limit. -> do it first.
		    ;; TODO: verify speed of match/target-off-limit.
		    (RegExp-class-match class c2))
		 (not (target-off-limit? target state)))
	 (propagate target state str (+fx index 1)))))

(define-method (take-transit t::FSM-assert-transit state str index)
   (with-access::FSM-assert-transit t (target condition)
      (when (and (not (target-off-limit? target state))
		 (condition str index))
	     (propagate target state str index))))

(define-method (take-transit t::FSM-condition-transit state str index)
   (with-access::FSM-condition-transit t (target condition)
      (when (and (not (target-off-limit? target state))
		 (condition str index))
	 (propagate target state str (+fx index 1)))))

(define-method (take-transit t::FSM-cluster state str index)
   (with-access::FSM-cluster t (target cluster-index)
      (unless (target-off-limit? target state)
	 (with-access::FSM-state state (clusters)
	    ;; copy on write
	    (set! clusters (copy-vector clusters (vector-length clusters)))
	    (vector-set! clusters cluster-index index)
	    (propagate target state str index)))))

(define-method (take-transit t::FSM-backref-cluster-exit state str index)
   (with-access::FSM-backref-cluster-exit t (target cluster-index backref-index)
      (with-access::FSM-node target (forbidden?)
	 ;; we can't use the std target-off-limit? yet, as the backref-cluster
	 ;; change here...
	 (unless forbidden?
	    (with-access::FSM-state state (backref-clusters clusters)
	       ;; copy on write
	       (set! clusters (copy-vector clusters (vector-length clusters)))
	       (set! backref-clusters
		     (copy-vector backref-clusters
				  (vector-length backref-clusters)))
	       
	       ;; update the exits
	       (vector-set! clusters cluster-index index)
	       (vector-set! backref-clusters backref-index index)
	       ;; backref-cluster-exit has to update the backref-entry too.
	       (vector-set! backref-clusters
			    (-fx backref-index 1)
			    (vector-ref clusters (-fx cluster-index 1)))

	       (unless (target-off-limit? target state)
		  (propagate target state str index)))))))

(define-method (take-transit t::FSM-cluster-assert state str index)
   (with-access::FSM-cluster-assert t (entry exit negative? target)
      (with-access::FSM-node target (forbidden?)
	 ;; we can't use the std target-off-limit? yet, as the backref-cluster
	 ;; might change here.
	 (unless (if negative?   ;; if negative, then backrefs won't change
		     (target-off-limit? target state)
		     forbidden?) ;; otherwise we can only look at forbidden?
	    (with-access::FSM-state state (node)
	       ;; TODO: this must not be here!!
	       (let ((old-frozen *frozen-states*)
		     (old-rev-next-round *rev-next-round*))
		  (set! *frozen-states* '())
		  (set! *rev-next-round* '())
		  (set! node entry)
		  (let ((match (run state str index)))
		     (set! *frozen-states* old-frozen)
		     (set! *rev-next-round* old-rev-next-round)
		     (cond
			((and negative?
			      (not match))
			 (propagate target state str index))
			((and (not negative?)
			      match)
			 (unless (target-off-limit? target match)
			    (propagate target match str index)))
			(else
			 'nothing-to-do))))))))) ;; assert failed


(define-generic (propagate n::FSM-node state::FSM-state str::bstring
			   index::bint)
   (error "FSM-match"
	  "forgot node-type"
	  n))

(define (occupy-node! n::FSM-node state::FSM-state)
   (with-access::FSM-node n (occupied-by)
      (with-access::FSM-state state (node)
	 (set! node n)
	 (set! occupied-by (cons state occupied-by))
	 (push-state! state))))

(define-method (propagate n::FSM-final state str index)
   (with-access::FSM-state state (final-index)
      (set! final-index index))
      ;; clear frozen nodes. we have reached the final node, and all frozen
      ;; ones are of lower priority.
      (set! *frozen-states* '())
      ;; there cannot be any other node here. so just put us into the queue.
      (occupy-node! n state)
      (*reached-final* #t))

(define-method (propagate n::FSM-simple state str index)
   (with-access::FSM-simple n (transit O-cost-transit)
      (if O-cost-transit
	  ;; just move to the next node.
	  (take-transit O-cost-transit state str index)
	  (occupy-node! n state))))

(define-method (propagate n::FSM-non-empty state str index)
   (with-access::FSM-non-empty n (exit transit O-cost-transit)
      (if O-cost-transit
	  (with-access::FSM-node exit (forbidden?)
	     (let ((old-forbidden? forbidden?))
		(set! forbidden? #t)
		(take-transit O-cost-transit state str index)
		(set! forbidden? old-forbidden?)))
	  (occupy-node! n state))))

;; TODO: can be optimized... (RegExp: propagate-in-order)
;; the state does not need to be always duplicated.
(define (propagate-in-order choices state str index )
   (for-each (lambda (choice)
		(unless (target-off-limit? choice state)
		   (let ((dupl (duplicate::FSM-state state)))
		      (propagate choice dupl str index))))
	     choices))

(define-method (propagate n::FSM-disjunction state str index)
   (with-access::FSM-disjunction n (alternatives)
      (propagate-in-order alternatives state str index)))

(define-method (propagate n::FSM-*-exit state str index)
   (with-access::FSM-*-exit n (*-entry greedy? exit forbidden?)
      (let ((old-forbidden? forbidden?))
	 (set! forbidden? #t) ;; we don't allow empty iterations.
	 (let ((choices (if greedy?
			    (list *-entry exit)
			    (list exit *-entry))))
	    (propagate-in-order choices state str index)
	    (set! forbidden? old-forbidden?)))))

(define-method (propagate n::FSM-? state str index)
   (with-access::FSM-? n (body exit greedy?)
      (let ((choices (if greedy?
			 (list body exit)
			 (list exit body))))
	 (propagate-in-order choices state str index))))

(define-method (propagate n::FSM-repeat-entry state str index)
   ;; all information is stored in repeat-exit
   (with-access::FSM-repeat-entry n (repeat-exit)
      (with-access::FSM-repeat-exit repeat-exit (repeat-body)
	 (with-access::FSM-state state (loops)
	    (set! loops (cons (instantiate::FSM-loop-info
				(iterations 0)
				(index-time index)
				(loop-exit repeat-exit))
			      loops))
	    ;; only now can we use target-off-limit?
	    (unless (target-off-limit? repeat-body state)
	       (propagate repeat-body state str index))))))

(define-method (propagate n::FSM-repeat-exit state str index)
   (with-access::FSM-repeat-exit n (repeat-body exit min max greedy?)

      (define (repeat state new-count)
	 (with-access::FSM-state state (loops)
	    ;; replace first loop-info
	    (set! loops (cons (instantiate::FSM-loop-info
				 (iterations new-count)
				 (index-time index)
				 (loop-exit n))
			      (cdr loops)))
	    (unless (target-off-limit? repeat-body state)
	       (propagate repeat-body state str index))))

      (define (leave state)
	 (with-access::FSM-state state (loops)
	    ;; we are leaving. -> remove loop-info
	    (set! loops (cdr loops))
	    (unless (target-off-limit? exit state)
	       (propagate exit state str index))))

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
		   'do-nothing)
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
			     
(define-method (propagate n::FSM-backref state str index)
   (with-access::FSM-backref n (backref-nb exit case-sensitive?)
      (with-access::FSM-state state (backref-clusters)
	 (let* ((tmp (*fx backref-nb 2))
		(start (vector-ref backref-clusters tmp))
		(stop (vector-ref backref-clusters (+fx tmp 1)))
		(prefix? (if case-sensitive?
			     string-prefix?
			     string-prefix-ci?)))
	    ;; as the start is only updated, when there is a stop
	    ;; (see FSM-backref-cluster-exit) then there must be a stop
	    ;; if there's a start.
	    (cond
	       ((or (not start)
		    (=fx start stop)) ;; empty string
		(unless (target-off-limit? exit state)
		   (propagate exit state str index)))
	       ((>=fx index (string-length str))
		;; can't match, but string-prefix? would crash
		'do-nothing)
	       ((prefix? str str start stop index)
		;; node has now to wait stop-start before it can continue.
		(widen!::FSM-sleeping-state state
		   (cycles-to-sleep (-fx stop start)))
		;; for the first iteration we can say, that we occupy this
		;; node. after that no.
		(occupy-node! n state))
	       (else
		;; no match. nothing to do
		'do-nothing))))))
