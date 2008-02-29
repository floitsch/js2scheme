(module jsre-RegExp-match
   (import jsre-RegExp-classes
	   jsre-RegExp-fsm)
   (export (regexp-run fsm::FSM str::bstring)))

;; REFACTORINGS TODO:
;; - merge FSM-*-entry/FSM-?
;; - merge FSM-assert-transit and FSM-condition-transit ? (one consumes a char)
;; - node-visit, take-target (better name!), .. need start-pos
;; - others...

;; for a description of the process look at RegExp-fsm

(define (regexp-run fsm str)
   (with-access::FSM fsm (entry exit nb-clusters)
      (let* ((state (instantiate::FSM-state
		       (clusters (make-vector (* nb-clusters 2) #f))
		       (node entry)))
	     (rev-init-states (propagate entry state str -1 0 '()))
	     (init-states (reverse! rev-init-states))
	     (match (run init-states str 0)))
	 (and match
	      (cons (substring str 0 (FSM-state-final-index match))
		    (FSM-state-clusters match))))))

(define (run states str time) ;; index == time
   (cond
      ((null? states)
       #f) ;; no match
      ((FSM-final? (FSM-state-node (car states)))
       ;; first state in priority-list is final. Can't be any better...
       (car states))
      ((>=fx time (string-length str))
       ;; end of string. If there's a state pointing to the final-node we have
       ;; a winner.
       (let ((winner (any (lambda (state)
			     (and (FSM-final? (FSM-state-node state))
				  state))
			  states)))
	  winner))
      (else ;; advance states
       (let loop ((states states)
		  (rev-next-round '()))
	  (if (null? states)
	      (run (reverse! rev-next-round) str (+fx time 1))
	      (with-access::FSM-state (car states) (node)
		 (let ((rnr (node-visit node (car states) str time
					rev-next-round)))
		    (loop (cdr states) rnr))))))))

(define-generic (node-visit n::FSM-node state::FSM-state
			    str::bstring time::bint ;; time == index
			    rev-next-states::pair-nil)
   (error "FSM-match"
	  "forgot node-type"
	  n))

(define-method (node-visit n::FSM-final state str time rev-next-states)
   ;; unless there is already somebody else on the node simply update the time
   ;; and put ourselves back in the queue.
   (with-access::FSM-node n (last-visited)
      (if (<fx last-visited time)
	  (begin
	     (set! last-visited time)
	     (cons state rev-next-states))
	  rev-next-states)))

(define-method (node-visit n::FSM-simple state str time rev-next-states)
   ;; don't look for O-cost-transits. They are not relevant here.
   (with-access::FSM-simple n (transit)
      (if transit
	  (take-transit transit state str time time rev-next-states)
	  rev-next-states)))

(define-method (node-visit n::FSM-non-empty state str time rev-next-states)
   ;; don't look for O-cost-transits. They are not relevant here.
   (with-access::FSM-non-empty n (last-visited exit transit O-cost-transit)
      (let ((t0 transit) (t1 O-cost-transit))
	 [assert (t0 t1) (not (and t0 t1))])
      (if transit ;; should never happen
	  (take-transit transit state str time time rev-next-states)
	  rev-next-states)))

;; FSM-disjunction, FSM-*-entry, FSM-*-exit and FSM-? do not
;; have any node-visit. (propagation should never leave a state on one of these
;; nodes.

(define-generic (take-transit t::FSM-transit state str time index rev-next-states)
   ;; O-cost-transit.
   (with-access::FSM-transit t (target)
      (with-access::FSM-node target (last-visited)
	 (if (<fx last-visited time)
	     (propagate target state str time index rev-next-states)
	     rev-next-states))))

(define-method (take-transit t::FSM-char-transit state str time index
			     rev-next-states)
   ;; TODO case-sensitive
   (with-access::FSM-char-transit t (c target)
      (with-access::FSM-node target (last-visited)
	 (if (and (<fx last-visited time)
		  (char=? c (string-ref str index)))
	     (propagate target state str time (+fx index 1) rev-next-states)
	     rev-next-states))))

(define-method (take-transit t::FSM-class-transit state str time index
			     rev-next-states)
   (with-access::FSM-class-transit t (class target)
      (with-access::FSM-node target (last-visited)
	 (if (and (<fx last-visited time)
		  (RegExp-match-c (string-ref str index) class))
	     (propagate target state str time (+fx index 1) rev-next-states)
	     rev-next-states))))

(define-method (take-transit t::FSM-assert-transit state str time index
			     rev-next-states)
   (with-access::FSM-assert-transit t (condition target)
      (with-access::FSM-node target (last-visited)
	 (if (and (<fx last-visited time)
		  (condition str 0 index)) ;; TODO: what about startpos
	     (propagate target state str time index rev-next-states)
	     rev-next-states))))

(define-method (take-transit t::FSM-condition-transit state str time index
			     rev-next-states)
   (with-access::FSM-condition-transit t (condition target)
      (with-access::FSM-node target (last-visited)
	 (if (and (<fx last-visited time)
		  (condition (string-ref str index)))
	     (propagate target state str time (+fx index 1) rev-next-states)
	     rev-next-states))))

(define-method (take-transit t::FSM-cluster state str time index
			     rev-next-states)
   (with-access::FSM-cluster-entry t (cluster-index target)
      (with-access::FSM-node target (last-visited)
	 (if (<fx last-visited time)
	     (with-access::FSM-state state (clusters)
		;; copy on write
		(set! clusters (copy-vector clusters (vector-length clusters)))
		(vector-set! clusters cluster-index index)
		(propagate target state str time index rev-next-states))
	     rev-next-states))))

; (define-method (take-transit t::FSM-cluster-assert state str time
; 			     rev-next-states)
;    (with-access::FSM-cluster-assert t (entry exit negative? target)
;       (with-access::FSM-node target (last-visited)
; 	 (when (<fx last-visited time)

;; --------
;; if a node is reached through "propagate" then we are sure, that there is no
;; other node visiting here.
(define-generic (propagate n::FSM-node state str time index rev-next-states)
   (error "FSM-match"
	  "forgot node-type"
	  n))

(define-method (propagate n::FSM-final state str time index rev-next-states)
   (with-access::FSM-final n (last-visited)
      (set! last-visited time)
      (with-access::FSM-state state (node final-index)
	 (set! node n)
	 (set! final-index index))
      (cons state rev-next-states)))

(define-method (propagate n::FSM-simple state str time index rev-next-states)
   (with-access::FSM-simple n (last-visited transit O-cost-transit)
      (set! last-visited time)
      ;; there must not be two transits.
      (let ((t0 transit) (t1 O-cost-transit))
	 [assert (t0 t1) (not (and t0 t1))])
      (if O-cost-transit
	  ;; just move to the next node.
	  (take-transit O-cost-transit state str time index rev-next-states)
	  ;; update state and next-state-list.
	  (with-access::FSM-state state (node)
	     (set! node n)
	     (cons state rev-next-states)))))

(define-method (propagate n::FSM-non-empty state str time index
			  rev-next-states)
   (with-access::FSM-non-empty n (last-visited exit transit O-cost-transit)
      (let ((t0 transit) (t1 O-cost-transit))
	 [assert (t0 t1) (not (and t0 t1))])
      (set! last-visited time)
      (if O-cost-transit
	  ;; we do not allow empty matches. -> mark the exit node as visited.
	  ;; propagation will stop before the exit-node.
	  ;; afterwards we restore the old time-value.
	  (with-access::FSM-node exit (last-visited)
	     (let ((old-exit-time last-visited))
		(set! last-visited time)
		(let ((rns (take-transit O-cost-transit state
					 str time index rev-next-states)))
		   (set! last-visited old-exit-time)
		   rns)))
	  (with-access::FSM-state state (node)
	     (set! node n)
	     (cons state rev-next-states)))))

(define (propagate-in-order choices state str time index rev-next-states)
   (if (null? choices)
       rev-next-states
       (with-access::FSM-node (car choices) (last-visited)
	  (if (<fx last-visited time)
	      (let* ((dupl (duplicate::FSM-state state))
		     (rns (propagate (car choices) dupl
				     str time index rev-next-states)))
		 (propagate-in-order (cdr choices) state str time index rns))
	      (propagate-in-order (cdr choices) state str time index
				  rev-next-states)))))


(define-method (propagate n::FSM-disjunction state str time index rev-next-states)
   ;; TODO: not yet completely optimized
   (with-access::FSM-disjunction n (last-visited alternatives)
      (set! last-visited time)
      (propagate-in-order alternatives state str time index rev-next-states)))

(define-method (propagate n::FSM-*-entry state str time index rev-next-states)
   (with-access::FSM-*-entry n (last-visited body *-exit greedy?)
      (set! last-visited time)
      (let ((choices (if greedy?
			 (list body *-exit)
			 (list *-exit body))))
	 (propagate-in-order choices state str time index rev-next-states))))

(define-method (propagate n::FSM-*-exit state str time index rev-next-states)
   (with-access::FSM-*-exit n (last-visited *-entry exit)
      (set! last-visited time)
      (propagate-in-order (list *-entry exit) state str time index
			  rev-next-states)))

(define-method (propagate n::FSM-? state str time index rev-next-states)
   (with-access::FSM-? n (last-visited body exit greedy?)
      (set! last-visited time)
      (let ((choices (if greedy?
			 (list body exit)
			 (list exit body))))
	 (propagate-in-order choices state str time index rev-next-states))))
