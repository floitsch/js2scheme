(module jsre-RegExp-dot
   (import jsre-RegExp-classes
	   jsre-RegExp-fsm
	   jsre-RegExp-state)
   (export (regexp->dot fsm)
	   (running->dot fsm states frozen-states str)))

(define (dot-header)
   (print "digraph g {")
;   (print "node [shape = record];")
   (print "rankdir=LR;"))

(define (dot-footer)
   (print "}"))
   
(define (regexp->dot fsm)
   (dot-header)
   (dot-out-fsm fsm)
   (dot-footer))

(define (running->dot fsm states frozen-states str)
   (dot-header)
   (print (gensym 'str) "[label=\"" str "\",shape=record];")
   (let ((ht-ids (dot-out-fsm fsm)))
      (dot-out-states states ht-ids #f)
      (for-each (lambda (frozen)
		   (dot-out-states (car frozen) ht-ids (cdr frozen)))
		frozen-states))
   (dot-footer))

(define (dot-out-states states ht-ids frozen-index)
   (for-each (lambda (state prio)
		(dot-state-out state ht-ids prio frozen-index))
	     states
	     (iota (length states))))

(define (dot-out-fsm fsm)
   (let ((entry (gensym 'entry))
	 (ht-ids (make-hashtable)))
      (print entry "[peripheries=0,label=\"\"];")
      (print entry " -> " (get-id (FSM-entry fsm) ht-ids) ";")
      (dot-out (FSM-entry fsm) ht-ids (make-hashtable))
      ht-ids))

(define (get-id obj ht)
   (or (hashtable-get ht obj)
       (let ((id (gensym 'obj)))
	  (hashtable-put! ht obj id)
	  id)))

(define (dot-state-out state::FSM-state ht-ids prio frozen-index)
   (with-access::FSM-state state (node clusters backref-clusters)
      (let ((s-id (gensym 'state)))
	 (print s-id
		"[shape=record,"
		(if frozen-index
		    "style=dashed,"
		    "")
		"label=\"" prio
		(if (FSM-sleeping-state? state)
		    (with-access::FSM-sleeping-state state (cycles-to-sleep)
		       (format " [~a]" cycles-to-sleep))
		    "")
		(if frozen-index
		    (format " *<~a>*" frozen-index)
		    "")
		"|" clusters
		"|" backref-clusters "\"];")
	 (print s-id " -> " (get-id node ht-ids) ";"))))

(define-generic (dot-out n::FSM-node ht-id ht-done)
   (error "dot"
	  "forgot a node-type"
	  (format "~s" n)))

(define-method (dot-out n::FSM-final ht-id ht-done)
   (unless (hashtable-get ht-done n)
      (hashtable-put! ht-done n #t)
      (print (get-id n ht-id) "[label=\"\",peripheries=2];")))

(define-method (dot-out n::FSM-simple ht-id ht-done)
   (unless (hashtable-get ht-done n)
      (hashtable-put! ht-done n #t)
      (print (get-id n ht-id) "[label=\"\"]; // simple")
      (with-access::FSM-simple n (transit O-cost-transit)
	 (let ((t (or transit O-cost-transit)))
	    (dot-out (FSM-transit-target t) ht-id ht-done)
	    (dot-transit-out t n ht-id)))))

(define-method (dot-out n::FSM-disjunction ht-id ht-done)
   (unless (hashtable-get ht-done n)
      (hashtable-put! ht-done n #t)
      (print (get-id n ht-id) "[label=\"\"]; // disjunction")
      (with-access::FSM-disjunction n (alternatives)
	 (for-each (lambda (alt i)
		      (dot-out alt ht-id ht-done)
		      (print (get-id n ht-id) " -> " (get-id alt ht-id)
			     "[style=dashed, label=\"" i "\"];"))
		   alternatives (iota (length alternatives))))))

(define-method (dot-out n::FSM-*-exit ht-id ht-done)
   (unless (hashtable-get ht-done n)
      (hashtable-put! ht-done n #t)
      (print (get-id n ht-id) "[label=\"\"]; // *-entry")
      (with-access::FSM-*-exit n (loop-body exit greedy?)
	 (dot-out loop-body ht-id ht-done)
	 (dot-out exit ht-id ht-done)
	 (print (get-id n ht-id) " -> " (get-id loop-body ht-id) "[style=dashed, label=\"" (if greedy? 0 1) "\"];")
	 (print (get-id n ht-id) " -> " (get-id exit ht-id) "[style=dashed, label=\"" (if greedy? 1 0) "\"];"))))

(define-method (dot-out n::FSM-repeat-entry ht-id ht-done)
   (unless (hashtable-get ht-done n)
      (hashtable-put! ht-done n #t)
      (with-access::FSM-repeat-entry n (repeat-exit)
	 (with-access::FSM-repeat-exit repeat-exit (min max loop-body)
	    (print (get-id n ht-id) "[label=\"\"]; "
		   "// {" min ", " (or max "") "}")
	    (dot-out loop-body ht-id ht-done)))))

(define-method (dot-out n::FSM-repeat-exit ht-id ht-done)
   (unless (hashtable-get ht-done n)
      (hashtable-put! ht-done n #t)
      (with-access::FSM-repeat-exit n (loop-body exit min max greedy?)
	 (let ((rep-desc (format "{~a, ~a}" min (or max ""))))
	    (print (get-id n ht-id) "[label=\"\"]; // repeat-exit " rep-desc)
	    (dot-out loop-body ht-id ht-done)
	    (dot-out exit ht-id ht-done)
	    (print (get-id n ht-id) " -> " (get-id loop-body ht-id)
		   "[style=dashed, label=\"" (if greedy? 0 1) " "
		   rep-desc "\"];")
	    (print (get-id n ht-id) " -> " (get-id exit ht-id)
		   "[style=dashed, label=\"" (if greedy? 1 0) " "
		   rep-desc "\"];")))))

(define-method (dot-out n::FSM-non-empty ht-id ht-done)
   (unless (hashtable-get ht-done n)
      (hashtable-put! ht-done n #t)
      (print (get-id n ht-id) "[label=\"\"]; // non-empty")
      (with-access::FSM-simple n (transit O-cost-transit)
	 (let ((t (or transit O-cost-transit)))
	    (dot-out (FSM-transit-target t) ht-id ht-done)
	    (dot-transit-out t n ht-id)))
      (with-access::FSM-non-empty n (exit)
	 (print (get-id n ht-id) " -> " (get-id exit ht-id)
		"[style=dotted, label=\"non-empty\"];"))))

(define-method (dot-out n::FSM-? ht-id ht-done)
   (unless (hashtable-get ht-done n)
      (hashtable-put! ht-done n #t)
      (print (get-id n ht-id) "[label=\"\"]; // ?")
      (with-access::FSM-? n (body exit greedy?)
	 (dot-out body ht-id ht-done)
	 (dot-out exit ht-id ht-done)
	 (print (get-id n ht-id) " -> " (get-id body ht-id) "[style=dashed, label=\"" (if greedy? 0 1) "\"];")
	 (print (get-id n ht-id) " -> " (get-id exit ht-id) "[style=dashed, label=\"" (if greedy? 1 0) "\"];"))))

(define-method (dot-out n::FSM-backref ht-id ht-done)
   (unless (hashtable-get ht-done n)
      (hashtable-put! ht-done n #t)
      (with-access::FSM-backref n (exit backref-nb)
	 (print (get-id n ht-id) "[label=\"br "
		backref-nb "\"]; // backref")
	 (dot-out exit ht-id ht-done)
	 (print (get-id n ht-id) " -> " (get-id exit ht-id) "[style=dashed];"))))

(define-generic (dot-transit-out t::FSM-transit start ht-id)
   (with-access::FSM-transit t (target)
      (print (get-id start ht-id) " -> " (get-id target ht-id) "[style=dashed];")))

(define-method (dot-transit-out t::FSM-char-transit start ht-id)
   (with-access::FSM-char-transit t (target c)
      (print (get-id start ht-id) " -> "
	     (get-id target ht-id)
	     "[label=\"" c "\"];")))

(define-method (dot-transit-out t::FSM-class-transit start ht-id)
   (with-access::FSM-class-transit t (target dot-info)
      (print (get-id start ht-id) " -> "
	     (get-id target ht-id)
	     "[label=\"" dot-info "\"];")))

(define-method (dot-transit-out t::FSM-assert-transit start ht-id)
   (with-access::FSM-class-transit t (target dot-info)
      (print (get-id start ht-id) " -> "
	     (get-id target ht-id)
	     "[style=dashed, label=\"" dot-info "\"];")))

(define-method (dot-transit-out t::FSM-condition-transit start ht-id)
   (with-access::FSM-class-transit t (target dot-info)
      (print (get-id start ht-id) " -> "
	     (get-id target ht-id)
	     "[label=\"" dot-info "\"];")))

(define-method (dot-transit-out t::FSM-cluster start ht-id)
   (with-access::FSM-cluster t (target cluster-index)
      (print (get-id start ht-id) " -> "
	     (get-id target ht-id)
	     "[style=dashed, label=\"(+" cluster-index ")\"];")))

(define-method (dot-transit-out t::FSM-backref-cluster-exit start ht-id)
   (with-access::FSM-backref-cluster-exit t (target cluster-index backref-index)
      (print (get-id start ht-id) " -> "
	     (get-id target ht-id)
	     "[style=dashed, label=\"(-" cluster-index "/" backref-index ")\"];")))

(define-method (dot-transit-out t::FSM-cluster-assert start ht-id)
   (with-access::FSM-cluster-assert t (target entry exit negative?)
      (dot-out entry ht-id (make-hashtable))
      (print (get-id start ht-id) " -> "
	     (get-id entry ht-id)
	     "[style=dashed"];")
      (print (get-id exit ht-id) " -> "
	     (get-id target ht-id)
	     "[style=dashed, label=\""
	     (if negative?
		 "not"
		 "")
	     "\"];")
      (print (get-id start ht-id)
	     " -> "
	     (get-id target ht-id)
	     "[style=dotted, label=\"assert\"];")))
