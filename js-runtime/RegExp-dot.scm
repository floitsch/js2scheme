(module jsre-RegExp-dot
   (import jsre-RegExp-classes
	   jsre-RegExp-fsm
	   jsre-RegExp-state)
   (export (regexp->dot fsm)
	   (running->dot fsm states frozen-states str index)))

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

(define (running->dot fsm states frozen-states str index)
   (dot-header)
   (print (gensym 'str) "[label=\"" (substring str 0 index) "\",shape=record];")
   (let ((ht-ids (dot-out-fsm fsm)))
      (dot-out-states states ht-ids #f str index)
      (for-each (lambda (frozen)
		   (dot-out-states (car frozen) ht-ids (cdr frozen) str index))
		frozen-states))
   (dot-footer))

(define (dot-out-states states ht-ids frozen-index str index)
   (for-each (lambda (state prio)
		(dot-state-out state ht-ids prio frozen-index str index))
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

(define (dot-state-out state::FSM-state ht-ids prio frozen-index str index)
   (with-access::FSM-state state (node clusters backref-clusters loops
				       start-index)
      (let ((s-id (gensym 'state)))
	 (print s-id
		"[shape=record,"
		(if frozen-index
		    "style=dashed,"
		    "")
		"label=\""
		(if (>= start-index 0)
		    (substring str start-index index)
		    "")
		"|" prio
		(if (FSM-sleeping-state? state)
		    (with-access::FSM-sleeping-state state (cycles-to-sleep)
		       (format " [~a]" cycles-to-sleep))
		    "")
		(if frozen-index
		    (format " *<~a>*" frozen-index)
		    "")
		"|" clusters
		"|" backref-clusters
		"|" (map FSM-loop-info-iterations loops) "\"];")
	 (print s-id " -> " (get-id node ht-ids) ";"))))

(define-generic (dot-out n::FSM-node ht-id ht-done)
   (unless (hashtable-get ht-done n)
      (hashtable-put! ht-done n #t)
      (print (get-id n ht-id) "[label=\"ERROR\"];")
      (with-access::FSM-start n (next)
	 (dot-out next ht-id ht-done)
	 (print (get-id n ht-id) " -> " (get-id next ht-id) ";"))))
;   (error "dot"
;	  "forgot a node-type"
;	  n))

(define-method (dot-out n::FSM-start ht-id ht-done)
   (unless (hashtable-get ht-done n)
      (hashtable-put! ht-done n #t)
      (print (get-id n ht-id) "[label=\"start\"];")
      (with-access::FSM-start n (next)
	 (dot-out next ht-id ht-done)
	 (print (get-id n ht-id) " -> " (get-id next ht-id) ";"))))

(define-method (dot-out n::FSM-final ht-id ht-done)
   (unless (hashtable-get ht-done n)
      (hashtable-put! ht-done n #t)
      (print (get-id n ht-id) "[label=\"\",peripheries=2];")))

(define-method (dot-out n::FSM-disjunction ht-id ht-done)
   (unless (hashtable-get ht-done n)
      (hashtable-put! ht-done n #t)
      (print (get-id n ht-id) "[label=\"\"]; // disjunction")
      (with-access::FSM-disjunction n (next alternatives)
	 (for-each (lambda (alt i)
		      (dot-out alt ht-id ht-done)
		      (print (get-id n ht-id) " -> " (get-id alt ht-id)
			     "[style=dashed, label=\"" i "\"];"))
		   (cons next alternatives)
		   (iota (+fx 1 (length alternatives)))))))

(define-method (dot-out n::FSM-? ht-id ht-done)
   (unless (hashtable-get ht-done n)
      (hashtable-put! ht-done n #t)
      (print (get-id n ht-id) "[label=\"?\"]; // ?")
      (with-access::FSM-? n (next exit greedy?)
	 (dot-out next ht-id ht-done)
	 (dot-out exit ht-id ht-done)
	 (print (get-id n ht-id) " -> " (get-id next ht-id) "[style=dashed, label=\"" (if greedy? 0 1) "\"];")
	 (print (get-id n ht-id) " -> " (get-id exit ht-id) "[style=dashed, label=\"" (if greedy? 1 0) "\"];"))))

(define-method (dot-out n::FSM-*-exit ht-id ht-done)
   (unless (hashtable-get ht-done n)
      (hashtable-put! ht-done n #t)
      (print (get-id n ht-id) "[label=\"*-exit\"]; // *-exit")
      (with-access::FSM-*-exit n (loop-body next greedy?)
	 (dot-out loop-body ht-id ht-done)
	 (dot-out next ht-id ht-done)
	 (print (get-id n ht-id) " -> " (get-id loop-body ht-id) "[style=dashed, label=\"" (if greedy? 0 1) "\"];")
	 (print (get-id n ht-id) " -> " (get-id next ht-id) "[style=dashed, label=\"" (if greedy? 1 0) "\"];"))))

(define-method (dot-out n::FSM-repeat-entry ht-id ht-done)
   (unless (hashtable-get ht-done n)
      (hashtable-put! ht-done n #t)
      (with-access::FSM-repeat-entry n (repeat-exit next)
	 (with-access::FSM-repeat-exit repeat-exit (min max)
	    (print (get-id n ht-id) "[label=\"repeat-entry\"]; "
		   "// {" min ", " (or max "") "}")
	    (dot-out next ht-id ht-done)
	    (print (get-id n ht-id) " -> " (get-id next ht-id) ";")))))

(define-method (dot-out n::FSM-repeat-exit ht-id ht-done)
   (unless (hashtable-get ht-done n)
      (hashtable-put! ht-done n #t)
      (with-access::FSM-repeat-exit n (loop-body next min max greedy?)
	 (let ((rep-desc (format "{~a, ~a}" min (or max ""))))
	    (print (get-id n ht-id) "[label=\"repeat-exit\"]; // repeat-exit " rep-desc)
	    (dot-out loop-body ht-id ht-done)
	    (dot-out next ht-id ht-done)
	    (print (get-id n ht-id) " -> " (get-id loop-body ht-id)
		   "[style=dashed, label=\"" (if greedy? 0 1) " "
		   rep-desc "\"];")
	    (print (get-id n ht-id) " -> " (get-id next ht-id)
		   "[style=dashed, label=\"" (if greedy? 1 0) " "
		   rep-desc "\"];")))))

(define-method (dot-out n::FSM-non-empty ht-id ht-done)
   (unless (hashtable-get ht-done n)
      (hashtable-put! ht-done n #t)
      (print (get-id n ht-id) "[label=\"non-empty\"]; // non-empty")
      (with-access::FSM-non-empty n (next other)
	 (dot-out next ht-id ht-done)
	 (dot-out other ht-id ht-done)
	 (print (get-id n ht-id) " -> " (get-id next ht-id) ";")
	 (print (get-id n ht-id) " -> " (get-id other ht-id)
		"[style=dotted, label=\"non-empty\"];"))))

(define-method (dot-out n::FSM-backref ht-id ht-done)
   (unless (hashtable-get ht-done n)
      (hashtable-put! ht-done n #t)
      (with-access::FSM-backref n (next backref-nb)
	 (print (get-id n ht-id) "[label=\"br "
		backref-nb "\"]; // backref")
	 (dot-out next ht-id ht-done)
	 (print (get-id n ht-id) " -> " (get-id next ht-id) "[style=dashed];"))))

(define-method (dot-out n::FSM-char ht-id ht-done)
   (unless (hashtable-get ht-done n)
      (hashtable-put! ht-done n #t)
      (with-access::FSM-char n (next c)
	 (print (get-id n ht-id) "[label=\"'"
		c "'\"]; // char")
	 (dot-out next ht-id ht-done)
	 (print (get-id n ht-id) " -> "
		(get-id next ht-id) ";"))))

(define-method (dot-out n::FSM-every-char ht-id ht-done)
   (unless (hashtable-get ht-done n)
      (hashtable-put! ht-done n #t)
      (with-access::FSM-char n (next c)
	 (print (get-id n ht-id) "[label=\"every\"]; // char")
	 (dot-out next ht-id ht-done)
	 (print (get-id n ht-id) " -> "
		(get-id next ht-id) ";"))))

(define-method (dot-out n::FSM-class ht-id ht-done)
   (unless (hashtable-get ht-done n)
      (hashtable-put! ht-done n #t)
      (with-access::FSM-class n (next)
	 (print (get-id n ht-id) "[label=\"[...]\"]; // class")
	 (dot-out next ht-id ht-done)
	 (print (get-id n ht-id) " -> "
		(get-id next ht-id) ";"))))

(define-method (dot-out n::FSM-assert ht-id ht-done)
   (unless (hashtable-get ht-done n)
      (hashtable-put! ht-done n #t)
      (with-access::FSM-assert n (next)
	 (print (get-id n ht-id) "[label=\"assert\"]; // assert")
	 (dot-out next ht-id ht-done)
	 (print (get-id n ht-id) " -> "
		(get-id next ht-id) ";"))))

(define-method (dot-out n::FSM-condition ht-id ht-done)
   (unless (hashtable-get ht-done n)
      (hashtable-put! ht-done n #t)
      (with-access::FSM-condition n (next)
	 (print (get-id n ht-id) "[label=\"cond\"]; // condition")
	 (dot-out next ht-id ht-done)
	 (print (get-id n ht-id) " -> "
		(get-id next ht-id) ";"))))

(define-method (dot-out n::FSM-cluster-entry ht-id ht-done)
   (unless (hashtable-get ht-done n)
      (hashtable-put! ht-done n #t)
      (with-access::FSM-cluster-entry n (next cluster-index)
	 (print (get-id n ht-id) "[label=\"+" cluster-index "\"]; // cluster")
	 (dot-out next ht-id ht-done)
	 (print (get-id n ht-id) " -> "
		(get-id next ht-id) ";"))))

(define-method (dot-out n::FSM-cluster-exit ht-id ht-done)
   (unless (hashtable-get ht-done n)
      (hashtable-put! ht-done n #t)
      (with-access::FSM-cluster-exit n (next cluster-index backref-exit-index)
	 (if backref-exit-index
	     (print (get-id n ht-id) "[label=\"-" cluster-index "/"
		    backref-exit-index "\"]; // cluster/backref")
	     (print (get-id n ht-id) "[label=\"-" cluster-index "\"]; // cluster"))
	 (dot-out next ht-id ht-done)
	 (print (get-id n ht-id) " -> "
		(get-id next ht-id) ";"))))

(define-method (dot-out n::FSM-cluster-assert ht-id ht-done)
   (unless (hashtable-get ht-done n)
      (hashtable-put! ht-done n #t)
      (with-access::FSM-cluster-assert n (next entry negative?)
	 (print (get-id n ht-id)
		"[label=\"cluster-assert\"]; // cluster-assert")
	 (dot-out entry ht-id ht-done)
	 (dot-out next ht-id ht-done)
	 (print (get-id n ht-id) " -> "
		(get-id next ht-id) ";")
	 (print (get-id n ht-id)
		" -> "
		(get-id entry ht-id)
		"[style=dotted, label=\""
		(if negative?
		    "not"
		    "")
		"\"];"))))
