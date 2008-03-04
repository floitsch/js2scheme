(module jsre-RegExp-dot
   (import jsre-RegExp-classes
	   jsre-RegExp-fsm)
   (export (regexp->dot fsm)))

(define (regexp->dot fsm)
   (print "digraph g {")
;   (print "node [shape = record];")
   (print "rankdir=LR;")

   (let ((entry (gensym 'entry))
	 (ht-ids (make-hashtable)))
      (print entry "[peripheries=0,label=\"\"];")
      (print entry " -> " (get-id (FSM-entry fsm) ht-ids) ";")
      (dot-out (FSM-entry fsm) ht-ids (make-hashtable)))
   (print "}"))

(define (get-id obj ht)
   (or (hashtable-get ht obj)
       (let ((id (gensym 'obj)))
	  (hashtable-put! ht obj id)
	  id)))

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

(define-method (dot-out n::FSM-*-entry ht-id ht-done)
   (unless (hashtable-get ht-done n)
      (hashtable-put! ht-done n #t)
      (print (get-id n ht-id) "[label=\"\"]; // *-entry")
      (with-access::FSM-*-entry n (body *-exit greedy?)
	 (dot-out body ht-id ht-done)
	 (dot-out *-exit ht-id ht-done)
	 (print (get-id n ht-id) " -> " (get-id body ht-id) "[style=dashed, label=\"" (if greedy? 0 1) "\"];")
	 (print (get-id n ht-id) " -> " (get-id *-exit ht-id) "[style=dashed, label=\"" (if greedy? 1 0) "\"];"))))

(define-method (dot-out n::FSM-*-exit ht-id ht-done)
   (unless (hashtable-get ht-done n)
      (hashtable-put! ht-done n #t)
      (print (get-id n ht-id) "[label=\"\"]; // *-exit")
      (with-access::FSM-*-exit n (*-entry exit)
	 (dot-out exit ht-id ht-done)
	 (print (get-id n ht-id) " -> " (get-id *-entry ht-id) "[style=dashed];")
	 (print (get-id n ht-id) " -> " (get-id exit ht-id) "[style=dashed];"))))

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
