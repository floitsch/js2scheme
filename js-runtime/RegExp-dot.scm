;; Copyright (c) 2007-2011, Florian Loitsch
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are met:
;;    * Redistributions of source code must retain the above copyright
;;      notice, this list of conditions and the following disclaimer.
;;    * Redistributions in binary form must reproduce the above copyright
;;      notice, this list of conditions and the following disclaimer in the
;;      documentation and/or other materials provided with the distribution.
;;    * Neither the name of the <organization> nor the
;;      names of its contributors may be used to endorse or promote products
;;      derived from this software without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;; DISCLAIMED. IN NO EVENT SHALL <COPYRIGHT HOLDER> BE LIABLE FOR ANY
;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
;; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
;; ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(module jsre-RegExp-dot
   (import jsre-base-string
	   jsre-RegExp-classes
	   jsre-RegExp-fsm
	   jsre-RegExp-state)
   (use jsre-conversion
	jsre-base-object)
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
   (print (gensym 'str)
	  "[label=\"" (js-substring str 0 index) "\",shape=record];")
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
				       start-index
				       sharing-clusters?
				       sharing-br-clusters?)
      (let ((s-id (gensym 'state)))
	 (print s-id
		"[shape=record,"
		(if frozen-index
		    "style=dashed,"
		    "")
		"label=\""
		(if (>= start-index 0)
		    (js-substring str start-index index)
		    "")
		"|" prio
		(if (FSM-sleeping-state? state)
		    (with-access::FSM-sleeping-state state (cycles-to-sleep)
		       (format " [~a]" cycles-to-sleep))
		    "")
		(if frozen-index
		    (format " *<~a>*" frozen-index)
		    "")
		"|" clusters " " sharing-clusters?
		"|" backref-clusters " " sharing-br-clusters?
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
      (with-access::FSM-start n (offset next)
	 (print (get-id n ht-id) "[label=\"start-" offset "\"];")
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

(define-method (dot-out n::FSM-KMP ht-id ht-done)
   (unless (hashtable-get ht-done n)
      (hashtable-put! ht-done n #t)
      (print (get-id n ht-id) "[label=\"KMP\"];")
      (with-access::FSM-KMP n (next fail)
	 (dot-out next ht-id ht-done)
	 (print (get-id n ht-id) " -> " (get-id next ht-id) ";")
	 (dot-out fail ht-id ht-done)
	 (print (get-id n ht-id) " -> " (get-id fail ht-id)
		"[style=dashed, label=\"fail\"];"))))

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

(define-method (dot-out n::FSM-everything ht-id ht-done)
   (unless (hashtable-get ht-done n)
      (hashtable-put! ht-done n #t)
      (with-access::FSM-everything n (next)
	 (print (get-id n ht-id) "[label=\"every\"]; // every")
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

(define-method (dot-out n::FSM-cluster ht-id ht-done)
   (unless (hashtable-get ht-done n)
      (hashtable-put! ht-done n #t)
      (with-access::FSM-cluster n (next cluster-index backref-cluster-index)
	 (if backref-cluster-index
	     (print (get-id n ht-id) "[label=\"" cluster-index "/"
		    backref-cluster-index "\"]; // cluster/backref")
	     (print (get-id n ht-id) "[label=\"" cluster-index "\"]; // cluster"))
	 (dot-out next ht-id ht-done)
	 (print (get-id n ht-id) " -> "
		(get-id next ht-id) ";"))))

(define-method (dot-out n::FSM-cluster-clear ht-id ht-done)
   (unless (hashtable-get ht-done n)
      (hashtable-put! ht-done n #t)
      (with-access::FSM-cluster-clear n (next start-index stop-index
					      backref-start-index
					      backref-stop-index)
	 (if backref-start-index
	     (print (get-id n ht-id) "[label=\""
		    start-index "-" stop-index "/"
		    backref-start-index "-" backref-stop-index
		    "\"]; // cluster-clear")
	     (print (get-id n ht-id) "[label=\""
		    start-index "-" stop-index
		    "\"]; // cluster-clear"))
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
