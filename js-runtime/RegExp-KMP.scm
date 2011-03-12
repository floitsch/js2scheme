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

(directives
   (import jsre-RegExp-classes
	   jsre-base-string)
   (export
    (final-class FSM-KMP::FSM-consuming
       fail::FSM-consuming))
   (static
    (KMP::bint greedy?::bool min::bint loop-re els::pair-nil
	       recurse::procedure entry::FSM-node exit::FSM-node
	       clusters-nb::bint get-id::procedure))
   (static
    (wide-class FSM-KMP-re::FSM-KMP
       re)))

;; Knuth-Morris-Pratt
(define (KMP greedy? min loop-re els recurse entry exit clusters-nb get-id)
   ;; the els are not necessarily chars (or 'one-char-consuming?')
   ;; for instance els could be '^' or even '.*'
   ;; take as many one-chars (like 'a', 'every' or '.') as possible and recurse
   ;; on the rest. The kept nodes are then used for the 'do-KMP'.
   (receive (occ-els rest)
      (one-char-consuming/rest els)
      (let* ((c-nb (recurse-on-rest rest recurse entry exit clusters-nb))
	     (new-exit (FSM-node-next entry)))
	 (cond
	    ((null? occ-els)
	     (recurse `(:quantified ,greedy? ,min #f ,loop-re)
		      entry new-exit c-nb))
	    (else
	     (recurse loop-re entry new-exit c-nb)
	     (let ((loop-node (FSM-node-next entry)))
		(do-KMP occ-els loop-node
			loop-node loop-re #t
			greedy?
			recurse
			(if (=fx min 0) entry loop-node)
			new-exit c-nb get-id)))))))

;; split els into one-char-consuming and the rest.
;; ex: 'ab[cd]..*ab' will be split into:
;;        ab[cd]. and .*ab
;; either part might be empty.
(define (one-char-consuming/rest els)
   (let loop ((els els)
	      (rev-occ '()))
      (cond
	 ((null? els)
	  (values (reverse! rev-occ) els))
	 ((one-char-consuming? (car els))
	  (loop (cdr els) (cons (car els) rev-occ)))
	 (else
	  (values (reverse! rev-occ) els)))))

;; sets entry.next to new exit and returns new cluster-nb
;; basically executes (recurse `(seq: ,@rest) ...)
(define (recurse-on-rest rest recurse entry exit clusters-nb)
   (if (null? rest)
       (with-access::FSM-node entry (next)
	  (set! next exit)
	  clusters-nb)
       (recurse `(seq: ,@rest) entry exit clusters-nb)))

;; is n/n-re included in node/node-re
;; ex: a is included in [aA] or '.'
;; returns 'default' if it cannot be established
;;
;; later we might want to use the 're's too...
(define (included-in-node? n n-re node node-re default)
   (cond
      ((FSM-everything? node) #t)
      ((and (FSM-char? n) (FSM-char? node))
       (js-char=? (FSM-char-c n) (FSM-char-c node)))
      ((and (FSM-class? n) (FSM-char? node))
       #f)
      ((and (FSM-char? n) (FSM-class? node))
       (RegExp-class-match (FSM-class-class node) (FSM-char-c n)))
      ((and (FSM-class? n) (FSM-class? node))
       (RegExp-class-subset? (FSM-class-class n)
			     (FSM-class-class node)))
      (else default)))

;; do n1 and n2 overlap?
;; ex: [ab] overlaps with [bc]
;; returns 'default' if it cannot be established
;;
;; later we might want to use the 're's too...
(define (nodes-overlap? n1 n1-re n2 n2-re default)
   (cond
      ((or (FSM-everything? n1)
	   (FSM-everything? n2))
       #t)
      ((and (FSM-char? n1) (FSM-char? n2))
       (js-char=? (FSM-char-c n1) (FSM-char-c n2)))
      ((and (FSM-class? n1) (FSM-char? n2))
       (RegExp-class-match (FSM-class-class n1) (FSM-char-c n2)))
      ((and (FSM-char? n1) (FSM-class? n2))
       (RegExp-class-match (FSM-class-class n2) (FSM-char-c n1)))
      ((and (FSM-class? n1) (FSM-class? n2))
       (RegExp-class-overlap? (FSM-class-class n1)
			      (FSM-class-class n2)))
      (else default)))
   
(define (included-in-loop-node? node node-re loop-node loop-re)
   (included-in-node? node node-re loop-node loop-re #f))

;; if the border points to a node that is 'smaller' than our node than we do
;; not even need to try. same is true, if the node is equal.
;; ex: 'aab' the border of the second 'a' points to the first 'a'. -> no need
;; to try.
;; ex2: 'a[aA]b'. The border of '[aA]' points again to 'a'. -> no need to try.
(define (optimized-border::FSM-node node::FSM-node node-re border::FSM-node)
   (cond
      ((FSM-KMP-re? border)
       (with-access::FSM-KMP-re border (next re fail)
	  (if (included-in-node? next re node node-re #f)
	      (optimized-border node node-re fail)
	      border)))
      (else ;; should always be the loop-node.
       ;; but even if not, can't be wrong.
       border)))

;; border does not exist if current-node is bigger than border-target.
;; ex: a[aA] there exists no next-border for [aA].
(define (next-border-exists? border::FSM-consuming node::FSM-node node-re)
   (let loop ((border border))
      (if (not (FSM-KMP-re? border))
	  #t
	  (with-access::FSM-KMP-re border (next re fail)
	     (cond
		((included-in-node? node node-re next re #f)
		 ;; border can be extended.
		 ;; the new border is the target of 'next'.
		 #t)
		((nodes-overlap? node node-re next re #t)
		 ;; worst case...
		 ;; ex: a[aA]
		 #f)
		(else
		 (loop fail)))))))

;; we cannot merge advance-border and next-border-exists? as
;; the some nodes that are needed for advance-border are only created when
;; 'next-border-exists?'. That is, when we have to know if a border exists it
;; is not yet possible to actually get the advanced-border.
(define (advance-border::FSM-consuming border::FSM-consuming node::FSM-node node-re)
   (let loop ((border border))
      (if (not (FSM-KMP-re? border))
	  (with-access::FSM-node border (next)
	     next)
	  (with-access::FSM-KMP-re border (next re fail)
	     (cond
		((included-in-node? node node-re next re #f)
		 ;; border can be extended.
		 ;; the new border is the target of 'next'.
		 (FSM-node-next next))
		((nodes-overlap? node node-re next re #t)
		 ;; worst case...
		 ;; ex: a[aA]
		 (error "advance-border"
			"advance-border must work. programming-error"
			#f))
		(else
		 (loop fail)))))))

	  
(define (shrink-KMPs! n)
   (when (FSM-KMP-re? n)
      (shrink! n)
      (with-access::FSM-node n (next)
	 (with-access::FSM-node next (next)
	    (shrink-KMPs! next)))))

;; if need-to-set-loop-node? loop-node has not yet been pointed to the
;; correct target. It must point to the first entry of the KMP-nodes.
;; if however no KMP-node has been created it must point to the
;; disjunction (which is still the first entry).
;; as usual the created nodes must fill entry->exit.
;; the c-nb can not be changed (as all els are one-char-nodes.
;; border is the 'fail'-fsm. If a KMP-node fails with its
;; 'next' entry it should try the 'border' entry.
;; Contrary to KMP we optimize this a little bit: no need to try 'a'
;; again if we just failed matching 'a'. (the original KMP would stupidly
;; try again...)
(define (do-KMP els border loop-node loop-re need-to-set-loop-node?
		greedy? recurse entry exit c-nb get-id)
   (cond
      ((null? els)
       ;; we matched the string.
       ;; split!. one goes back to last border. the other one continues
       ;; the re.
       (let ((dis-node (instantiate::FSM-disjunction
			  (id (get-id))
			  (next *tmp-node*))))
	  (with-access::FSM-disjunction dis-node (next alternatives)
	     (cond
		(greedy?
		 (set! next border)
		 (set! alternatives (list exit)))
		(else
		 (set! next exit)
		 (set! alternatives (list border))))
	     (with-access::FSM-node entry (next)
		(set! next dis-node))
	     (when need-to-set-loop-node?
		(with-access::FSM-node loop-node (next)
		   (set! next dis-node)))))
       (shrink-KMPs! (FSM-node-next loop-node))
       c-nb)
      (else
       (recurse (car els) entry exit c-nb)
       (let ((current-node (FSM-node-next entry)))
	  (cond
	     ((or (not (next-border-exists? border current-node (car els)))
		  (not (included-in-loop-node? current-node (car els)
					      loop-node loop-re)))
	      ;; abort KMP before this node...
	      ;; ex (not border):
	      ;;   a[aA]b the border of b would be [aA]. If accepting this
	      ;; border the fsm could match 'aAAb' (which is not correct).
	      ;; -> next-border of [aA] is #f. We create the disjunction before
	      ;; the node [aA].
	      ;;
	      ;; ex (not included)
	      ;;    '[abc]*bcda'. we have to abort at 'd', as it is not
	      ;;     included in the loop-node.
	      (recurse-on-rest (cdr els) recurse current-node exit c-nb)
	      ;; recurse-on-rest can not change c-nb -> no need to get result.
	      (do-KMP '() border loop-node loop-re need-to-set-loop-node?
		      greedy? recurse entry current-node c-nb get-id))
	     (else
	      (let ((this-border (optimized-border current-node (car els)
						   border)))
		 (let ((kmp-node (instantiate::FSM-KMP
				    (id (get-id))
				    (next current-node)
				    (fail this-border))))
		    (widen!::FSM-KMP-re kmp-node
		       (re (car els)))
		    (when need-to-set-loop-node?
		       (with-access::FSM-node loop-node (next)
			  (set! next kmp-node)))
		    (with-access::FSM-node entry (next)
		       (set! next kmp-node))
		    (do-KMP (cdr els)
			    (advance-border border current-node (car els))
			    loop-node loop-re #f
			    greedy? recurse
			    current-node exit c-nb get-id)))))))))
