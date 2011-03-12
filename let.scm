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

(module let
   (import walk
	   verbose
	   nodes)
   (export (let-intro! tree::Program)))

;; adds let*-nodes.

(define (let-intro! tree)
   (verbose "let-intro!")
   (reverse-liveness tree)
   (add-nodes! tree))

;; at this moment of the compilation all (non-global) vars have a begin-nesting
;; and a end-nesting stack.
;; this pass reverses this, and assigns to each respective node the
;; variables that start to be live at this node (and the same for the end).
;; If the node is a 'begin' the start/end is assigned to the respective
;; subnodes.
;; Ex:
;; suppose a variable x is live in the 'test' and 'then' parts of an 'if'. Then
;; the 'if' node records the beginning and end of the liveness of x.
;; On the other hand. suppose x is used in a begin:
;; (begin
;;    n1
;;    n2 uses x
;;    n3
;     n4 uses x
;; )
;; then we make an exception, and record x's liveness in n2 and n4 (instead of
;; the surrounding begin).
(define (reverse-liveness tree)
   (verbose " let-liveness")
   (rev-live tree #f #f))

;; 'surrounding-fun' is maybe not the correct name (as it could be the "Program too").
(define-nmethod (Node.rev-live surrounding-fun)
   (default-walk this surrounding-fun))

(define-nmethod (Program.rev-live surrounding-fun)
   (default-walk this this))
(define-nmethod (Fun.rev-live surrounding-fun)
   (default-walk this this))

(define-nmethod (Decl.rev-live surrounding-fun)
   (define (mark-live-begin node var)
      (with-access::Node node (live-begins)
	 (set! live-begins (cons var (or live-begins '())))))
   (define (mark-live-end node var)
      (with-access::Node node (live-ends)
	 (set! live-ends (cons var (or live-ends '())))))

   (define (transitive-with-var var)
      (if (Intercepted-Var? var)
	  (with-access::Intercepted-Var var (intercepted)
	     (transitive-with-var intercepted))
	  var))

   (let ((var (transitive-with-var (Decl-var this))))
      (with-access::Var var (live-begin-stack live-end-stack global?
					      no-let? escapes?)
	 (cond
	    (global? ;; we are printing those directly in Program-out.
	     'do-nothing)
	    (no-let? ;; in particular Decl-Intercept variables.
	     'do-nothing)
	    ((not live-begin-stack) ;; imported, runtime or whatever...
	     'do-nothing)
	    (escapes?
	     (mark-live-begin (Scope-body surrounding-fun) var)
	     (mark-live-end (Scope-body surrounding-fun) var))
	    ((Begin? (cadr live-begin-stack))
	     (mark-live-begin (car live-begin-stack) var)
	     (mark-live-end (car live-end-stack) var))
	    (else
	     (mark-live-begin (cadr live-begin-stack) var)
	     (mark-live-end (cadr live-end-stack) var)
	     (let ((begin-stack live-begin-stack)
		   (end-stack live-end-stack))
		[assert (begin-stack end-stack)
			(eq? (cadr begin-stack) (cadr end-stack))])
	     ))
	 (set! live-begin-stack #f)
	 (set! live-end-stack #f)
	 ;(delete! var.live-begin-stack)
	 ;(delete! var.live-end-stack)
	 )))

(define-nmethod (Param.rev-live surrounding-fun)
   'do-nothing)
(define-nmethod (This-Decl.rev-live surrounding-fun)
   'do-nothing)
(define-nmethod (Arguments-Decl.rev-live surrounding-fun)
   'do-nothing)

(define (add-nodes! tree)
   (verbose " add-let-nodes")
   (intro! tree #f))

(define (do-intro! n walk! default-walk!)
   ;(delete! n.live-ends)
   (with-access::Node n (live-ends live-begins)
      (set! live-ends #f)
      (if live-begins
	  (let ((v (car live-begins)))
	     (if (null? (cdr live-begins))
		 ;(delete! n.live-begins)
		 (set! live-begins #f)
		 (set! live-begins (cdr live-begins)))
	     (walk! (instantiate::Let*
		       (vassigs (list (var-assig v (new-undefined))))
		       (body n))))
	  (default-walk! n))))
   
(define-nmethod (Node.intro!)
   (do-intro! this walk! default-walk!))

(define-nmethod (Let*.intro!)
   (default-walk! this)
   (with-access::Let* this (vassigs body)
      (when (Let*? body)
	 (set! vassigs (append! vassigs (Let*-vassigs body)))
	 (set! body (Let*-body body))))
   this)
       
(define-nmethod (Begin.intro!)
   ;(delete! this.live-ends)
   (with-access::Begin this (live-begins live-ends els)
      (set! live-ends #f)
      (if live-begins
       (do-intro! this walk! default-walk!)
       (let loop ((els els))
	  (if (null? els)
	      this
	      (let* ((el (car els))
		     (live-begins (or (Node-live-begins el) '()))
		     (live-ends (or (Node-live-ends el) '()))
		     (long-v (any (lambda (v)
				     (and (not (memq v live-ends))
					  v))
				  live-begins))
		     (filtered-l (filter!
				  (lambda (v)
				     (not (eq? v long-v)))
				  live-begins)))
		 (if (null? filtered-l)
		     ;(delete! el.live-begins)
		     (Node-live-begins-set! el #f)
		     (Node-live-begins-set! el filtered-l))
		 (cond
		    ((and long-v
			  (Vassig? el)
			  (eq? (Ref-var (Vassig-lhs el)) long-v))
		     (let ((let-n (instantiate::Let*
				     (vassigs (list el))
				     (body (instantiate::Sequence
					      (els (cdr els)))))))
			(set-car! els (walk! let-n))
			(set-cdr! els '())))
		    (long-v
		     (let ((let-n (instantiate::Let*
				     (vassigs `(,(var-assig long-v
							    (new-undefined))))
				     (body (instantiate::Sequence
					      ;; we need to create a new list.
					      (els (cons (car els)
							 (cdr els))))))))
			(set-car! els (walk! let-n))
			(set-cdr! els '())))
		    (else
		     (set-car! els (walk! el))))
		 (loop (cdr els))))))))
