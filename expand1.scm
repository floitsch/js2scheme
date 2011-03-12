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

(module expand1
   (import verbose
	   symbol
	   nodes
	   walk
	   label)
   (export (expand1! tree::Program)))

;; - add implicit "Labelled"-nodes for Loops, Funs and Case
;; - replace For with While
;; - add implicit "Fall-through"-node.
;; - add implicit "return undefined".
;; - replace x += exp with x = x + exp ...
;; - replace Calls that are actually method calls with Method-call
;; - replace void x; with (begin x, undefined)
;; - delete X is transformed to
;;    * delete o f (Delete-property-call) if X is of form o[f]
;;    * delete v   (Delete-call) otherwise
;; - For-in if lhs is not an identifier replace it with temporary variable, and put
;;   assignment into loop.
;; - replace ++x with tmp = x; x = x+1; tmp  (same for --) 11.4.4, 11.4.5
;; - replace x++ with x = x+1;  (same for --) 11.3.1, 11.3.2
(define (expand1! tree)
   (verbose " expand")
   (expand! tree #f))

(define-nmethod (Node.expand!)
   (default-walk! this))

(define-nmethod (For.expand!)
   (with-access::For this (body incr test init continue-label break-label)
      (let* ((continue-labelled (instantiate::Labelled
				   (id *default-continue-label-id*)
				   (body (walk! body))))
	     (while-body (if incr
			     (instantiate::Block
				(els (list continue-labelled
					   (walk! incr))))
			     continue-labelled))
	     (while (instantiate::While
		       (test (if test
				 (walk! test)
				 (instantiate::Bool
				    (val #t))))
		       (body while-body)))
	     (block (if init
			(instantiate::Block
			   (els (list (walk! init) while)))
			while))
	     (break-labelled (instantiate::Labelled
				(id *default-break-label-id*)
				(body block)))
	     (new-this break-labelled))
	 (with-access::Labelled continue-labelled (label)
	    (set! label continue-label))
	 (with-access::Labelled break-labelled (label)
	    (set! label break-label))
	 ;; TODO(flo): shrink this.
	 new-this)))

(define (Loop-expand! this::Loop default-walk!::procedure)
   (with-access::Loop this (body continue-label break-label)
      (let ((continue-labelled (instantiate::Labelled
				  (id *default-continue-label-id*)
				  (body body))))
	 (with-access::Labelled continue-labelled (label)
	    (set! label continue-label))
	 (set! body continue-labelled)
	 (let* ((old-this (default-walk! this))
		(break-labelled (instantiate::Labelled
				   (id *default-break-label-id*)
				   (body old-this))))
	    (with-access::Labelled break-labelled (label)
	       (set! label break-label))
	    ;; TODO(flo): shrink this.
	    break-labelled))))
   
(define-nmethod (While.expand!)
   (Loop-expand! this default-walk!))

(define-nmethod (For-In.expand!)
   (with-access::For-In this (lhs obj body)
      (cond
	 ((Ref? lhs)
	  (Loop-expand! this default-walk!))
	 ((Access? lhs)
	  (let* ((tmp (Decl-of-new-Var (gensym 'tmp)))
		 (tmp-var (Decl-var tmp))
		 (old-lhs lhs))
	     (set! lhs tmp)
	     (set! body (instantiate::Block
			   (els (list (instantiate::Accsig
					 (lhs old-lhs)
					 (val (var-reference tmp-var)))
				      body))))
	     (Loop-expand! this default-walk!)))
	 ((and (Var-Decl-List? lhs)
	       (Vassig? (car (Var-Decl-List-els lhs))))
	  ;; put the assignment outside the For-in (it must be evaluated only
	  ;; once).
	  ;; we are dropping the var-decl-list in the process
	  (let* ((assig (car (Var-Decl-List-els lhs)))
		 (var (Ref-var (Vassig-lhs assig))))
	     (set! lhs (var-reference var))
	     (walk! (instantiate::Block
		       (els (list assig
				  this))))))
	 ((Var-Decl-List? lhs)
	  ;; just drop the Var-decl-list
	  (set! lhs (car (Var-Decl-List-els lhs)))
	  (Loop-expand! this default-walk!))
	 (else (error "expand1"
		      "forgot something in For-in"
		      #f)))))

(define-nmethod (Switch-Clause.expand!)
   (with-access::Switch-Clause this (break-label body)
      (default-walk! this)
      (let ((new-body (instantiate::Labelled
			 (id #f)
			 (label break-label)
			 (body (instantiate::Block
				  (els (list body
					     (instantiate::Fall-Through))))))))
	 (set! body new-body)
	 this)))

(define-nmethod (Fun.expand!)
   (with-access::Fun this (return-label body)
      (default-walk! this)
      (let* ((return (instantiate::Return
			(expr (new-undefined))
			(label return-label)))
	     (new-body (instantiate::Labelled
			  (id #f)
			  (label return-label)
			  (body (instantiate::Block
				   (els (list body return)))))))
	 (set! body new-body)
	 this)))

(define-nmethod (Call.expand!)
   (with-access::Call this (op args)
      (default-walk! this)
      (if (Access? op)
	  (instantiate::Method-Call
	     (op op)
	     (args args))
	  this)))

(define-nmethod (Vassig-Op.expand!)
   (with-access::Vassig-Op this (lhs op val)
      (default-walk! this)
      (let ((new-rhs (instantiate::Binary
			(op op)
			(args (list (var-reference (Ref-var lhs)) val)))))
	 (instantiate::Vassig
	    (lhs lhs)
	    (val new-rhs)))))

(define-nmethod (Accsig-Op.expand!)
   (with-access::Accsig-Op this (lhs op val)
      (default-walk! this)
      (let* ((o (Access-obj lhs))
	     (field (Access-field lhs))
	     (tmp-o-id (gensym 'tmp-o))
	     (tmp-field-id (gensym 'tmp-field))
	     (tmp-o-decl (Decl-of-new-Var tmp-o-id))
	     (tmp-field-decl (Decl-of-new-Var tmp-field-id))
	     (init-o (instantiate::Init
			(lhs tmp-o-decl)
			(val o)))
	     (tmp-o-var (Decl-var tmp-o-decl))
	     (init-field (instantiate::Init
			    (lhs tmp-field-decl)
			    (val field)))
	     (tmp-field-var (Decl-var tmp-field-decl))

	     (access-lhs (instantiate::Access
			    (obj (var-reference tmp-o-var))
			    (field (var-reference tmp-field-var))))
	     (access-rhs (instantiate::Access
			    (obj (var-reference tmp-o-var))
			    (field (var-reference tmp-field-var))))
	     (rhs-binary (instantiate::Binary
			    (op op)
			    (args (list access-rhs val))))
	     (accsig (instantiate::Accsig
			(lhs access-lhs)
			(val rhs-binary)))
	     (sequence (instantiate::Sequence
			  (els (list init-o
				     init-field
				     accsig)))))
      (Var-internal?-set! tmp-o-var #t)
      (Var-internal?-set! tmp-field-var #t)
      sequence)))

(define-nmethod (Unary.expand!)
   (default-walk! this)
   (let* ((op (Unary-op this))
	  (args (Unary-args this))
	  (op-id (Ref-id op))
	  (expr (car args)))
      (cond
	 ((eq? op-id 'void)
	  (instantiate::Sequence
	     (els `(,expr ,(new-undefined)))))
	 ((eq? op-id 'delete)
	  (cond
	     ((Access? expr)
	      (with-access::Access expr (obj field)
		 (instantiate::Delete-Property-Call
		    (op op)
		    (args (list obj field)))))
	     (else
	      (instantiate::Delete-Call
		 (op op)
		 (args (list expr))))))
	 ((and (or (eq? op-id '++)
		   (eq? op-id '--))
	       (Access? expr))
	  ;; 11.4.4, 11.4.5
	  (let* ((obj (Access-obj expr))
		 (field (Access-field expr))
		 (tmp-obj-id (gensym 'o))
		 (tmp-obj-decl (Decl-of-new-Var tmp-obj-id))
		 (tmp-obj-var (Decl-var tmp-obj-decl))
		 (tmp-prop-id (gensym 'prop))
		 (tmp-prop-decl (Decl-of-new-Var tmp-prop-id))
		 (tmp-prop-var (Decl-var tmp-prop-decl))
		 (any->number-var (id->runtime-var 'any->number))
		 (op-var (id->runtime-var (if (eq? op-id '++) '+ '-)))
		 (tmp-obj-ref (var-reference tmp-obj-var))
		 (tmp-prop-ref (var-reference tmp-prop-var))
		 (as-number (instantiate::Unary
			       (op (var-reference any->number-var))
			       (args (list (instantiate::Access
					      (obj tmp-obj-ref)
					      (field tmp-prop-ref)))))))
	     (instantiate::Sequence
		(els `(,(instantiate::Vassig
			   (lhs tmp-obj-decl)
			   (val obj))
		       ,(instantiate::Vassig
			   (lhs tmp-prop-decl)
			   (val field))
		       ,(instantiate::Accsig
			   (lhs (instantiate::Access
				   (obj (var-reference tmp-obj-var))
				   (field (var-reference tmp-prop-var))))
			   (val (instantiate::Binary
				   (op (var-reference op-var))
				   (args (list as-number
					       (instantiate::Number
						  (val "1"))))))))))))
	 ((or (eq? op-id '++)
	      (eq? op-id '--))
	  ;; 11.4.4, 11.4.5
	  (instantiate::Vassig
	     (lhs expr) ;; a variable (either Var-ref or Decl)
	     (val (instantiate::Binary
		     (op (var-reference (id->runtime-var
					 (if (eq? op-id '++) '+ '-))))
		     (args `(,(instantiate::Unary
				 (op (var-reference
				      (id->runtime-var 'any->number)))
				 (args `(,(var-reference (Ref-var expr)))))
			     ,(instantiate::Number
				 (val "1"))))))))
	 (else
	  this))))

(define-nmethod (Postfix.expand!)
   ;;11.3.1, 11.3.2
   (default-walk! this)
   (let* ((op (Postfix-op this))
	  (args (Postfix-args this))
	  (op-id (Ref-id op))
	  (expr (car args)))
      (if (Access? expr)
	  (let* ((obj (Access-obj expr))
		 (field (Access-field expr))
		 (tmp-return-id (gensym 'tmp))
		 (tmp-return-decl (Decl-of-new-Var tmp-return-id))
		 (tmp-return-var (Decl-var tmp-return-decl))
		 (tmp-obj-id (gensym 'o))
		 (tmp-obj-decl (Decl-of-new-Var tmp-obj-id))
		 (tmp-obj-var (Decl-var tmp-obj-decl))
		 (tmp-prop-id (gensym 'prop))
		 (tmp-prop-decl (Decl-of-new-Var tmp-prop-id))
		 (tmp-prop-var (Decl-var tmp-prop-decl))
		 (any->number-var (id->runtime-var 'any->number))
		 (op-var (id->runtime-var (if (eq? op-id '++) '+ '-))))
	     (instantiate::Sequence
		(els `(,(instantiate::Vassig
			   (lhs tmp-obj-decl)
			   (val obj))
		       ,(instantiate::Vassig
			   (lhs tmp-prop-decl)
			   (val field))
		       ,(instantiate::Vassig
			   (lhs tmp-return-decl)
			   (val
			    (instantiate::Unary
			       (op (var-reference any->number-var))
			       (args `(,(instantiate::Access
					   (obj (var-reference tmp-obj-var))
					   (field (var-reference
						   tmp-prop-var))))))))
		       ,(instantiate::Accsig
			   (lhs (instantiate::Access
				   (obj (var-reference tmp-obj-var))
				   (field (var-reference tmp-prop-var))))
			   (val (instantiate::Binary
				   (op (var-reference op-var))
				   (args `(,(var-reference tmp-return-var)
					   ,(instantiate::Number
					       (val "1")))))))
		       ,(var-reference tmp-return-var)))))
	  (let* ((tmp-return-id (gensym 'tmp))
		 (tmp-return-decl (Decl-of-new-Var tmp-return-id))
		 (tmp-return-var (Decl-var tmp-return-decl))
		 (any->number-var (id->runtime-var 'any->number))
		 (op-var (id->runtime-var (if (eq? op-id '++) '+ '-)))
		 (expr-var (Ref-var expr)))
	     (instantiate::Sequence
		(els `(,(instantiate::Vassig
			   (lhs tmp-return-decl)
			   (val (instantiate::Unary
				   (op (var-reference any->number-var))
				   (args (list expr)))))
		       ,(var-assig expr-var
				   (instantiate::Binary
				      (op (var-reference op-var))
				      (args `(,(var-reference tmp-return-var)
					      ,(instantiate::Number
						  (val "1"))))))
		       ,(var-reference tmp-return-var))))))))
