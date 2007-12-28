(module liveness
   (include "protobject.sch")
   (include "nodes.sch")
   (include "tools.sch")
   (option (loadq "protobject-eval.sch"))
   (import protobject
	   verbose
	   nodes
	   var)
   (export (liveness tree::pobject)))

;; should be one of the latest passes before emitting the code.
;; the liveness information is stored within the nodes, and modifying these
;; nodes might invalidate the information.

;; very simple liveness analysis:
;; every node represents a nesting-level.
;; we accumulate the levels (in the 'nesting'-var), and whenever a var is used
;; we define the var's range as the shared levels.

(define (liveness tree)
   (verbose " liveness")
   (overload traverse live (Node
			    Program
			    Fun
			    Begin
			    Var-ref
			    Vassig
			    Accsig
			    If
			    While
			    Do
			    For-in
			    Switch
			    Try
			    Call
			    Method-call
			    New
			    Access
			    Array
			    Obj-init
			    ;; HACK (bad nodes-macros)
			    Bind-exit-invoc)
	     (set! (node 'Node).proto.default-traverse-value '(() . ()))
	     (tree.traverse '())))

(define (union . Ls)
   (let ((ht (make-eq-hashtable)))
      (for-each
       (lambda (l)
	  (for-each (lambda (v)
		       (hashtable-put! ht v #t))
		    l))
       Ls)
      (hashtable-key-list ht)))

(define (difference l1 l2)
   (let ((ht (make-eq-hashtable)))
      (for-each (lambda (v) (hashtable-put! ht v #t)) l1)
      (for-each (lambda (v) (hashtable-remove! ht v)) l2)
      (hashtable-key-list ht)))

(define (intersection . Ls)
   (define (intersection2 l1-ht l2)
      (let ((res-ht (make-eq-hashtable)))
	 (for-each (lambda (v) (if (hashtable-get l1-ht v)
				   (hashtable-put! res-ht v #t)))
		   l2)
	 res-ht))
   (if (null? Ls)
       '()
       (let ((res-ht (make-eq-hashtable)))
	  (for-each (lambda (v) (hashtable-put! res-ht v #t)) (car Ls))
	  (let loop ((rest-Ls (cdr Ls))
		     (res-ht res-ht))
	     (if (null? rest-Ls)
		 (hashtable-key-list res-ht)
		 (loop (cdr rest-Ls)
		       (intersection2 res-ht (car rest-Ls))))))))

(define (sequential-live l)
   (let loop ((read/writes l)
	      (read-acc '())
	      (write-acc '()))
      (if (null? read/writes)
	  (cons read-acc write-acc)
	  (let* ((read/write (car read/writes))
		 (new-reads (difference (car read/write) write-acc))
		 (read-union (union read-acc new-reads))
		 (write-union (union write-acc (cdr read/write))))
	     (loop (cdr read/writes)
		   read-union
		   write-union)))))

;; variable 'var' is used at nesting level 'nesting'.
(define (update-liveness! var nesting)
   (let* ((live-begin-stack var.live-begin-stack)
	  (live-end-stack var.live-end-stack))
      (cond
	 (var.escapes?
	  'do-nothing)
	 (var.external?
	  'do-nothing)
	 (var.eval?
	  'do-nothing)
	 ((not live-begin-stack)
	  (begin
	     (set! var.live-begin-stack nesting)
	     (set! var.live-end-stack nesting)))
	 ;; common case (same level)
	 ((eq? (cdr nesting) (cdr live-begin-stack))
	  (set! var.live-end-stack nesting))
	 (else
	  (let* ((begin-length (length live-begin-stack))
		 (nesting-length (length nesting))
		 (min-length (min begin-length nesting-length))
		 (shorted-begin (list-tail live-begin-stack
					   (-fx begin-length min-length)))
		 (shorted-nesting (list-tail nesting
					     (-fx nesting-length min-length))))
	     (let loop ((s-begin shorted-begin)
			(s-end shorted-nesting))
		(cond
		   ((null? s-begin)
		    (error #f "Must not happen: liveness" '()))
		   ((eq? (cdr s-begin) (cdr s-end))
		    ;; this even works for 'if/then/else'
		    ;; although it isn't optimal.
		    ;; suppose x is used in the test, and
		    ;; the else branch.
		    ;; the x is not used in the 'then'-branch.
		    ;; this simple analysis marks it as used
		    ;; even in the 'then'-branch.
		    (set! var.live-begin-stack s-begin)
		    (set! var.live-end-stack s-end))
		   (else (loop (cdr s-begin)
			       (cdr s-end))))))))))
   
(define-pmethod (Node-live nesting)
   (this.traverse1 (cons this nesting)))

(define-pmethod (Program-live nesting)
   ;; we don't care for escaping... vars.
   ;; => the nesting can restart at 0 here.
   (this.traverse1 (list this))
   ;; return empty sets
   '(()()))

(define-pmethod (Fun-live nesting)
   ;; we don't care for escaping... vars.
   ;; => the nesting can restart at 0 here.
   (this.traverse1 (list this))
   ;; return empty sets
   '(()()))

(define-pmethod (Var-ref-live nesting)
   (define (transitive-with-var var)
      (if (inherits-from? var (node 'Intercepted-var))
	  (transitive-with-var var.intercepted)
	  var))

   (let ((var (transitive-with-var this.var)))
      (update-liveness! var (cons this nesting))
      (cons (list var) '())))

(define-pmethod (Vassig-live nesting)
   (let* ((new-nesting (cons this nesting))
	  (ignored (this.lhs.traverse new-nesting))
	  (rvalue-read/write (this.val.traverse new-nesting)))
      (cons (car rvalue-read/write)
	    (union (cdr rvalue-read/write) (list this.lhs.var)))))

(define-pmethod (Accsig-live nesting)
   (let* ((new-nesting (cons this nesting)))
      (sequential-live (list (this.lhs.traverse new-nesting)
			     (this.val.traverse new-nesting)))))

(define-pmethod (Begin-live nesting)
   (let ((new-nesting (cons this nesting)))
      (sequential-live (map (lambda (n) (n.traverse new-nesting))
			    this.els))))

(define-pmethod (If-live nesting)
   (let* ((new-nesting (cons this nesting))
	  (test-read/write (this.test.traverse new-nesting))
	  (then-read/write (this.then.traverse new-nesting))
	  (else-read/write (this.else.traverse new-nesting))
	  (then-new-read (difference (car then-read/write)
				     (cdr test-read/write)))
	  (else-new-read (difference (car else-read/write)
				     (cdr test-read/write)))
	  (new-read (union (car test-read/write)
			   then-new-read
			   else-new-read))
	  (new-write (union (cdr test-read/write)
			    ;; only if written in both branches.
			    (intersection (cdr then-read/write)
					  (cdr else-read/write)))))
      (cons new-read new-write)))

(define-pmethod (While-live nesting)
   (let* ((new-nesting (cons this nesting))
	  (test-read/write (this.test.traverse new-nesting))
	  (body-read/write (this.body.traverse new-nesting))
	  (body-new-read (difference (car body-read/write)
				     (cdr test-read/write)))
	  (new-read (union (car test-read/write)
			   body-new-read))
	  ;; don't add body, as it might not be executed
	  (new-write (cdr test-read/write)))
      
      ;; body-new-read contains vars, that are potentially read before
      ;; being assigned, (but that are potentially assigned in the body.)
      ;; The variable is hence potentially live during the whole loop.
      (for-each (lambda (var)
		   (update-liveness! var new-nesting))
		body-new-read)
      (cons new-read new-write)))

(define-pmethod (Do-live nesting)
   (let* ((new-nesting (cons this nesting))
	  (body-read/write (this.body.traverse new-nesting))
	  (test-read/write (this.test.traverse new-nesting))
	  (test-new-read (difference (car test-read/write)
				     (cdr body-read/write)))
	  (new-read (union (car body-read/write)
			   test-new-read))
	  (new-write (union (cdr body-read/write)
			    (cdr test-read/write))))
      
      ;; (cdr body-read/write) contains vars, that are potentially read before
      ;; being assigned, but that are potentially assigned in the body.
      ;; The variable is hence live during the whole loop.
      (for-each (lambda (var)
		   (update-liveness! var new-nesting))
		(cdr body-read/write))
      (cons new-read new-write)))

(define-pmethod (For-in-live nesting)
   (let ((new-nesting (cons this nesting)))
      (this.lhs.traverse new-nesting)
      ;; lhs is at least alive during whole loop.
      (update-liveness! this.lhs.var new-nesting)
      (let* ((obj-read/write (this.obj.traverse new-nesting))
	     (body-read/write (this.body.traverse new-nesting))
	     (new-body-read (difference (car body-read/write)
					;; the lhs-var is written too.
					(cons this.lhs.var
					      (cdr obj-read/write))))
	     (new-read (union (car obj-read/write)
			      new-body-read))
	     ;; we can't count obj-read/write, as the obj might
	     ;; not have any properties.
	     (new-write (cdr obj-read/write)))

      ;; new-body-read contains vars, that are potentially read before
      ;; being assigned, but that are potentially assigned in the body.
      ;; The variable is hence live during the whole loop.
      (for-each (lambda (var)
		   (update-liveness! var new-nesting))
		new-body-read)
      (cons new-read new-write))))

(define-pmethod (Switch-live nesting)
   (let* ((new-nesting (cons this nesting))
	  (key-read/write (this.key.traverse new-nesting))
	  (case-read/writes (map (lambda (n)
				    (n.traverse new-nesting))
				 this.cases))
	  (case-reads (difference (apply union (map car case-read/writes))
				  (cdr key-read/write)))
	  (new-reads (union (car key-read/write)
			    case-reads))
	  (new-writes (union (cdr key-read/write)
			     (apply intersection (map cdr case-read/writes)))))
      (cons new-reads new-writes)))

(define-pmethod (Try-live nesting)
   (let* ((new-nesting (cons this nesting))
	  (body-read/write (this.body.traverse new-nesting))
	  (catch-read/write (if this.catch
				(this.catch.traverse new-nesting)
				'(()())))
	  (finally-read/write (if this.finally
				  (this.finally.traverse new-nesting)
				  '(()())))
	  ;; we can't assume, that body-writes were really done...
	  ;; we can't even be sure, that the catch is completely executed, but
	  ;; we want the read-variables directly after the try/catch, and this
	  ;; means, that the catch didn't throw another exception (otherwise we
	  ;; wouldn't be directly after the try/catch
	  (new-read (union (car body-read/write)
			   (car catch-read/write)
			   (difference (car finally-read/write)
				       (cdr catch-read/write))))
	  ;; the write we want here, is the write directly after the try/catch.
	  ;; which means, that the 'catch' or 'finally' didn't throw an
	  ;; exception.
	  (new-write (union (cdr catch-read/write)
			    (cdr finally-read/write))))
      (cons new-read new-write)))

(define-pmethod (Call-live nesting)
   (let ((new-nesting (cons this nesting)))
      (sequential-live (map (lambda (n)
			       (n.traverse new-nesting))
			    (cons this.op this.args)))))

(define-pmethod (Method-call-live nesting)
   (let ((new-nesting (cons this nesting)))
      (sequential-live (map (lambda (n)
			       (n.traverse new-nesting))
			    (cons* this.op this.args)))))

(define-pmethod (New-live nesting)
   (let ((new-nesting (cons this nesting)))
      (sequential-live (map (lambda (n) (n.traverse new-nesting))
			    (cons this.class this.args)))))

(define-pmethod (Access-live nesting)
   (let ((new-nesting (cons this nesting)))
      (sequential-live (list (this.obj.traverse new-nesting)
			     (this.field.traverse new-nesting)))))

(define-pmethod (Array-live nesting)
   (let ((new-nesting (cons this nesting)))
      (sequential-live (map (lambda (n) (n.traverse new-nesting))
			    this.els))))

(define-pmethod (Obj-init-live nesting)
   (let ((new-nesting (cons this nesting)))
      (sequential-live (map (lambda (n) (n.traverse new-nesting))
			    this.props))))

(define-pmethod (Bind-exit-invoc-live nesting)
   (if this.expr
       (this.expr.traverse (cons this nesting))
       this.default-traverse-value))
