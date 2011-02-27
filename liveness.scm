(module liveness
   (include "tools.sch")
   (import walk
	   verbose
	   nodes)
   (export (liveness tree::Program)))

;; should be one of the latest passes before emitting the code.
;; the liveness information is stored within the nodes, and modifying these
;; nodes might invalidate the information.

(define (liveness tree)
   (verbose " liveness")
   (live tree #f '()))

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
   (with-access::Var var (live-begin-stack live-end-stack escapes?
					   external? eval?)
      (cond
	 (escapes?
	  'do-nothing)
	 (external?
	  'do-nothing)
	 (eval?
	  'do-nothing)
	 ((not live-begin-stack)
	  (begin
	     (set! live-begin-stack nesting)
	     (set! live-end-stack nesting)))
	 ;; common case (same level)
	 ((eq? (cdr nesting) (cdr live-begin-stack))
	  (set! live-end-stack nesting))
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
		    (set! live-begin-stack s-begin)
		    (set! live-end-stack s-end))
		   (else (loop (cdr s-begin)
			       (cdr s-end))))))))))
   
;; very simple liveness analysis:
;; every node represents a nesting-level.
;; we accumulate the levels (in the 'nesting'-var), and whenever a var is used
;; we define the var's range as the shared levels.

(define-nmethod (Node.live nesting)
   (error #f
	  "Internal Error. Forgot node type."
	  (class-name (object-class this))))

(define-nmethod (Labelled.live nesting)
   (error #f
	  "Internal Error. Node should not exist anymore."
	  (class-name (object-class this))))

;; Throw is treated specially.
(define-nmethod (Flow-Interruption.live nesting)
   (error #f
	  "Internal Error. Node should not exist anymore."
	  (class-name (object-class this))))
(define-nmethod (For.live nesting)
   (error #f
	  "Internal Error. Node should not exist anymore."
	  (class-name (object-class this))))

(define-nmethod (Program.live nesting)
   ;; we don't care for escaping... vars.
   ;; => the nesting can restart at 0 here.
   (default-walk this (list this))
   ;; return empty sets (should not be necessary).
   '(()()))

(define-nmethod (Fun.live nesting)
   ;; we don't care for escaping... vars.
   ;; => the nesting can restart at 0 here.
   (default-walk this (list this))
   ;; return empty sets
   '(()()))

(define-nmethod (Ref.live nesting)
   (define (transitive-with-var var)
      (if (Intercepted-Var? var)
	  (with-access::Intercepted-Var var (intercepted)
	     (transitive-with-var intercepted))
	  var))

   (let ((var (transitive-with-var (Ref-var this))))
      (update-liveness! var (cons this nesting))
      (cons (list var) '())))

(define-nmethod (NOP.live nesting)
   (default-walk this nesting)
   '(() . ()))

(define-nmethod (Vassig.live nesting)
   (with-access::Vassig this (lhs val)
      (let* ((new-nesting (cons this nesting))
	     (ignored (walk lhs new-nesting))
	     (rvalue-read/write (walk val new-nesting)))
	 (cons (car rvalue-read/write)
	       (union (cdr rvalue-read/write) (list (Ref-var lhs)))))))

(define-nmethod (Accsig.live nesting)
   (with-access::Accsig this (lhs val)
      (let* ((new-nesting (cons this nesting)))
	 (sequential-live (list (walk lhs new-nesting)
				(walk val new-nesting))))))

(define-nmethod (Begin.live nesting)
   (with-access::Begin this (els)
      (let ((new-nesting (cons this nesting)))
	 (sequential-live (map (lambda (n) (walk n new-nesting))
			       els)))))

(define-nmethod (If.live nesting)
   (with-access::If this (test then else)
      (let* ((new-nesting (cons this nesting))
	     (test-read/write (walk test new-nesting))
	     (then-read/write (walk then new-nesting))
	     (else-read/write (walk else new-nesting))
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
	 (cons new-read new-write))))

(define-nmethod (While.live nesting)
   (with-access::While this (test body)
      (let* ((new-nesting (cons this nesting))
	     (test-read/write (walk test new-nesting))
	     (body-read/write (walk body new-nesting))
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
	 (cons new-read new-write))))

(define-nmethod (Do.live nesting)
   (with-access::Do this (body test)
      (let* ((new-nesting (cons this nesting))
	     (body-read/write (walk body new-nesting))
	     (test-read/write (walk test new-nesting))
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
	 (cons new-read new-write))))

(define-nmethod (For-In.live nesting)
   (with-access::For-In this (lhs obj body)
      (let ((new-nesting (cons this nesting)))
	 (walk lhs new-nesting)
	 ;; lhs is at least alive during whole loop.
	 (update-liveness! (Ref-var lhs) new-nesting)
	 (let* ((obj-read/write (walk obj new-nesting))
		(body-read/write (walk body new-nesting))
		(new-body-read (difference (car body-read/write)
					   ;; the lhs-var is written too.
					   (cons (Ref-var lhs)
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
	    (cons new-read new-write)))))

(define-nmethod (Switch.live nesting)
   (with-access::Switch this (key cases)
      (let* ((new-nesting (cons this nesting))
	     (key-read/write (walk key new-nesting))
	     (case-read/writes (map (lambda (n)
				       (walk n new-nesting))
				    cases))
	     (case-reads (difference (apply union (map car case-read/writes))
				     (cdr key-read/write)))
	     (new-reads (union (car key-read/write)
			       case-reads))
	     (new-writes (union (cdr key-read/write)
				(apply intersection (map cdr case-read/writes)))))
	 (cons new-reads new-writes))))

(define-nmethod (Fall-Through.live nesting)
   '(() . ()))

(define-nmethod (Case.live nesting)
   (with-access::Case this (expr body)
      (let ((new-nesting (cons this nesting)))
	 (sequential-live (list (walk expr new-nesting)
				(walk body new-nesting))))))

(define-nmethod (Default.live nesting)
   (with-access::Default this (body)
      (walk body (cons this nesting))))

(define-nmethod (Try.live nesting)
   (with-access::Try this (body catch finally)
      (let* ((new-nesting (cons this nesting))
	     (body-read/write (walk body new-nesting))
	     (catch-read/write (if catch
				   (walk catch new-nesting)
				   '(()())))
	     (finally-read/write (if finally
				     (walk finally new-nesting)
				     '(()())))
	     ;; we can't assume, that body-writes were really done...
	     ;; we can't even be sure, that the catch is completely executed,
	     ;; but we want the read-variables directly after the try/catch,
	     ;; and this means, that the catch didn't throw another exception
	     ;; (otherwise we wouldn't be directly after the try/catch
	     (new-read (union (car body-read/write)
			      (car catch-read/write)
			      (difference (car finally-read/write)
					  (cdr catch-read/write))))
	     ;; the write we want here, is the write directly after the
	     ;; try/catch.
	     ;; which means, that the 'catch' or 'finally' didn't throw an
	     ;; exception.
	     (new-write (union (cdr catch-read/write)
			       (cdr finally-read/write))))
	 (cons new-read new-write))))

(define-nmethod (Call.live nesting)
   (with-access::Call this (op args)
      (let ((new-nesting (cons this nesting)))
	 (sequential-live (map (lambda (n)
				  (walk n new-nesting))
			       (cons op args))))))

(define-nmethod (Method-Call.live nesting)
   (with-access::Method-Call this (op args)
      (let ((new-nesting (cons this nesting)))
	 (sequential-live (map (lambda (n)
				  (walk n new-nesting))
			       (cons op args))))))

(define-nmethod (New.live nesting)
   (with-access::New this (class args)
      (let ((new-nesting (cons this nesting)))
	 (sequential-live (map (lambda (n) (walk n new-nesting))
			       (cons class args))))))

(define-nmethod (Access.live nesting)
   (with-access::Access this (obj field)
      (let ((new-nesting (cons this nesting)))
	 (sequential-live (list (walk obj new-nesting)
				(walk field new-nesting))))))

(define-nmethod (Array.live nesting)
   (with-access::Array this (els)
      (let ((new-nesting (cons this nesting)))
	 (sequential-live (map (lambda (n) (walk n new-nesting))
			       els)))))

(define-nmethod (Obj-Init.live nesting)
   (with-access::Obj-Init this (props)
      (let ((new-nesting (cons this nesting)))
	 (sequential-live (map (lambda (n) (walk n new-nesting))
			       props)))))

(define-nmethod (Bind-Exit-Invoc.live nesting)
   (with-access::Bind-Exit-Invoc this (expr)
      (if expr
	  (walk expr (cons this nesting))
	  '(() . ()))))

(define-nmethod (Bind-Exit.live nesting)
   (with-access::Bind-Exit this (body)
      (walk body (cons this nesting))))

(define-nmethod (Throw.live nesting)
   (with-access::Throw this (expr)
      (walk expr (cons this nesting))))

(define-nmethod (With.live nesting)
   (with-access::With this (obj body)
      (let ((new-nesting (cons this nesting)))
	 (sequential-live (list (walk obj new-nesting)
				(walk body new-nesting))))))

(define-nmethod (Decl-Intercept.live nesting)
   (with-access::Decl-Intercept this (decl body)
      (let ((new-nesting (cons this nesting)))
	 (walk decl new-nesting)
	 (walk body new-nesting))))

(define-nmethod (Literal.live nesting)
   '(() . ()))

(define-nmethod (Array.live nesting)
   (with-access::Array this (els)
      (sequential-live (map (lambda (n) (walk n nesting)) els))))

(define-nmethod (Array-Element.live nesting)
   (with-access::Array-Element this (expr)
      (walk expr nesting)))

(define-nmethod (Property-Init.live nesting)
   (with-access::Property-Init this (val)
      (walk val nesting)))

(define-nmethod (Reg-Exp.live nesting)
   '(() . ()))
