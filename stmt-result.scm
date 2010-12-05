(module stmt-result
   (import walk
	   verbose
	   config
	   nodes)
   (export (stmt-result tree)))

;; this pass requires the whole program to be nearly unmodified. It must hence
;; be executed quite early. Function declarations should however already be at
;; the top of the program. As this pass introduces a temporary variable it has
;; to be executed after symbol-resolution.
;;
;; usually the result of statements can't be used. But when a statement is
;; inside an 'eval' then statements might have results.
;; Difficulty: some statements (like NOP) do not return results, and any
;; previously computed result should be returned. It is hence not sufficient to
;; simply search the last statement and return its result.
;; Ex:
;;   eval('3; if(true) /*NOP*/; else 5;')
;; should return 3. (the NOP does not return any value, and the last computed
;; value is hence 3).
;;
;; we simply introduce a new variable which is then returned at the end of the
;; program. Every top-level statement updates the variable. Not really
;; efficient, but otherwise we would need to detect dead code (ie breaks,
;; continues, ...) to limit the statements that can update the var.

(define (stmt-result tree)
   (when (thread-parameter 'eval?)
      (verbose "stmt-result")
      (stmt-res! tree #f #f)))

(define-nmethod (Node.stmt-res! return-var)
   ;; we are not a statement. -> we have a return-value -> update var.
   (var-assig return-var this))

(define-nmethod (Program.stmt-res! ignored)
   (with-access::Program this (body)
      (let* ((return-decl (Decl-of-new-Var (gensym 'eval-return)))
	     (return-var (Decl-var return-decl)))
	 ;; body is a Block
	 (if (config 'strict-ecma)
	     ;; according to ecma Section 14
	     ;;      'SourceElements: SourceElements SourceElement'
	     ;; one should always return the last result. (even if it's a NOP).
	     ;; strict-ecma follows this rule.
	     ;;
	     ;; otherwise we consider the eval to be enclosed into a block (which is
	     ;; what everybody else seems to do atm).
	     (if (null? (Begin-els body))
		 'do-nothing
		 (let ((last-source-el-p (last-pair (Begin-els body))))
		    (set-car! last-source-el-p
			      (walk! (car last-source-el-p) return-var))))
	     (set! body (walk! body return-var)))
	 ;; now the body updates the variable, make sure it is initialized, and
	 ;; return it.
	 (set! body
	       (instantiate::Block
		  (els (list (instantiate::Vassig
				(lhs return-decl)
				(val (new-undefined)))
			     body
			     (var-reference return-var)))))
	 this)))

(define-nmethod (Block.stmt-res! return-var)
   (with-access::Block this (els)
      (set! els (map! (lambda (n)
			 (walk! n return-var))
		      els))
      this))

(define-nmethod (Init.stmt-res! return-var)
   ;; var xxx = yyy
   ;; returns 'empty'
   this)

(define-nmethod (Decl.stmt-res! return-var)
   ;; var xxx
   ;; returns 'empty'
   this)

(define-nmethod (Demoted-Decl.stmt-res! return-var)
   ;; var xxx which has been transformed into xxx
   this)

(define-nmethod (NOP.stmt-res! return-var)
   ;; NOP returns 'empty'
   this)

(define-nmethod (If.stmt-res! return-var)
   (with-access::If this (then else)
      ;; If updates return-var only if both branches update.
      (set! then (walk! then return-var))
      (set! else (walk! else return-var))
      this))

(define-nmethod (Loop.stmt-res! return-var)
   (with-access::Loop this (body)
      (set! body (walk! body return-var))
      this))

(define-nmethod (Continue.stmt-res! return-var)
   ;; Continue returns 'empty'
   this)

(define-nmethod (Break.stmt-res! return-var)
   ;; Break returns 'empty'
   this)

(define-nmethod (Return.stmt-res! return-var)
   (error "stmt-res"
	  "'return' is not allowed at top-level"
	  #f))

(define-nmethod (With.stmt-res! return-var)
   (with-access::With this (body)
      (set! body (walk! body return-var))
      this))

(define-nmethod (Switch.stmt-res! return-var)
   (with-access::Switch this (cases)
      (set! cases (map! (lambda (n)
			   (walk! n return-var))
			cases))
      this))

(define-nmethod (Fall-Through.stmt-res! return-var)
   this)

(define-nmethod (Switch-Clause.stmt-res! return-var)
   (with-access::Switch-Clause this (body)
      (set! body (walk! body return-var))
      this))

(define-nmethod (Labelled.stmt-res! return-var)
   (with-access::Labelled this (body)
      (set! body (walk! body return-var))
      this))

(define-nmethod (Throw.stmt-res! return-var)
   this)

(define-nmethod (Try.stmt-res! return-var)
   (with-access::Try this (body catch finally)
      (set! body (walk! body return-var))
      (when catch (set! catch (walk! catch return-var)))
      (when finally (set! finally (walk! finally return-var)))
      this))

(define-nmethod (Catch.stmt-res! return-var)
   (with-access::Catch this (body)
      (set! body (walk! body return-var))
      this))

(define-nmethod (Fun-Binding.stmt-res! return-var)
   this)
