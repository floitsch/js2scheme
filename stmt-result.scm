(module stmt-result
   (include "protobject.sch")
   (include "nodes.sch")
   (option (loadq "protobject-eval.sch"))
   (import protobject
	   verbose
	   var
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
      (overload traverse! stmt-res! (Node
				   
				   Program
				   ;; statements:
				   Block
				   Init
				   Decl ;; var-declaration
				   Var-ref ;; some decls have been converted.
				   NOP
				   If
				   Loop
				   Continue
				   Break
				   Return
				   With
				   Switch
				   Fall-through
				   Switch-clause
				   Labelled
				   Throw
				   Try
				   Catch
				   Fun-binding)
		(tree.traverse! #f))))

(define-pmethod (Node-stmt-res! return-var)
   ;; we are not a statement. -> we have a return-value -> update var.
   (return-var.assig this))

(define-pmethod (Program-stmt-res! ignored)
   (let* ((return-decl (Decl-of-new-Var (gensym 'eval-return)))
	  (return-var return-decl.var)
	  (body this.body))
      ;; body is a Block
      (if (config 'strict-ecma)
	  ;; according to ecma Section 14
	  ;;      'SourceElements: SourceElements SourceElement'
	  ;; one should always return the last result. (even if it's a NOP).
	  ;; strict-ecma follows this rule.
	  ;;
	  ;; otherwise we consider the eval to be enclosed into a block (which is
	  ;; what everybody else seems to do atm).
	  (if (null? body.els)
	      'do-nothing
	      (let ((last-source-el-p (last-pair body.els)))
		 (set-car! last-source-el-p
			   ((car last-source-el-p).traverse! return-var))))
	  (set! this.body (body.traverse! return-var)))
      ;; now the body updates the variable, make sure it is initialized, and
      ;; return it.
      (set! this.body
	    (new-node Block
		      (list (new-node Vassig
				      return-decl
				      (new-node Undefined))
			    this.body
			    (return-var.reference))))
      this))

(define-pmethod (Block-stmt-res! return-var)
   (set! this.els (map! (lambda (n)
			   (n.traverse! return-var))
			this.els))
   this)

(define-pmethod (Init-stmt-res! return-var)
   ;; var xxx = yyy
   ;; returns 'empty'
   this)

(define-pmethod (Decl-stmt-res! return-var)
   ;; var xxx
   ;; returns 'empty'
   this)

(define-pmethod (Var-ref-stmt-res! return-var)
   (if this.was-decl?
       ;; var xxx which has been transformed into xxx
       this
       (pcall this Node-stmt-res! return-var)))

(define-pmethod (NOP-stmt-res! return-var)
   ;; NOP returns 'empty'
   this)

(define-pmethod (If-stmt-res! return-var)
   ;; If updates return-var only if both branches update.
   (set! this.then (this.then.traverse! return-var))
   (set! this.else (this.else.traverse! return-var))
   this)

(define-pmethod (Loop-stmt-res! return-var)
   (set! this.body (this.body.traverse! return-var))
   this)

(define-pmethod (Do-stmt-res! return-var)
   (set! this.body (this.body.traverse! return-var))
   this)

(define-pmethod (Continue-stmt-res! return-var)
   ;; Continue returns 'empty'
   this)

(define-pmethod (Break-stmt-res! return-var)
   ;; Break returns 'empty'
   this)

(define-pmethod (Return-stmt-res! return-var)
   (error "stmt-res"
	  "'return' is not allowed at top-level"
	  #f))

(define-pmethod (With-stmt-res! return-var)
   (set! this.body (this.body.traverse! return-var))
   this)

(define-pmethod (Switch-stmt-res! return-var)
   (set! this.cases (map! (lambda (n)
			     (n.traverse! return-var))
			  this.cases))
   this)

(define-pmethod (Fall-through-stmt-res! return-var)
   this)

(define-pmethod (Switch-clause-stmt-res! return-var)
   (set! this.body (this.body.traverse! return-var))
   this)

(define-pmethod (Labelled-stmt-res! return-var)
   (set! this.body (this.body.traverse! return-var))
   this)

(define-pmethod (Throw-stmt-res! return-var)
   this)

(define-pmethod (Try-stmt-res! return-var)
   (set! this.body (this.body.traverse! return-var))
   (when this.catch (set! this.catch (this.catch.traverse! return-var)))
   this)

(define-pmethod (Catch-stmt-res! return-var)
   (set! this.body (this.body.traverse! return-var))
   this)

(define-pmethod (Fun-binding-stmt-res! return-var)
   this)
