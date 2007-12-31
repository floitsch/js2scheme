(module js-out
   (include "protobject.sch")
   (include "nodes.sch")
   (option (loadq "protobject-eval.sch"))
   (import parser
	   config
	   protobject
	   nodes
	   var
	   verbose)
   (export (js-out tree::pobject . Lp)))

;; this module has two purposes:
;; - pretty print of JavaScript
;; - get the function-strings. A function's toString has to return a textual
;; representation of the function. This module generates this string.
;;
;; without port, js-out simply assigns a str-property to Funs.
;; otherwise the whole program is printed into the given port.

(define (js-out tree . Lp)
   ;; procedure-body at end of file

(define *generate-function-strings?* #unspecified)

(define *function-out-str-port* #unspecified)
(define *inside-function?* #f)
(define *function-str-id* '?)
(define *function-strings-ht* (make-hashtable))

   
(define (assign-priorities!)
   (define (assign-priority! n-sym priority)
      (set! (node n-sym).proto.priority (pmethod () priority)))

   (define-pmethod (binary-priority)
      (+ 10 (case this.op.id
	       ((OR) 1)
	       ((&&) 2)
	       ((BIT_OR) 3)
	       ((^) 4)
	       ((&) 5)
	       ((== != === !==) 6)
	       ((< > <= >= instanceof in) 7)
	       ((<< >> >>>) 8)
	       ((+ -) 9)
	       ((* / %) 10)
	       (else
		(error "binary priority"
		       "bad op"
		       this.op.id)))))
   
   (assign-priority! 'Node #f)
   (assign-priority! 'Sequence 1)
   (assign-priority! 'Cond 2)
   (assign-priority! 'Var-ref 30)
   (assign-priority! 'Fun 25)
   (assign-priority! 'Vassig 3)
   (assign-priority! 'Vassig-op 3)
   (assign-priority! 'Accsig 3)
   (assign-priority! 'Accsig-op 3)
   (assign-priority! 'Call 25)
   (set! (node 'Binary).proto.priority binary-priority)
   (assign-priority! 'Unary 19)
   (assign-priority! 'Postfix 19)
   (assign-priority! 'New 25)
   (assign-priority! 'Access 25)
   (assign-priority! 'This 30)
   (assign-priority! 'Literal 30)
   (assign-priority! 'Array 30)
   (assign-priority! 'Obj-init 30)
   (assign-priority! 'Reg-exp 30))

(define (indent+ indent)
   (+ indent 3))

(define (indent! indent)
   (if (not (config 'compress?))
       (display (make-string indent #\space))))

(define (newline-out)
   (if (not (config 'compress?))
       (display "\n")))

(define (space-out)
   (if (not (config 'compress?))
       (display " ")))

(define (op->string op)
   (case op
      ((OR) "||")
      ((BIT_OR) "|")
      (else (symbol->string op))))

(define (block-body body indent needs-separation? newline-after?)
   (let ((body-block? (inherits-from? body (node 'Block))))
      (if body-block?
	  (begin
	     (space-out)
	     (body.traverse #f indent #f #f newline-after?)
	     #t)
	  (begin
	     (if (and (config 'compress?)
		      needs-separation?)
		 (display " ")
		 (newline-out))
	     (body.traverse #f (indent+ indent) #f)
	     #f))))

(define-macro (check-expr . Lbody)
   `(let* ((stmt? this.stmt?)
	   (this-priority (this.priority))
	   (needs-parentheses? (and (not stmt?)
				    this-priority
				    priority
				    (< this-priority priority)))
	   (in-for-in? (and (not needs-parentheses?) in-for-in?)))
       (if stmt? (indent! indent))
       (if needs-parentheses? (display "("))
       ,@Lbody
       (if needs-parentheses? (display ")"))
       (if stmt?
	   (begin
	      (display ";")
	      (newline-out)))))

(define (Block-out-without-braces stmt
				  priority
				  indent
				  in-for-init?)
   (if (inherits-from? stmt (node 'Block))
       (for-each (lambda (n)
		    (n.traverse #f indent #f))
		 stmt.els)
       (stmt.traverse #f indent #f)))

(define (stmt->block stmt)
   (cond
      ((inherits-from? stmt (node 'Block))
       stmt)
      ((inherits-from? stmt (node 'NOP))
       (new-node Block '()))
      (else
       (new-node Block (list stmt)))))

;; priority is only for expressions.
;; indent only for statements.
(define-pmethod (Node-out priority indent in-for-in?)
   (error "Node-out"
	  "forgot node type: "
	  (pobject-name this)))

(define-pmethod (Program-out priority indent in-for-in?)
   (Block-out-without-braces this.body
			     #f
			     0
			     #f)
   (when *generate-function-strings?*
      (set! this.function-str-ids-ht *function-strings-ht*)))

(define-pmethod (Sequence-out priority indent in-for-in?)
   (check-expr
    (let ((els this.els))
       ((car els).traverse this-priority indent in-for-in?)
       (for-each (lambda (n)
		    (display ",")
		    (space-out)
		    (n.traverse this-priority indent in-for-in?))
		 (cdr els)))))

(define-pmethod (Block-out priority indent in-for-in? . Lflags)
   (let ((indent? (if (null? Lflags)
		      #t
		      (car Lflags)))
	 (new-line-after? (if (null? Lflags)
			      #t
			      (cadr Lflags))))

      (if indent? (indent! indent))
      (display "{")
      (newline-out)
      (for-each (lambda (n)
		   (n.traverse #f (indent+ indent) #f))
		this.els)
      (indent! indent)
      (display "}")
      (if new-line-after? (newline-out))))

(define-pmethod (Var-out)
   (display (or this.generated
		this.id)))

(define-pmethod (Var-decl-list-out priority indent in-for-in?)
   (check-expr
    (display "var ")
    (let ((els this.els))
       ((car els).traverse 1 indent #f)
       (for-each (lambda (n)
		    (display ",")
		    (space-out)
		    (n.traverse 1 indent #f))
		 (cdr els)))))

(define-pmethod (Var-ref-out priority indent in-for-in?)
   (check-expr
    (if this.var
	(this.var.out)
	(display this.id))))

(define-pmethod (NOP-out priority indent in-for-in?)
   (indent! indent)
   (display ";")
   (newline-out))

(define-pmethod (If-out priority indent in-for-in?)
   (if (and (inherits-from? this.then (node 'If)) ;; nested if
	    (inherits-from? this.then.else (node 'NOP)) ;; has no else-branch
	    (not (inherits-from? this.else (node 'NOP)))) ;; but we have one
       (set! this.then (new-node Block (list this.then)))) ;; protect our else
   (indent! indent)
   (display "if")
   (space-out)
   (display "(")
   (this.test.traverse #f indent #f)
   (display ")")
   (let* ((else-branch? (not (inherits-from? this.else (node 'NOP))))
	  (then-block? (block-body this.then indent #f (not else-branch?))))
      (when else-branch?
	 (if then-block?
	     (space-out)
	     (indent! indent))
	 (display "else")
	 (block-body this.else indent #t #t))))

(define-pmethod (For-out priority indent in-for-in?)
   (indent! indent)
   (display "for")
   (space-out)
   (display "(")
   (when this.init (this.init.traverse #f (indent+ indent) #f))
   (display ";")
   (space-out)
   (when this.test (this.test.traverse #f (indent+ indent) #f))
   (display ";")
   (space-out)
   (when this.incr (this.incr.traverse #f (indent+ indent) #f))
   (display ")")
   (block-body this.body indent #f #t))

(define-pmethod (While-out priority indent in-for-in?)
   (indent! indent)
   (display "while")
   (space-out)
   (display "(")
   (this.test.traverse #f (indent+ indent) #f)
   (display ")")
   (block-body this.body indent #f #t))

(define-pmethod (Do-out priority indent in-for-in?)
   (indent! indent)
   (display "do")
   (if (block-body this.body indent #t #f)
       (space-out)
       (indent! indent))
   (display "while")
   (space-out)
   (display "(")
   (this.test.traverse #f (indent+ indent) #f)
   (display ");")
   (newline-out))

(define-pmethod (For-in-out priority indent in-for-in?)
   (indent! indent)
   (display "for")
   (space-out)
   (display "(")
   (this.lhs.traverse #f (indent+ indent) #t)
   (display " in ")
   (this.obj.traverse #f (indent+ indent) #f)
   (display ")")
   (block-body this.body indent #f #t))

(define-pmethod (Continue-out priority indent in-for-in?)
   (indent! indent)
   (display "continue")
   (when this.id (display* " " this.id))
   (display ";")
   (newline-out))

(define-pmethod (Break-out priority indent in-for-in?)
   (indent! indent)
   (display "break")
   (when this.id (display* " " this.id))
   (display ";")
   (newline-out))

(define-pmethod (Return-out priority indent in-for-in?)
   (indent! indent)
   (display "return ")
   (this.expr.traverse #f (indent+ indent) #f)
   (display ";")
   (newline-out))

(define-pmethod (With-out priority indent in-for-in?)
   (indent! indent)
   (display "with")
   (space-out)
   (display "(")
   (this.obj.traverse #f (indent+ indent) #f)
   (display ")")
   (block-body this.body indent #f #t))

(define-pmethod (Switch-out priority indent in-for-in?)
   (indent! indent)
   (display "switch")
   (space-out)
   (display "(")
   (this.key.traverse #f (indent+ indent) #f)
   (display ")")
   (space-out)
   (display "{")
   (newline-out)
   (for-each (lambda (n)
		(n.traverse #f indent #f))
	     this.cases)
   (indent! indent)
   (display "}")
   (newline-out))

(define-pmethod (Case-out priority indent in-for-in?)
   (indent! indent)
   (display "case ")
   (this.expr.traverse #f (indent+ indent) #f)
   (display ":")
   (newline-out)
   (Block-out-without-braces this.body #f (indent+ indent) #f))

(define-pmethod (Default-out priority indent in-for-in?)
   (indent! indent)
   (display "default:")
   (newline-out)
   (Block-out-without-braces this.body #f (indent+ indent) #f))

(define-pmethod (Throw-out priority indent in-for-in?)
   (indent! indent)
   (display "throw ")
   (this.expr.traverse #f (indent+ indent) #f)
   (display ";")
   (newline-out))

(define-pmethod (Try-out priority indent in-for-in?)
   (indent! indent)
   (display "try")
   (let ((block? (block-body this.body indent #t #f)))
      (if block?
	  (space-out)
	  (indent! indent))
      (if this.catch
	  (this.catch.traverse #f indent #f))
      (if this.finally
	  (begin
	     (space-out)
	     (display "finally")
	     (block-body (stmt->block this.finally) indent #t #t))
	  (newline-out))))

(define-pmethod (Catch-out priority indent in-for-in?)
   (space-out)
   (display "catch(")
   (this.decl.traverse #f indent #f)
   (display ")")
   (block-body (stmt->block this.body) indent #f #t))

(define-pmethod (Labelled-out priority indent in-for-in?)
   (indent! indent)
   (display* this.id ":")
   (block-body this.body indent #f #t))

(define (parameter-display params indent)
   (display "(")
   (if (null? params)
       'do-nothing
       (begin
	  ((car params).traverse 1 indent #f)
	  (for-each (lambda (param)
		       (display ",")
		       (space-out)
		       (param.traverse 1 indent #f))
		    (cdr params))))
   (display ")"))

(define (function-out fun name indent)
   (define (do-function-out)
      (display "function")
      (if name
	  (begin
	     (display " ")
	     (name.traverse #f indent #f)))
      (parameter-display fun.params indent)
      (block-body (stmt->block fun.body)
		  indent #f #f))

   (cond
      ((and *generate-function-strings?*
	    (not *inside-function?*))
       ;; top-level function
       (let ((str-port (open-output-string))
	     (fun-str-id (gensym 'fun-str)))
	  (set! *function-str-id* fun-str-id)
	  (set! *function-out-str-port* str-port)
	  (set! *inside-function?* #t)
	  (with-output-to-port str-port do-function-out)
	  (let ((str (close-output-port str-port)))
	     (hashtable-put! *function-strings-ht*
			     fun-str-id
			     str)
	     (set! fun.str (list fun-str-id 0 (string-length str)))
	     (set! *inside-function?* #f))))
      (*generate-function-strings?*
       (let ((start-pos (string-length
			 (flush-output-port *function-out-str-port*))))
	  (do-function-out)
	  (let ((end-pos (string-length
			  (flush-output-port *function-out-str-port*))))
	     (set! fun.str (list *function-str-id* start-pos end-pos)))))
      (else
       (do-function-out))))

(define-pmethod (Fun-binding-out priority indent in-for-in?)
   (indent! indent)
   (function-out this.val this.lhs indent)
   (newline-out))

(define-pmethod (Named-fun-out priority indent in-for-in?)
   (check-expr
    (function-out this.fun this.decl indent)))

(define-pmethod (Fun-out priority indent in-for-in?)
   (check-expr
    (function-out this #f indent)))

(define-pmethod (Vassig-out priority indent in-for-in?)
   (check-expr
    (this.lhs.traverse this-priority indent in-for-in?)
    (space-out)
    (display "=")
    (space-out)
    (this.val.traverse this-priority indent in-for-in?)))

(define-pmethod (Vassig-op-out priority indent in-for-in?)
   (check-expr
    (this.lhs.traverse this-priority indent in-for-in?)
    (space-out)
    (display* (op->string this.op.id) "=")
    (space-out)
    (this.val.traverse this-priority indent in-for-in?)))

(define-pmethod (Accsig-out priority indent in-for-in?)
   (check-expr
    (this.lhs.traverse this-priority indent in-for-in?)
    (space-out)
    (display "=")
    (space-out)
    (this.val.traverse this-priority indent in-for-in?)))

(define-pmethod (Accsig-op-out priority indent in-for-in?)
   (check-expr
    (this.lhs.traverse this-priority indent in-for-in?)
    (space-out)
    (display* (op->string this.op.id) "=")
    (space-out)
    (this.val.traverse this-priority indent in-for-in?)))

(define-pmethod (Cond-out priority indent in-for-in?)
   (check-expr
    (this.test.traverse this-priority indent in-for-in?)
    (display "?")
    (space-out)
    (this.then.traverse this-priority indent #f)
    (display ":")
    (space-out)
    (this.else.traverse this-priority indent #t)))

(define-pmethod (Call-out priority indent in-for-in?)
   (check-expr
    (this.op.traverse this-priority indent in-for-in?)
    (display "(")
    (unless (null? this.args)
       ((car this.args).traverse 1 indent #f)
       (for-each (lambda (arg)
		    (display ",")
		    (space-out)
		    (arg.traverse 1 indent #f))
		 (cdr this.args)))
    (display ")")))

(define *max-priority* 100)

(define-pmethod (Binary-out priority indent in-for-in?)
   ;; force parenthesis if we are in a for-in
   (let ((priority (if (and in-for-in?
			    (eq? this.op.id 'in))
		       *max-priority*
		       priority)))
      (check-expr
       ((car this.args).traverse this-priority indent in-for-in?)
       (case this.op.id
	  ((in instanceof) (display " ") (display this.op.id) (display " "))
	  (else (space-out) (display (op->string this.op.id)) (space-out)))
       (let ((rhs-priority (case this.op.id
			      ((OR ;; x||(y||z) <=> (x||y)||z
				&& ;; x&&(y&&z) <=> (x&&y)&&z
				BIT_OR ;; x|(y|z) <=> (x|y)|z
				& ;; x&(y&z) <=> (x&y)&z
				* ;; x*(y*z) <=> (x*y)*z
				) this-priority)
				;; true==(5==5) <!=> (true==5)==5
			      ((== != === !== ;; a==(b==c) <!=> (a==b)==c
				   ^ ;; x^(y^z) <!=> (x^y)^z
				   < > <= >= instanceof in
				   << >> >>>
				   - ;; x-(y-z) <!=> (x-y)-z
				   ;; x+(y+z) <!=> (x+y)+z
				   ;; ex:  "t"+(1+3) -> "t4"
				   ;;  but ("t"+1)+3 -> "t13"
				   +
				   / %)
			       (+ this-priority 1))
			      (else
			       (error "rhs-priority"
				      "missed binary operator"
				      this.op.id)))))
	  ((cadr this.args).traverse rhs-priority
			   indent in-for-in?)))))
   
(define-pmethod (Unary-out priority indent in-for-in?)
   (check-expr
    (display this.op.id)
    (case this.op.id
       ((delete void typeof) (display " "))
       (else 'do-nothing))
    ((car this.args).traverse this-priority indent in-for-in?)))

(define-pmethod (Postfix-out priority indent in-for-in?)
   (check-expr
    ((car this.args).traverse this-priority indent in-for-in?)
    (display this.op.id)))

(define-pmethod (New-out priority indent in-for-in?)
   (check-expr
    (display "new ")
    (this.class.traverse this-priority indent in-for-in?)
    (display "(")
    (unless (null? this.args)
       ((car this.args).traverse 1 indent #f)
       (for-each (lambda (arg)
		    (display ",")
		    (space-out)
		    (arg.traverse 1 indent #f))
		 (cdr this.args)))
    (display ")")))

(define-pmethod (Access-out priority indent in-for-in?)
   (check-expr
    (this.obj.traverse this-priority indent in-for-in?)
    (display "[")
    (this.field.traverse this-priority indent #f)
    (display "]")))

(define-pmethod (Dot-out priority indent in-for-in?)
   (check-expr
    (this.obj.traverse this-priority indent in-for-in?)
    (display ".")
    (display this.field.val)))

(define-pmethod (This-out priority indent in-for-in?)
   (check-expr
    (display "this")))

(define-pmethod (Literal-out priority indent in-for-in?)
   (error "Literal-out" "forgot literal type" this.val))

(define-pmethod (Undefined-out priority indent in-for-in?)
   (check-expr
    (display "undefined")))

(define-pmethod (Null-out priority indent in-for-in?)
   (check-expr
    (display "null")))

(define-pmethod (Bool-out priority indent in-for-in?)
   (check-expr
    (if this.val
	(display "true")
	(display "false"))))

(define-pmethod (Number-out priority indent in-for-in?)
   (check-expr
    (display this.val)))

(define-pmethod (String-out priority indent in-for-in?)
   ;; TODO: escape string
   (check-expr
    (display* this.val))) ;"\"" (string-for-read this.val) "\"")))

(define-pmethod (Array-out priority indent in-for-in?)
   (check-expr
    (display "[")
    (let loop ((i 0)
	       (els this.els))
       (unless (>= i this.length)
	  (if (not (= i 0))
	      (display ", "))
	  (if (or (null? els)
		  (not (= (car els).index i)))
	      (loop (+ i 1)
		    els)
	      (begin
		 ((car els).expr.traverse 1 indent in-for-in?)
		 (loop (+ i 1)
		       (cdr els))))))
    (display "]")))

(define-pmethod (Obj-init-out priority indent in-for-in?)
   (check-expr
    (display "{")
    (for-each (lambda (prop)
		 (display* "\"" prop.name.val "\":")
		 (space-out)
		 (prop.val.traverse #f indent #f))
	      this.props)
    (display "}")))

(define-pmethod (Reg-exp-out priority indent in-for-in?)
   (check-expr
    (display this.pattern)))


;; ========================================================================
;; main procedure starts here
;; ========================================================================
(when (or (not (null? Lp))
	  (config 'function-strings))
   (verbose "js-out")
   (let ((p (if (null? Lp)
		;; (Funs will replace the port)
		(open-output-procedure (lambda (str) 'ignore))
		(car Lp))))
      (set! *generate-function-strings?* (null? Lp))
      (assign-priorities!)
      (overload traverse out (Node
			      Program
			      Block
			      Sequence
			      Var-decl-list
			      Var-ref
			      NOP
			      If
			      For
			      While
			      Do
			      For-in
			      Continue
			      Break
			      Return
			      With
			      Switch
			      Case
			      Default
			      Throw
			      Try
			      Catch
			      Labelled
			      Fun-binding
			      Named-fun
			      Fun
			      Vassig
			      Vassig-op
			      Accsig
			      Accsig-op
			      Cond
			      Call
			      Binary
			      Unary
			      Postfix
			      New
			      Access
			      Dot
			      This
			      Literal
			      Undefined
			      Null
			      Bool
			      Number
			      String
			      Array
			      Obj-init
			      Reg-exp)
		;; Var is not always assigned yet.
		;; if it isn't then just the Var-ref.id is used (see
		;; Var-ref-out)
		(overload out out (Var)
			  (with-output-to-port p
			     (lambda ()
				(tree.traverse #f 0 #f))))))))
