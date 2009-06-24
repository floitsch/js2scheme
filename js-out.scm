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


(define (inherits-from-any-of? n . L)
   (any? (lambda (c) (inherits-from? n (node c))) L))

(define (primary-expr? n)
   (inherits-from-any-of? n 'This 'Var-ref 'Literal 'Reg-exp 'Array 'Obj-init))

(define (member-expr? n)
   (or (primary-expr? n)
       (inherits-from-any-of? n 'Fun 'Named-fun 'Access 'Dot 'New)))

;; we merge NewExpressions and MemberExpressions
;; we will (for now) never print 'new's without parenthesis.
;;           eg: 'new X;' becomes 'new X();'
;; this simplifies the requirements.

(define (lhs-expr? n)
   (or (member-expr? n)
       (inherits-from? n (node 'Call))))

(define (unary-expr? n)
   (or (lhs-expr? n)
       (inherits-from-any-of? n 'Postfix 'Unary)))

(define (binary-expr? n . Lkinds)
   (and (inherits-from? n (node 'Binary))
	(let ((op n.op.id))
	   (any? (lambda (k) (eq? op k)) Lkinds))))

(define (*-or-unary-expr? n)
   (or (unary-expr? n)
       (binary-expr? n '*)))

(define (mult-expr? n)
   (or (unary-expr? n)
       (binary-expr? n '* '/ '%)))

(define (add-expr? n)
   (or (mult-expr? n)
       (binary-expr? n '+ '-)))

(define (shift-expr? n)
   (or (add-expr? n)
       (binary-expr? n '<< '>> '>>>)))

(define (rel-expr? n)
   (or (shift-expr? n)
       (binary-expr? n '< '> '<= '>= 'instanceof 'in)))

(define (eq-expr? n)
   (or (rel-expr? n)
       (binary-expr? n '== '!= '=== '!==)))

(define (bit-and-expr? n)
   (or (eq-expr? n)
       (binary-expr? n '&)))

(define (bit-xor-expr? n)
   (or (bit-and-expr? n)
       (binary-expr? n '^)))

(define (bit-or-expr? n)
   (or (bit-xor-expr? n)
       (binary-expr? n 'BIT_OR)))

(define (and-expr? n)
   (or (bit-or-expr? n)
       (binary-expr? n '&&)))

(define (or-expr? n)
   (or (and-expr? n)
       (binary-expr? n 'OR)))

(define (assig-expr? n)
   (or (or-expr? n)
       (inherits-from-any-of? n 'Cond 'Assig)))

(define (expr? n)
   (cond-expand
      (bigloo-debug (unless (or (assig-expr? n)
				(inherits-from? n (node 'Sequence)))
		       (error "expr?"
			      "forgot an expression"
			      (pobject-name n)))
		    #t)
      (else #t)))

(define (no-requirement n)
   #t)

(define (indent+ indent)
   (+ indent 3))

(define (indent! indent)
   (unless (config 'compress?)
      (let loop ((i 0))
	 (when (<fx i indent)
	    (display #\space)
	    (loop (+fx i 1))))))

(define (newline-out)
   (unless (config 'compress?)
      (display "\n")))

(define (space-out)
   (unless (config 'compress?)
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
	     (body.stmt-out indent
			    #f ;; don't indent
			    newline-after?)
	     #t) ;; yes. it was a Block.
	  (begin
	     (if (and (config 'compress?)
		      needs-separation?)
		 (display " ")
		 (newline-out)) ;; compress?==#t -> newline-out does nothing
	     (body.stmt-out (indent+ indent))
	     #f)))) ;; no. not a block.

(define (program-out this)
   (Block-out-without-braces this.body 0)
   (when *generate-function-strings?*
      (set! this.function-str-ids-ht *function-strings-ht*)))

(define (stmt-out n indent)
   (n.stmt-out indent))

(define-pmethod (Node-stmt-out indent)
   ;; apparently an expression.
   (indent! indent)
   (expr-out this indent expr? #f #t)
   (display ";")
   (newline-out))

(define (Block-out-without-braces stmt indent)
   (if (inherits-from? stmt (node 'Block))
       (for-each (lambda (n)
		    (stmt-out n indent))
		 stmt.els)
       (stmt-out stmt indent)))

(define (stmt->block stmt)
   (cond
      ((inherits-from? stmt (node 'Block))
       stmt)
      ((inherits-from? stmt (node 'NOP))
       (new-node Block '()))
      (else
       (new-node Block (list stmt)))))

(define-pmethod (Block-stmt-out indent
				#!optional (indent? #t) (new-line-after? #t))
   (if indent? (indent! indent))
   (display "{")
   (newline-out)
   (for-each (lambda (n)
		(stmt-out n (indent+ indent)))
	     this.els)
   (indent! indent)
   (display "}")
   (if new-line-after? (newline-out)))

(define (Var-decl-list-out this indent in-for-init?)
   (display "var ")
    (let ((els this.els))
       (expr-out (car els) indent assig-expr? in-for-init? #f)
       (for-each (lambda (n)
		    (display ",")
		    (space-out)
		    (expr-out n indent assig-expr? in-for-init? #f))
		 (cdr els))))

(define-pmethod (Var-decl-list-stmt-out indent)
   (indent! indent)
   (Var-decl-list-out this indent #f)
   (display ";")
   (newline))

(define-pmethod (NOP-stmt-out indent)
   (indent! indent)
   (display ";")
   (newline-out))

(define-pmethod (If-stmt-out indent #!optional (indent? #t))
   (if (and (inherits-from? this.then (node 'If)) ;; nested if
	    (inherits-from? this.then.else (node 'NOP)) ;; has no else-branch
	    (not (inherits-from? this.else (node 'NOP)))) ;; but we have one
       (set! this.then (new-node Block (list this.then)))) ;; protect our else
   (when indent? (indent! indent))
   (display "if")
   (space-out)
   (display "(")
   (expr-out this.test indent expr? #f #f)
   (display ")")
   (let* ((else-branch? (not (inherits-from? this.else (node 'NOP))))
	  (then-block? (block-body this.then indent #f (not else-branch?))))
      (when else-branch?
	 (if then-block?
	     (space-out)
	     (indent! indent))
	 (display "else")
	 (if (inherits-from? this.else (node 'If))
	     (begin
		(display #\space)
		;; TODO: this shortcuts the stmt-out procedure.
		(this.else.stmt-out indent #f))
	     (block-body this.else indent #t #t)))))

(define-pmethod (For-stmt-out indent)
   (indent! indent)
   (display "for")
   (space-out)
   (display "(")
   ;; no requirement, as init could be a var-init too.
   (when this.init (expr-out this.init (indent+ indent)
			     no-requirement #t #f))
   (display ";")
   (when this.test
      (space-out)
      (expr-out this.test (indent+ indent) expr? #f #f))
   (display ";")
   (when this.incr
      (space-out)
      (expr-out this.incr (indent+ indent) expr? #f #f))
   (display ")")
   (block-body this.body indent #f #t))

(define-pmethod (While-stmt-out indent)
   (indent! indent)
   (display "while")
   (space-out)
   (display "(")
   (expr-out this.test (indent+ indent) expr? #f #f)
   (display ")")
   (block-body this.body indent #f #t))

(define-pmethod (Do-stmt-out indent)
   (indent! indent)
   (display "do")
   (if (block-body this.body indent #t #f)
       (space-out)
       (indent! indent))
   (display "while")
   (space-out)
   (display "(")
   (expr-out this.test (indent+ indent) expr? #f #f)
   (display ");")
   (newline-out))

(define-pmethod (For-in-stmt-out indent)
   (indent! indent)
   (display "for")
   (space-out)
   (display "(")
   ;; no-requirement as lhs could be a var-decl-list too.
   ;; note that we set 'in-for-in?' flag to true.
   (expr-out this.lhs (indent+ indent) no-requirement #t #f)
   (display " in ")
   (expr-out this.obj (indent+ indent) expr? #f #f)
   (display ")")
   (block-body this.body indent #f #t))

(define-pmethod (Continue-stmt-out indent)
   (indent! indent)
   (display "continue")
   (when this.id (display* " " this.id))
   (display ";")
   (newline-out))

(define-pmethod (Break-stmt-out indent)
   (indent! indent)
   (display "break")
   (when this.id (display* " " this.id))
   (display ";")
   (newline-out))

(define-pmethod (Return-stmt-out indent)
   (indent! indent)
   (display "return ")
   (expr-out this.expr (indent+ indent) expr? #f #f)
   (display ";")
   (newline-out))

(define-pmethod (With-stmt-out indent)
   (indent! indent)
   (display "with")
   (space-out)
   (display "(")
   (expr-out this.obj (indent+ indent) expr? #f #f)
   (display ")")
   (block-body this.body indent #f #t))

(define-pmethod (Switch-stmt-out indent)
   (indent! indent)
   (display "switch")
   (space-out)
   (display "(")
   (expr-out this.key (indent+ indent) expr? #f #f)
   (display ")")
   (space-out)
   (display "{")
   (newline-out)
   (for-each (lambda (n)
		(stmt-out n indent))
	     this.cases)
   (indent! indent)
   (display "}")
   (newline-out))

;; we simply use stmt-out. Could use something else, though.
(define-pmethod (Case-stmt-out indent)
   (indent! indent)
   (display "case ")
   (expr-out this.expr (indent+ indent) expr? #f #f)
   (display ":")
   (newline-out)
   (Block-out-without-braces this.body (indent+ indent)))

;; same as for Case-stmt-out. Name is not really well chosen.
(define-pmethod (Default-stmt-out indent)
   (indent! indent)
   (display "default:")
   (newline-out)
   (Block-out-without-braces this.body (indent+ indent)))

(define-pmethod (Throw-stmt-out indent)
   (indent! indent)
   (display "throw ")
   (expr-out this.expr (indent+ indent) expr? #f #f)
   (display ";")
   (newline-out))

(define-pmethod (Try-stmt-out indent)
   (indent! indent)
   (display "try")
   (let ((block? (block-body this.body indent #t #f)))
      (if block?
	  (space-out)
	  (indent! indent))
      (when this.catch
	 (catch-out this.catch indent))
      (if this.finally
	  (begin
	     (space-out)
	     (display "finally")
	     (block-body (stmt->block this.finally) indent #t #t))
	  (newline-out))))

(define (catch-out this indent)
   (space-out)
   (display "catch(")
   ;; must be a ref anyways. -> just test for primary-expr
   (expr-out this.decl indent primary-expr? #f #f)
   (display ")")
   (block-body (stmt->block this.body) indent #f #t))

(define-pmethod (Labelled-stmt-out indent)
   (indent! indent)
   (display* this.id ":")
   (block-body this.body indent #f #t))

(define (function-out fun name indent)
   (define (do-function-out)
      (display "function")
      (when name
	 (display #\space)
	 ;; name must be decl. -> just test for primary-expr
	 (expr-out name indent primary-expr? #f #f))
      (display "(")
      (let ((params fun.params))
	 (unless (null? params)
	    ;; params must be refs. -> primary-expr?
	    (expr-out (car params) indent primary-expr? #f #f)
	    (for-each (lambda (param)
			 (display ",")
			 (space-out)
			 (expr-out param indent primary-expr? #f #f))
		      (cdr params))))
      (display ")")
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

(define-pmethod (Fun-binding-stmt-out indent)
   (indent! indent)
   (function-out this.val this.lhs indent)
   (newline-out))


(define (expr-out n indent required-pred in-for-init? stmt-begin?)
   (let* ((needs-parentheses? (or
			       ;; something like "a-(b+c)"
			       (not (required-pred n))
			       ;; for (a = (x in y); ... ; ... )
			       (and in-for-init?
				    (inherits-from? n (node 'Binary))
				    (eq? n.op.id 'in))
			       ;; (function(){...})();
			       ;; ({a: 2, b:3}.print());
			       (and stmt-begin?
				    (or (inherits-from? n (node 'Named-fun))
					(inherits-from? n (node 'Fun))
					(inherits-from? n (node 'Obj-init))))))
	  (new-in-for-init? (and in-for-init?
				 (not needs-parentheses?)))
	  (new-stmt-begin? (and stmt-begin?
				(not needs-parentheses?))))
      (if needs-parentheses?
	  (begin
	     (display "(")
	     (n.expr-out indent new-in-for-init? new-stmt-begin?)
	     (display ")"))
	  (n.expr-out indent new-in-for-init? new-stmt-begin?))))

;; indent is only for statements (-> functions).
(define-pmethod (Node-expr-out indent in-for-init? stmt-begin?)
   (error "Node-out"
	  "forgot node type: "
	  (pobject-name this)))

(define (var-out this)
   (display (or this.generated
		this.id)))


(define-pmethod (Named-fun-expr-out indent in-for-init? stmt-begin?)
   (function-out this.body this.decl indent))

(define-pmethod (Fun-expr-out indent in-for-init? stmt-begin?)
   ;; note that expr-out already puts parenthesis around the fun if necessary.
   (function-out this #f indent))

(define-pmethod (Sequence-expr-out indent in-for-init? stmt-begin?)
   ;; we require expr?s as elements.
   ;; -> a,(b,c) and (a,b),c will become a,b,c
   (let ((els this.els))
      (expr-out (car els) indent expr? in-for-init? stmt-begin?)
      (for-each (lambda (n)
		   (display ",")
		   (space-out)
		   (expr-out n indent expr? in-for-init? #f))
		(cdr els))))

;; handles Vassig and Accsig
(define-pmethod (Assig-expr-out indent in-for-init? stmt-begin?)
   (expr-out this.lhs indent lhs-expr? in-for-init? stmt-begin?)
   (space-out)
   (display "=")
   (space-out)
   (expr-out this.val indent assig-expr? in-for-init? #f))

;; handles Vassig-op and Accsig-op
(define (Assig-op-expr-out this indent in-for-init? stmt-begin?)
   (expr-out this.lhs indent lhs-expr? in-for-init? stmt-begin?)
   (space-out)
   (display* (op->string this.op.id) "=")
   (space-out)
   (expr-out this.val indent assig-expr? in-for-init? #f))
   
(define-pmethod (Vassig-op-expr-out indent in-for-init? stmt-begin?)
   (Assig-op-expr-out this indent in-for-init? stmt-begin?))

(define-pmethod (Accsig-op-expr-out indent in-for-init? stmt-begin?)
   (Assig-op-expr-out this indent in-for-init? stmt-begin?))

(define-pmethod (Cond-expr-out indent in-for-init? stmt-begin?)
   (expr-out this.test indent or-expr? in-for-init? stmt-begin?)
   (display "?")
   (space-out)
   ;; the then part is allowed to have an 'in'.
   (expr-out this.then indent assig-expr? #f #f)
   (display ":")
   (space-out)
   (expr-out this.else indent assig-expr? in-for-init? #f))

(define-pmethod (Binary-expr-out indent in-for-init? stmt-begin?)
   (let ((left-req (case this.op.id
		      ((OR) or-expr?)
		      ((&&) and-expr?)
		      ((BIT_OR) bit-or-expr?)
		      ((^) bit-xor-expr?)
		      ((&) bit-and-expr?)
		      ((== != === !==) eq-expr?)
		      ((< > <= >= instanceof in) rel-expr?)
		      ((>> << >>>) shift-expr?)
		      ((+ -) add-expr?)
		      ((* / %) mult-expr?)
		      (else (error 'js-out
				   "forgot op"
				   this.op.id))))
	 (right-req (case this.op.id
		       ((OR) ;; x||(y||z) <=> (x||y)||z
			or-expr?)
		       ((&&) ;; x&&(y&&z) <=> (x&&y)&&z
			and-expr?)
		       ((BIT_OR) ;; x|(y|z) <=> (x|y)|z
			bit-or-expr?) ;; x^(y^z) <=> (x^y)^z
		       ((^)
			bit-xor-expr?)
		       ((&) ;; x&(y&z) <=> (x&y)&z
			bit-and-expr?)
		       ((== != === !==) rel-expr?)
		       ((< > <= >= instanceof in) shift-expr?)
		       ((>> << >>>) add-expr?)
		       ;; x+(y+z) <!=> (x+y)+z
		       ;; ex: "a"+(1+2) => "a3"
		       ;;     ("a"+1)+2 => "a12"
		       ((+ -) mult-expr?)
		       ;; the following 2 lines are only true when we ignore
		       ;; precision...
		       ;((*) ;; x*(y*z) <=> (x*y)*z
			;*-or-mult-expr?)
		       ;; use safe unary-expr? instead.
		       ((*) unary-expr?)
		       ((/ %) unary-expr?)
		       (else (error 'js-out
				    "forgot op"
				    this.op.id)))))
      (expr-out (car this.args) indent left-req in-for-init? stmt-begin?)
      (case this.op.id
	 ;; in theory one could avoid the spaces. at least sometimes.
	 ((in instanceof) (display " ") (display this.op.id) (display " "))
	 ((OR) (space-out) (display "||") (space-out))
	 ((BIT_OR) (space-out) (display "|") (space-out))
	 (else (space-out) (display this.op.id) (space-out)))
      (expr-out (cadr this.args) indent right-req in-for-init? #f)))

(define-pmethod (Unary-expr-out indent in-for-init? stmt-begin?)
   (case this.op.id
      ((unary-+) (display '+))
      ((unary--) (display '-))
      (else (display this.op.id)))
   (case this.op.id
      ((delete void typeof) (display " "))
      (else 'do-nothing))
   (expr-out (car this.args) indent unary-expr? in-for-init? #f))

(define-pmethod (Postfix-expr-out indent in-for-init? stmt-begin?)
    (expr-out (car this.args) indent lhs-expr? in-for-init? stmt-begin?)
    (display this.op.id))

(define-pmethod (Call-expr-out indent in-for-init? stmt-begin?)
   ;; we always print parenthesis for 'new' expressions.
   ;; as a consequence our requirements are easier.
   (expr-out this.op indent lhs-expr? in-for-init? stmt-begin?)
   (display "(")
   (unless (null? this.args)
      (expr-out (car this.args) indent assig-expr? #f #f)
      (for-each (lambda (arg)
		   (display ",")
		   (space-out)
		   (expr-out arg indent assig-expr? #f #f))
		(cdr this.args)))
   (display ")"))

(define-pmethod (New-expr-out indent in-for-init? stmt-begin?)
   ;; same as for Call. As we always add the parenthesis we have simpler
   ;; requirements.
   (display "new ")
   (expr-out this.class indent member-expr? in-for-init? #f)
   (display "(")
   (unless (null? this.args)
      (expr-out (car this.args) indent assig-expr? #f #f)
      (for-each (lambda (arg)
		   (display ",")
		   (space-out)
		   (expr-out arg indent assig-expr? #f #f))
		(cdr this.args)))
   (display ")"))

(define-pmethod (Access-expr-out indent in-for-init? stmt-begin?)
   (expr-out this.obj indent member-expr? in-for-init? stmt-begin?)
   (display "[")
   (expr-out this.field indent expr? #f #f)
   (display "]"))

(define-pmethod (Dot-expr-out indent in-for-init? stmt-begin?)
   (expr-out this.obj indent member-expr? in-for-init? stmt-begin?)
   (display ".")
   (display this.field.val))

(define-pmethod (This-expr-out indent in-for-init? stmt-begin?)
   (display "this"))

(define-pmethod (Literal-expr-out indent in-for-init? stmt-begin?)
   (error "Literal-out" "forgot literal type" this.val))

(define-pmethod (Undefined-expr-out indent in-for-init? stmt-begin?)
   ;; not entirely correct, as 'undefined' can be overwritten.
   ;; but there is no better way to get a fast 'undefined'.
   ;; at least I think so.
   (display "undefined"))

(define-pmethod (Null-expr-out indent in-for-init? stmt-begin?)
   (display "null"))

(define-pmethod (Bool-expr-out indent in-for-init? stmt-begin?)
   (if this.val
       (display "true")
       (display "false")))

(define-pmethod (Number-expr-out indent in-for-init? stmt-begin?)
   (display this.val))

(define-pmethod (String-expr-out indent in-for-init? stmt-begin?)
   ;; var is an already escaped string, including the delimiters.
   (display this.val))

(define-pmethod (Var-ref-expr-out indent in-for-init? stmt-begin?)
   (if this.var
       (var-out this.var)
       (display this.id)))

(define-pmethod (Array-expr-out indent in-for-init? stmt-begin?)
   ;; basically: we may avoid a "," at the last pos, if the was an el.
   ;; so [1] and [1,] are the same, but [,] and [] are not, as there is
   ;; no el.
   (display "[")
   (let loop ((i 0)
	      (els this.els))
      (cond
	 ((>=fx i this.length) 'done)
	 ((or (null? els)
	      (not (= (car els).index i)))
	  ;; empty el. just print the ',' which is always needed.
	  (display ",")
	  (loop (+fx i 1) els))
	 (else
	  (unless (=fx i 0) (space-out))
	  (expr-out (car els).expr indent assig-expr? #f #f)
	  (unless (=fx (+fx i 1) this.length)
	     ;; this is the optim. last element does not need a ','
	     (display ","))
	  (loop (+ i 1)	(cdr els)))))
    (display "]"))

(define-pmethod (Obj-init-expr-out indent in-for-init? stmt-begin?)
   (define (id-chars? str)
      ;; skip first and last '"'
      (let ((stop (-fx (string-length str) 1)))
	 (let loop ((i 1))
	    (cond
	       ((>=fx i stop) #t)
	       ((char-alphabetic? (string-ref str i))
		(loop (+fx i 1)))
	       ((and (>fx i 1)
		     (char-numeric? (string-ref str i)))
		(loop (+fx i 1)))
	       (else #f)))))

   (display "{")
   (let loop ((props this.props)
	      (first? #t))
      (unless (null? props)
	 (let ((prop (car props)))
	    (unless first?
	       (display ",")
	       (space-out))
	    (cond
	       ((inherits-from? prop.name (node 'Number))
		(display prop.name.val))
	       ;; String
	       ((id-chars? prop.name.val)
		(let ((str prop.name.val))
		   (display (substring str 1 (-fx (string-length str) 1)))))
	       (else
		(display prop.name.val)))
	    (display ":")
	    (space-out)
	    (expr-out prop.val indent assig-expr? #f #f)
	    (loop (cdr props) #f))))
   (display "}"))

(define-pmethod (Reg-exp-expr-out indent in-for-init? stmt-begin?)
   (display this.pattern))

(define-pmethod (Var-decl-list-expr-out indent in-for-init? stmt-begin?)
   (Var-decl-list-out this indent in-for-init?))

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
      (overload stmt-out stmt-out (Node
				   Block
				   Var-decl-list
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
				   Labelled
				   Fun-binding)
		(overload expr-out expr-out (Node
					     Named-fun
					     Fun
					     Sequence
					     Assig
					     Vassig-op
					     Accsig-op
					     Cond
					     Binary
					     Unary
					     Postfix
					     Call
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
					     Var-ref
					     Array
					     Obj-init
					     Reg-exp
					     Var-decl-list)
			  (with-output-to-port p
			     (lambda ()
				(program-out tree))))))))
