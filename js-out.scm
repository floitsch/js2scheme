(module js-out
   (import lexer
	   parser
	   config
	   nodes
	   verbose)
   (static (class Env
	      generate-function-strings?::bool
	      function-strings-ht
	      compress?::bool

	      (function-out-str-port (default #unspecified))
	      (inside-function?::bool (default #f))
	      (function-str-id::symbol (default '?))
	      ))
   (export (js-out tree::Node . Lp))
   )

;; this module has two purposes:
;; - pretty print of JavaScript
;; - get the function-strings. A function's toString has to return a textual
;; representation of the function. This module generates this string.
;;
;; without port, js-out simply assigns a str-property to Funs.
;; otherwise the whole program is printed into the given port.

(define (js-out tree . Lp)
   (when (or (not (null? Lp))
	     (config 'function-strings))
      (verbose "js-out")
      (let ((p (if (null? Lp)
		   ;; (Funs will replace the port)
		   (open-output-procedure (lambda (str) 'ignore))
		   (car Lp)))
	    (env (instantiate::Env
		    (generate-function-strings? (null? Lp))
		    (function-strings-ht (make-hashtable))
		    (compress? (config 'compress?)))))
	 (with-output-to-port p
	    (lambda ()
	       (program-out tree env))))))

(define (primary-expr? n)
   (or (This? n)
       (Ref? n)
       (Literal? n)
       (Reg-Exp? n)
       (Array? n)
       (Obj-Init? n)))

(define (call-expr? n)
   (or (primary-expr? n)
       (and (not (Binary? n))
	    (or (Fun? n)
		(Named-Fun? n)
		(Access? n)
		(New? n)
		(Call? n)))))

;; we merge NewExpressions, CallExpressions and MemberExpressions
;; we will (for now) never print 'new's without parenthesis.
;;           eg: 'new X;' becomes 'new X();'
;; this simplifies the requirements.

(define (lhs-expr? n) (call-expr? n))

(define (unary-expr? n)
   (or (lhs-expr? n)
       (Postfix? n)
       (Unary? n)))

(define (binary-expr? n . Lkinds)
   (and (Binary? n)
	(with-access::Binary n (op)
	   (with-access::Ref op (id)
	      (any? (lambda (k) (eq? id k)) Lkinds)))))

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
       (Cond? n)
       (Assig? n)))

(define (expr? n)
   (cond-expand
      (bigloo-debug (unless (or (assig-expr? n)
				(Sequence? n))
		       (error "expr?"
			      "forgot an expression"
			      (class-name (object-class n))))
		    #t)
      (else #t)))

(define (no-requirement n)
   #t)

(define (indent+ indent)
   (+ indent 3))

(define (indent! env indent)
   (with-access::Env env (compress?)
      (unless compress?
	 (let loop ((i 0))
	    (when (<fx i indent)
	       (display #\space)
	       (loop (+fx i 1)))))))

(define (newline-out env)
   (with-access::Env env (compress?)
      (unless compress?
	 (display "\n"))))

(define (space-out env)
   (with-access::Env env (compress?)
      (unless compress?
	 (display " "))))

(define (op->string op)
   (case op
      ((OR) "||")
      ((BIT_OR) "|")
      (else (symbol->string op))))

(define (block-body body env::Env indent needs-separation? newline-after?)
   (if (Block? body)
       (begin
	  (space-out env)
	  (block-stmt-out body
			  env
			  indent
			  #f ;; don't indent
			  newline-after?)
	  #t) ;; yes. it was a Block.
       (with-access::Env env (compress?)
	  (if (and compress?
		   needs-separation?)
	      (display " ")  ;; with compress the newline-out does nothing.
	      (newline-out env))
	  (stmt-out body env (indent+ indent))
	  #f))) ;; no. not a block.

(define (program-out this::Program env::Env)
   (with-access::Program this (body function-str-ids-ht)
      (Block-out-without-braces body env 0)
      (with-access::Env env (generate-function-strings? function-strings-ht)
	 (when generate-function-strings?
	    (set! function-str-ids-ht function-strings-ht)))))

(define-generic (stmt-out this::Node env::Env indent::int)
   ;; if we are here the node is probably just an expression.
   (indent! env indent)
   (nested-expr-out this env indent expr? #f #t)
   (display ";")
   (newline-out env))

(define-generic (expr-out this::Node env::Env ident::int
			  in-for-init?::bool stmt-begin?::bool)
   (error "expr-out"
	  "Internal error: forgot node-type"
	  (class-name (object-class this))))

(define (nested-expr-out this::Node env::Env indent of-required-type?::procedure
			 in-for-init? stmt-begin?)
   (let* ((needs-parentheses? (or
			       ;; something like "a-(b+c)"
			       (not (of-required-type? this))
			       ;; for (a = (x in y); ... ; ... )
			       (and in-for-init?
				    (Binary? this)
				    (eq? (Ref-id (Binary-op this)) 'in))
			       ;; (function(){...})();
			       ;; ({a: 2, b:3}.print());
			       (and stmt-begin?
				    (or (Named-Fun? this)
					(Fun? this)
					(Obj-Init? this)))))
	  (new-in-for-init? (and in-for-init?
				 (not needs-parentheses?)))
	  (new-stmt-begin? (and stmt-begin?
				(not needs-parentheses?))))
      (if needs-parentheses?
	  (begin
	     (display "(")
	     (expr-out this env indent new-in-for-init? new-stmt-begin?)
	     (display ")"))
	  (expr-out this env indent new-in-for-init? new-stmt-begin?))))

(define (Block-out-without-braces stmt::Node env::Env indent::int)
   (if (Block? stmt)
       (with-access::Block stmt (els)
	  (for-each (lambda (n)
		       (stmt-out n env indent))
		    els))
       (stmt-out stmt env indent)))

(define (stmt->block stmt)
   (cond
      ((Block? stmt)
       stmt)
      ((NOP? stmt)
       (instantiate::Block (els '())))
      (else
       (instantiate::Block (els (list stmt))))))

(define (block-stmt-out this::Block env::Env indent::int
			indent? new-line-after?)
   (with-access::Block this (els)
      (if indent? (indent! env indent))
      (display "{")
      (newline-out env)
      (for-each (lambda (n)
		   (stmt-out n env (indent+ indent)))
		els)
      (indent! env indent)
      (display "}")
      (if new-line-after? (newline-out env))))
   
(define-method (stmt-out this::Block env::Env indent)
   (block-stmt-out this env indent #t #t))

(define (Var-Decl-List-out this::Var-Decl-List env::Env
			   indent in-for-init?)
   (display "var ")
   (with-access::Var-Decl-List this (els)
       (nested-expr-out (car els) env indent assig-expr? in-for-init? #f)
       (for-each (lambda (n)
		    (display ",")
		    (space-out env)
		    (nested-expr-out n env indent assig-expr? in-for-init? #f))
		 (cdr els))))

(define-method (stmt-out this::Var-Decl-List env indent)
   (indent! env indent)
   (Var-Decl-List-out this env indent #f)
   (display ";")
   (newline-out env))

(define-method (stmt-out this::NOP env indent)
   (indent! env indent)
   (display ";")
   (newline-out env))

(define (If-stmt-out this::If env indent indent?)
   (with-access::If this (test then else)
      (when (and (If? then)            ;; nested if
		 (NOP? (If-else then)) ;; has no else-branch
		 (not (NOP? else)))    ;; but we have one
	 (set! then (instantiate::Block (els (list then)))))
      (when indent? (indent! env indent))
      (display "if")
      (space-out env)
      (display "(")
      (nested-expr-out test env indent expr? #f #f)
      (display ")")
      (let* ((else-branch? (not (NOP? else)))
	     (then-block? (block-body then env indent #f (not else-branch?))))
	 (when else-branch?
	    (if then-block?
		(space-out env)
		(indent! env indent))
	    (display "else")
	    (if (If? else)
		(begin
		   (display #\space)
		   (If-stmt-out else env indent #f))
		(block-body else env indent #t #t))))))
   
(define-method (stmt-out this::If env indent)
   (If-stmt-out this env indent #t))

(define-method (stmt-out this::For env indent)
   (with-access::For this (init test incr body)
      (indent! env indent)
      (display "for")
      (space-out env)
      (display "(")
      (when init
	 ;; no requirement, as init could be a var-init too.
	 (nested-expr-out init env (indent+ indent)
			  no-requirement #t #f))
      (display ";")
      (when test
	 (space-out env)
	 (nested-expr-out test env (indent+ indent) expr? #f #f))
      (display ";")
      (when incr
	 (space-out env)
	 (nested-expr-out incr env (indent+ indent) expr? #f #f))
      (display ")")
      (block-body body env indent #f #t)))

(define-method (stmt-out this::While env indent)
   (with-access::While this (test body)
      (indent! env indent)
      (display "while")
      (space-out env)
      (display "(")
      (nested-expr-out test env (indent+ indent) expr? #f #f)
      (display ")")
      (block-body body env indent #f #t)))

(define-method (stmt-out this::Do env indent)
   (with-access::Do this (body test)
      (indent! env indent)
      (display "do")
      (if (block-body body env indent #t #f)
	  (space-out env)
	  (indent! env indent))
      (display "while")
      (space-out env)
      (display "(")
      (nested-expr-out test env (indent+ indent) expr? #f #f)
      (display ");")
      (newline-out env)))

(define-method (stmt-out this::For-In env indent)
   (with-access::For-In this (lhs obj body)
      (indent! env indent)
      (display "for")
      (space-out env)
      (display "(")
      ;; no-requirement as lhs could be a var-decl-list too.
      ;; note that we set 'in-for-in?' flag to true.
      (nested-expr-out lhs env (indent+ indent) no-requirement #t #f)
      (display " in ")
      (nested-expr-out obj env (indent+ indent) expr? #f #f)
      (display ")")
      (block-body body env indent #f #t)))

(define-method (stmt-out this::Continue env indent)
   (with-access::Continue this (id)
      (indent! env indent)
      (display "continue")
      (when id (display* " " id))
      (display ";")
      (newline-out env)))

(define-method (stmt-out this::Break env indent)
   (with-access::Break this (id)
      (indent! env indent)
      (display "break")
      (when id (display* " " id))
      (display ";")
      (newline-out env)))

(define-method (stmt-out this::Return env indent)
   (with-access::Return this (expr)
      (indent! env indent)
      (display "return ")
      (nested-expr-out expr env (indent+ indent) expr? #f #f)
      (display ";")
      (newline-out env)))

(define-method (stmt-out this::With env indent)
   (with-access::With this (obj body)
      (indent! env indent)
      (display "with")
      (space-out env)
      (display "(")
      (nested-expr-out obj env (indent+ indent) expr? #f #f)
      (display ")")
      (block-body body env indent #f #t)))

(define-method (stmt-out this::Switch env indent)
   (with-access::Switch this (key cases)
      (indent! env indent)
      (display "switch")
      (space-out env)
      (display "(")
      (nested-expr-out key env (indent+ indent) expr? #f #f)
      (display ")")
      (space-out env)
      (display "{")
      (newline-out env)
      (for-each (lambda (n)
		   (stmt-out n env indent))
		cases)
      (indent! env indent)
      (display "}")
      (newline-out env)))

(define-method (stmt-out this::Case env indent)
   (with-access::Case this (expr body)
      (indent! env indent)
      (display "case ")
      (nested-expr-out expr env (indent+ indent) expr? #f #f)
      (display ":")
      (newline-out env)
      (Block-out-without-braces body env (indent+ indent))))

(define-method (stmt-out this::Default env indent)
   (with-access::Default this (body)
      (indent! env indent)
      (display "default:")
      (newline-out env)
      (Block-out-without-braces body env (indent+ indent))))

(define-method (stmt-out this::Throw env indent)
   (with-access::Throw this (expr)
      (indent! env indent)
      (display "throw ")
      (nested-expr-out expr env (indent+ indent) expr? #f #f)
      (display ";")
      (newline-out env)))

(define-method (stmt-out this::Try env indent)
   (with-access::Try this (body catch finally)
      (indent! env indent)
      (display "try")
      (block-body (stmt->block body) env indent #t #f)
      (space-out env)
      (when catch
	 (catch-out catch env indent))
      (if finally
	  (begin
	     (space-out env)
	     (display "finally")
	     (block-body (stmt->block finally) env indent #t #t))
	  (newline-out env))))

(define (catch-out this::Catch env indent)
   (with-access::Catch this (decl body)
      (space-out env)
      (display "catch(")
      ;; must be a ref anyways. -> just test for primary-expr
      (nested-expr-out decl env indent primary-expr? #f #f)
      (display ")")
      (block-body (stmt->block body) env indent #f #t)))

(define-method (stmt-out this::Labelled env indent)
   (with-access::Labelled this (id body)
      (indent! env indent)
      (display* id ":")
      (block-body body env indent #f #t)))

(define (function-out fun::Fun env name indent)
   (define (do-function-out)
      (with-access::Fun fun (params body)
	 (display "function")
	 (when name
	    (display #\space)
	    ;; name must be decl. -> just test for primary-expr
	    (nested-expr-out name env indent primary-expr? #f #f))
	 (display "(")
	 (unless (null? params)
	    ;; params must be refs. -> primary-expr?
	    (nested-expr-out (car params) env indent primary-expr? #f #f)
	    (for-each (lambda (param)
			 (display ",")
			 (space-out env)
			 (nested-expr-out param env indent primary-expr? #f #f))
		      (cdr params)))
	 (display ")")
	 (block-body (stmt->block body) env indent #f #f)))

   (with-access::Env env
	 (generate-function-strings? inside-function? function-str-id
				     function-out-str-port function-strings-ht)
      (cond
	 ((and generate-function-strings?
	       (not inside-function?))
	  ;; top-level function
	  (let ((str-port (open-output-string))
		(fun-str-id (gensym 'fun-str)))
	     (set! function-str-id fun-str-id)
	     (set! function-out-str-port str-port)
	     (set! inside-function? #t)
	     (with-output-to-port str-port do-function-out)
	     (let ((str (close-output-port str-port)))
		(hashtable-put! function-strings-ht
				fun-str-id
				str)
		(Fun-str-set! fun (list fun-str-id 0 (string-length str)))
		(set! inside-function? #f))))
	 (generate-function-strings?
	  (let ((start-pos (string-length
			    (flush-output-port function-out-str-port))))
	     (do-function-out)
	     (let ((end-pos (string-length
			     (flush-output-port function-out-str-port))))
		(with-access::Fun fun (str)
		   (set! str (list function-str-id start-pos end-pos))))))
	 (else
	  (do-function-out)))))

(define-method (stmt-out this::Fun-Binding env indent)
   (with-access::Fun-Binding this (lhs val)
      (indent! env indent)
      (function-out val env lhs indent)
      (newline-out env)))


(define (var-out this::Var)
   (with-access::Var this (generated id)
      (display (or generated id))))


(define-method (expr-out this::Named-Fun env indent in-for-init? stmt-begin?)
   (with-access::Named-Fun this (decl body)
      (function-out body env decl indent)))

(define-method (expr-out this::Fun env indent in-for-init? stmt-begin?)
   ;; note that expr-out already puts parenthesis around the fun if necessary.
   (function-out this env #f indent))

(define-method (expr-out this::Sequence env indent in-for-init? stmt-begin?)
   ;; we require expr?s as elements.
   ;; -> a,(b,c) and (a,b),c will become a,b,c
   (with-access::Sequence this (els)
      (nested-expr-out (car els) env indent expr? in-for-init? stmt-begin?)
      (for-each (lambda (n)
		   (display ",")
		   (space-out env)
		   (nested-expr-out n env indent expr? in-for-init? #f))
		(cdr els))))

;; handles Vassig and Accsig
(define-method (expr-out this::Assig env indent in-for-init? stmt-begin?)
   (with-access::Assig this (lhs val)
      (nested-expr-out lhs env indent lhs-expr? in-for-init? stmt-begin?)
      (space-out env)
      (display "=")
      (space-out env)
      (nested-expr-out val env indent assig-expr? in-for-init? #f)))

;; handles Vassig-op and Accsig-op
(define (Assig-Op-expr-out this::Assig env op indent in-for-init? stmt-begin?)
   (with-access::Assig this (lhs val)
      (nested-expr-out lhs env indent lhs-expr? in-for-init? stmt-begin?)
      (space-out env)
      (display* (op->string (Ref-id op)) "=")
      (space-out env)
      (nested-expr-out val env indent assig-expr? in-for-init? #f)))
   
(define-method (expr-out this::Vassig-Op env indent in-for-init? stmt-begin?)
   (Assig-Op-expr-out this env (Vassig-Op-op this) indent in-for-init? stmt-begin?))

(define-method (expr-out this::Accsig-Op env indent in-for-init? stmt-begin?)
   (Assig-Op-expr-out this env (Accsig-Op-op this) indent in-for-init? stmt-begin?))

(define-method (expr-out this::Cond env indent in-for-init? stmt-begin?)
   (with-access::Cond this (test then else)
      (nested-expr-out test env indent or-expr? in-for-init? stmt-begin?)
      (display "?")
      (space-out env)
      ;; the then part is allowed to have an 'in'.
      (nested-expr-out then env indent assig-expr? #f #f)
      (display ":")
      (space-out env)
      (nested-expr-out else env indent assig-expr? in-for-init? #f)))

(define-method (expr-out this::Binary env indent in-for-init? stmt-begin?)
   (with-access::Binary this (op args)
      (let* ((left (car args))
	     (right (cadr args))
	     (op-id (Ref-id op))
	     (left-req (case op-id
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
				       "Internal Error: forgot op"
				       op-id))))
	     (right-req (case op-id
			   ((OR) or-expr?) ;; x||(y||z) <=> (x||y)||z
			   ((&&) and-expr?) ;; x&&(y&&z) <=> (x&&y)&&z
			   ((BIT_OR) bit-or-expr?) ;; x|(y|z) <=> (x|y)|z
			   ((^)  bit-xor-expr?) ;; x^(y^z) <=> (x^y)^z
			   ((&) bit-and-expr?) ;; x&(y&z) <=> (x&y)&z
			   ((== != === !==) rel-expr?)
			   ((< > <= >= instanceof in) shift-expr?)
			   ((>> << >>>) add-expr?)
			   ;; we can't remove parenthesis for '+' as
			   ;;   x+(y+z) <!=> (x+y)+z
			   ;; ex: "a"+(1+2) => "a3"
			   ;;     ("a"+1)+2 => "a12"
			   ((+ -) mult-expr?)
			   ;; the following line is only true if we ignore
			   ;; precision.
			   ;((*) *-or-mult-expr?) ;; x*(y*z) <=> (x*y)*z
			   ;; use safe unary-expr? instead.
			   ((*) unary-expr?)
			   ((/ %) unary-expr?)
			   (else (error 'js-out
					"Internal Error: forgot op"
					op-id)))))
	 (nested-expr-out left env indent left-req in-for-init? stmt-begin?)
	 (case op-id
	    ;; in theory one could avoid the spaces. at least sometimes.
	    ((in instanceof) (display* " " op-id " "))
	    ((OR) (space-out env) (display "||") (space-out env))
	    ((BIT_OR) (space-out env) (display "|") (space-out env))
	    (else (space-out env) (display op-id) (space-out env)))
	 (nested-expr-out right env indent right-req in-for-init? #f))))

(define-method (expr-out this::Unary env indent in-for-init? stmt-begin?)
   (with-access::Unary this (op args)
      (let ((op-id (Ref-id op)))
	 (case op-id
	    ((unary-+) (display '+))
	    ((unary--) (display '-))
	    (else (display op-id)))
	 (case op-id
	    ((delete void typeof) (display " "))
	    (else 'do-nothing))
	 (nested-expr-out (car args) env indent unary-expr? in-for-init? #f))))

(define-method (expr-out this::Postfix env indent in-for-init? stmt-begin?)
   (with-access::Postfix this (op args)
      (nested-expr-out (car args) env indent lhs-expr? in-for-init? stmt-begin?)
      (display (Ref-id op))))

(define-method (expr-out this::Call env indent in-for-init? stmt-begin?)
   (with-access::Call this (op args)
      ;; we always print parenthesis for 'new' expressions.
      ;; as a consequence our requirements are easier.
      (nested-expr-out op env indent lhs-expr? in-for-init? stmt-begin?)
      (display "(")
      (unless (null? args)
	 (nested-expr-out (car args) env indent assig-expr? #f #f)
	 (for-each (lambda (arg)
		      (display ",")
		      (space-out env)
		      (nested-expr-out arg env indent assig-expr? #f #f))
		   (cdr args)))
      (display ")")))

(define-method (expr-out this::New env indent in-for-init? stmt-begin?)
   (with-access::New this (class args)
      ;; same as for Call. As we always add the parenthesis we have simpler
      ;; requirements.
      (display "new ")
      (nested-expr-out class env indent call-expr? in-for-init? #f)
      (display "(")
      (unless (null? args)
	 (nested-expr-out (car args) env indent assig-expr? #f #f)
	 (for-each (lambda (arg)
		      (display ",")
		      (space-out env)
		      (nested-expr-out arg env indent assig-expr? #f #f))
		   (cdr args)))
      (display ")")))

(define (valid-js-id? field)
   (define (valid-chars? str)
      (let loop ((i 0))
	 (cond
	    ((=fx i (string-length str))
	     #t)
	    ((or (char-alphabetic? (string-ref str i))
		 (char=? #\_ (string-ref str i)))
	     (loop (+fx i 1)))
	    ((and (>fx i 0)
		  (char-numeric? (string-ref str i)))
	     (loop (+fx i 1)))
	    (else #f))))

   (and (String? field)
	(let* ((delimited-str (String-val field))
	       (str (substring delimited-str
			       1 (-fx (string-length delimited-str) 1))))
	   (and (valid-chars? str)
		(not (reserved-word? (string->symbol str)))))))

(define-method (expr-out this::Access env indent in-for-init? stmt-begin?)
   (with-access::Access this (obj field)
      (nested-expr-out obj env indent call-expr? in-for-init? stmt-begin?)
      (if (and (valid-js-id? field)
	       (not (Number? obj)))
	  (let ((field-str (String-val field)))
	     (display ".")
	     (display (substring field-str
				 1 (-fx (string-length field-str) 1))))
	  (begin
	     (display "[")
	     (nested-expr-out field env indent expr? #f #f)
	     (display "]")))))

(define-method (expr-out this::This env indent in-for-init? stmt-begin?)
   (display "this"))

(define-method (expr-out this::Literal env indent in-for-init? stmt-begin?)
   (with-access::Literal this (val)
      (error "Literal-out" "Internal Error: forgot literal type" val)))

(define-method (expr-out this::Undefined env indent in-for-init? stmt-begin?)
   ;; not entirely correct, as 'undefined' can be overwritten.
   ;; but there is no better way to get a fast 'undefined'.
   ;; at least I think so.
   (display "undefined"))

(define-method (expr-out this::Null env indent in-for-init? stmt-begin?)
   (display "null"))

(define-method (expr-out this::Bool env indent in-for-init? stmt-begin?)
   (if (Bool-val this)
       (display "true")
       (display "false")))

(define-method (expr-out this::Number env indent in-for-init? stmt-begin?)
   (display (Number-val this)))

(define-method (expr-out this::String env indent in-for-init? stmt-begin?)
   ;; val is an already escaped string, including the delimiters.
   (display (String-val this)))

(define-method (expr-out this::Ref env indent in-for-init? stmt-begin?)
   (with-access::Ref this (var id)
      (if (not (eq? var (Var-nil)))
	  (var-out var)
	  (display id))))

(define-method (expr-out this::Array env indent in-for-init? stmt-begin?)
   (with-access::Array this (length els)
      ;; basically: we may avoid a "," at the last pos, if the was an el.
      ;; so [1] and [1,] are the same, but [,] and [] are not, as there is
      ;; no el.
      (display "[")
      (let loop ((i 0)
		 (els els))
	 (unless (>= i length)
	    (cond
	       ((or (null? els)
		    (not (= (Array-Element-index (car els)) i)))
		;; empty el. just print the ',' which is always needed.
		(display ",")
		(loop (+fx i 1) els))
	       (else
		(unless (=fx i 0) (space-out env))
		(nested-expr-out (Array-Element-expr (car els)) env
				 indent assig-expr? #f #f)
		(unless (=fx (+fx i 1) length)
		   ;; this is the optim. last element does not need a ','
		   (display ","))
		(loop (+ i 1)
		      (cdr els))))))
      (display "]")))

(define-method (expr-out this::Obj-Init env indent in-for-init? stmt-begin?)
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

   (with-access::Obj-Init this (props)
      (display "{")
      (let loop ((props props)
		 (first? #t))
	 (unless (null? props)
	    (let* ((prop (car props))
		   (name (Property-Init-name prop))
		   (val (Property-Init-val prop)))
	       (unless first?
		  (display ",")
		  (space-out env))
	       (cond
		  ((Number? name)
		   (display (Number-val name)))
		  ;; String
		  ((and (id-chars? (String-val name))
			(> (string-length (String-val name)) 2))
		   (let ((str (String-val name)))
		      (display (substring str 1 (-fx (string-length str) 1)))))
		  (else
		   (display (String-val name))))
	       (display ":")
	       (space-out env)
	       (nested-expr-out val env indent assig-expr? #f #f)
	       (loop (cdr props) #f))))
      (display "}")))

(define-method (expr-out this::Reg-Exp env indent in-for-init? stmt-begin?)
   (with-access::Reg-Exp this (pattern)
      (display pattern)))

(define-method (expr-out this::Var-Decl-List env indent in-for-init? stmt-begin?)
   (Var-Decl-List-out this env indent in-for-init?))
