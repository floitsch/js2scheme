(module parser
   (include "protobject.sch")
   (include "nodes.sch")
   (option (loadq "protobject-eval.sch"))
   (import lexer
	   protobject
	   nodes
	   verbose)
   (export (parse::pobject port)))

(define (parse port)
   ;; fun-body at bottom of file
   
   (define *input-port* #f)
   (define *peeked-tokens* '())
   (define *previous-token-type* #unspecified) ;; including new-line
   
   
   (define (read-regexp intro-token)
      (let ((token (read/rp *Reg-exp-grammar* *input-port*)))
	 (if (eq? (car token) 'EOF)
	     (error #f "unfinished regular expression literal" #f))
	 (if (eq? (car token) 'ERROR)
	     (error #f "bad regular-expression literal" (cdr token)))
	 (string-append (symbol->string intro-token) (cdr token))))
   
   (define (peek-token)
      (if (null? *peeked-tokens*)
	  (begin
	     (set! *peeked-tokens* (list (read/rp *JS-grammar* *input-port*)))
	     ;(display* (cdar *peeked-tokens*) " ")
	     (if (eq? (caar *peeked-tokens*)
		      'NEW_LINE)
		 (begin
		    (set! *previous-token-type* 'NEW_LINE)
		    (set! *peeked-tokens* (cdr *peeked-tokens*))
		    (peek-token))
		 (car *peeked-tokens*)))
	  (car *peeked-tokens*)))
   
   (define (token-push-back! token)
      (set! *peeked-tokens* (cons token *peeked-tokens*)))
   
   (define (peek-token-type)
      (car (peek-token)))
   
   (define (at-new-line-token?)
      (eq? *previous-token-type* 'NEW_LINE))
   
   (define (consume! type)
      (let ((token (consume-any!)))
	 (if (eq? (car token) type)
	     (cdr token)
	     (error token
		    (format "unexpected token. expected ~a got: " type)
		    (car token)))))
   
   (define (consume-statement-semicolon!)
      (cond
	 ((eq? (peek-token-type) 'SEMICOLON)
	  (consume-any!))
	 ((or (eq? (peek-token-type) 'RBRACE)
	      (at-new-line-token?)
	      (eq? (peek-token-type) 'EOF))
	  'do-nothing)
	 (else
	  (error #f "unexpected token: " (peek-token)))))
   
   (define (consume-any!)
      (let ((res (peek-token)))
	 (set! *previous-token-type* (car res))
	 (set! *peeked-tokens* (cdr *peeked-tokens*))
	 (peek-token) ;; prepare new token.
	 res))
   
   (define (eof?)
      (eq? (peek-token-type) 'EOF))
   
   (define (program)
      (new-node Program (source-elements)))
   
   (define (source-elements)
      (let loop ((rev-ses '()))
	 (if (eof?)
	     (new-node Block (reverse! rev-ses))
	     (loop (cons (source-element) rev-ses)))))
   
   (define (source-element)
      (case (peek-token-type)
	 ((function) (function-declaration))
	 ((ERROR EOF) (error #f "eof or error" (cdr (consume-any!))))
	 (else (statement))))
   
   (define (statement)
      (case (peek-token-type)
	 ((LBRACE) (block))
	 ((var) (var-decl-list #f))
	 ((SEMICOLON) (empty-statement))
	 ((if) (iff))
	 ((for while do) (iteration))
	 ((continue) (continue))
	 ((break) (break))
	 ((return) (return))
	 ((with) (with))
	 ((switch) (switch))
	 ((throw) (throw))
	 ((try) (trie))
	 ((ID) (labelled-or-expr))
	 ;; errors will be handled in the expr-clause.
	 (else (expression-statement))))
   
   (define (block)
      (consume! 'LBRACE)
      (let loop ((rev-stats '()))
	 (case (peek-token-type)
	    ((RBRACE)
	     (consume-any!)
	     (new-node Block (reverse! rev-stats)))
	    ;; errors will be handled in the statemente-clause
	    (else (loop (cons (statement) rev-stats))))))
   
   (define (var-decl-list in-for-init?)
      (consume! 'var)
      (let loop ((rev-vars (list (var in-for-init?))))
	 (case (peek-token-type)
	    ((SEMICOLON) (if (not in-for-init?)
			     (consume-any!))
			 (new-node Var-decl-list (reverse! rev-vars)))
	    ((COMMA) (consume-any!)
		     (loop (cons (var in-for-init?) rev-vars)))
	    ((in) (cond
		     ((not in-for-init?)
		      (error "var-decl-list"
			     "unexpected token: "
			     "in"))
		     ((or (null? rev-vars)
			  (not (null? (cdr rev-vars))))
		      (error "var-decl-list"
			     "for (var ... in ...) must contain *one* variable"
			     #f))
		     (else
		      (new-node Var-decl-list rev-vars))))
	    (else (if (and (not in-for-init?)
			   (not (null? rev-vars))
			   (or (at-new-line-token?)
			       (eq? (peek-token-type) 'EOF)))
		      (new-node Var-decl-list (reverse! rev-vars))
		      (error #f
			     "unexpected token, error or EOF"
			     (cdr (consume-any!))))))))
   
   (define (var in-for-init?)
      (let ((id (consume! 'ID)))
	 (case (peek-token-type)
	    ((=) (consume-any!)
		 (let ((expr (assig-expr in-for-init?)))
		    (new-node Init (new-node Decl id) expr)))
	    (else (new-node Decl id)))))
   
   (define (empty-statement)
      (consume! 'SEMICOLON)
      (new-node NOP))
   
   (define (iff)
      (consume-any!) ;; the 'if'
      (consume! 'LPAREN)
      (let ((test (expression #f)))
	 (consume! 'RPAREN)
	 (let ((then (statement)))
	    (case (peek-token-type)
	       ((else) (consume-any!)
		       (let ((else (statement)))
			  (new-node If test then else)))
	       (else (new-node If test then (new-node NOP)))))))
   
   (define (iteration)
      (case (peek-token-type)
	 ((for) (for))
	 ((while) (while))
	 ((do) (do-while))))
   
   (define (for)
      (define (init-first-part)
	 (case (peek-token-type)
	    ((var) (var-decl-list #t))
	    ((SEMICOLON) #f)
	    (else (expression #t))))
      
      (consume! 'for)
      (consume! 'LPAREN)
      (let ((first-part (init-first-part)))
	 (case (peek-token-type)
	    ((SEMICOLON) (for-init/test/incr first-part))
	    ((in) (for-in first-part)))))
   
   ;; for (init; test; incr)
   (define (for-init/test/incr init)
      (consume! 'SEMICOLON)
      (let ((test (case (peek-token-type)
		     ((SEMICOLON) #f)
		     (else (expression #f)))))
	 (consume! 'SEMICOLON)
	 (let ((incr (case (peek-token-type)
			((RPAREN) #f)
			(else (expression #f)))))
	    (consume! 'RPAREN)
	    (let* ((body (statement)))
	       (new-node For init test incr body)))))
   
   ;; for (lhs/var x in obj)
   (define (for-in lhs)
      ;; TODO: weed out bad lhs
      (consume! 'in)
      (let ((obj (expression #f))
	    (ignore-RPAREN (consume! 'RPAREN))
	    (body (statement)))
	 (cond
	    ((inherits-from? lhs (node 'Var-decl-list))
	     (let ((lhs-vars lhs.els))
		(unless (null? (cdr lhs-vars))
		   (error #f
			  "Only one variable allowed in 'for ... in' loop"
			  (cadr lhs-vars).id))
		(new-node For-in (car lhs-vars) obj body)))
	    ((or (inherits-from? lhs (node 'Sequence))
		 (inherits-from? lhs (node 'Assig))
		 (inherits-from? lhs (node 'Binary))
		 (inherits-from? lhs (node 'Unary))
		 (inherits-from? lhs (node 'Postfix)))
	     (error #f
		    "Bad left-hand side in 'for ... in' loop construct"
		    (pobject-name lhs)))
	    (else
	     (new-node For-in lhs obj body)))))
   
   (define (while)
      (consume! 'while)
      (consume! 'LPAREN)
      (let ((test (expression #f)))
	 (consume! 'RPAREN)
	 (let ((body (statement)))
	    (new-node While test body))))
   
   (define (do-while)
      (consume! 'do)
      (let ((body (statement)))
	 (consume! 'while)
	 (consume! 'LPAREN)
	 (let ((test (expression #f)))
	    (consume! 'RPAREN)
	    (consume-statement-semicolon!)
	    (new-node Do body test))))
   
   (define (continue)
      (consume! 'continue)
      (if (and (eq? (peek-token-type) 'ID)
	       (not (at-new-line-token?)))
	  (let ((id (consume! 'ID)))
	     (consume-statement-semicolon!)
	     (new-node Continue id))
	  (begin
	     (consume-statement-semicolon!)
	     (new-node Continue #f))))
   
   (define (break)
      (consume! 'break)
      (if (and (eq? (peek-token-type) 'ID)
	       (not (at-new-line-token?)))
	  (let ((id (consume! 'ID)))
	     (consume-statement-semicolon!)
	     (new-node Break id))
	  (begin
	     (consume-statement-semicolon!)
	     (new-node Break #f))))
   
   (define (return)
      (consume! 'return)
      (if (or (case (peek-token-type) ((EOF ERROR SEMICOLON) #t) (else #f))
	      (at-new-line-token?))
	  (begin
	     (consume-statement-semicolon!)
	     (new-node Return (new-node Undefined)))
	  (let ((expr (expression #f)))
	     (consume-statement-semicolon!)
	     (new-node Return expr))))
   
   (define (with)
      (consume! 'with)
      (consume! 'LPAREN)
      (let ((expr (expression #f)))
	 (consume! 'RPAREN)
	 (let ((body (statement)))
	    (new-node With expr body))))
   
   (define (switch)
      (consume! 'switch)
      (consume! 'LPAREN)
      (let ((key (expression #f)))
	 (consume! 'RPAREN)
	 (let ((cases (case-block)))
	    (new-node Switch key cases))))
   
   (define (case-block)
      (consume! 'LBRACE)
      (let loop ((rev-cases '())
		 (default-case-done? #f))
	 (case (peek-token-type)
	    ((RBRACE) (consume-any!)
		      (reverse! rev-cases))
	    ((case) (loop (cons (case-clause) rev-cases)
			  default-case-done?))
	    ((default) (if default-case-done?
			   (error #f "Only one default-clause allowed"
				  (peek-token))
			   (loop (cons (default-clause) rev-cases)
				 #t))))))
   
   (define (case-clause)
      (consume! 'case)
      (let ((expr (expression #f)))
	 (consume! ':)
	 (let ((body (switch-clause-statements)))
	    (new-node Case expr body))))
   
   (define (default-clause)
      (consume! 'default)
      (consume! ':)
      (new-node Default (switch-clause-statements)))
   
   (define (switch-clause-statements)
      (let loop ((rev-stats '()))
	 (case (peek-token-type)
	    ((RBRACE EOF ERROR default case)
	     (new-node Block (reverse! rev-stats)))
	    (else (loop (cons (statement) rev-stats))))))
   
   (define (throw)
      (consume! 'throw)
      (if (at-new-line-token?)
	  (error #f "throw must have a value" #f))
      (let ((expr (expression #f)))
	 (consume-statement-semicolon!)
	 (new-node Throw expr)))
   
   (define (trie)
      (consume! 'try)
      (let ((body (block)))
	 (let ((catch-part #f)
	       (finally-part #f))
	    (if (eq? (peek-token-type) 'catch)
		(set! catch-part (catch)))
	    (if (eq? (peek-token-type) 'finally)
		(set! finally-part (finally)))
	    (new-node Try body catch-part finally-part))))
   
   (define (catch)
      (consume! 'catch)
      (consume! 'LPAREN)
      (let ((id (consume! 'ID)))
	 (consume! 'RPAREN)
	 (let ((body (block)))
	    ;; not sure, if 'Param' is a really good choice.
	    ;; we'll see...
	    (new-node Catch (new-node Param id) body))))
   
   (define (finally)
      (consume! 'finally)
      (block))
   
   (define (labelled-or-expr)
      (let* ((id-token (consume-any!))
	     (next-token-type (peek-token-type)))
	 ;; TODO: following 2 lines should be assert.
	 [assert (id-token) (eq? (car id-token) 'ID)]
	 (token-push-back! id-token)
	 (if (eq? next-token-type  ':)
	     (labelled)
	     (expression-statement))))
   
   (define (expression-statement)
      (let ((expr (expression #f)))
	 (consume-statement-semicolon!)
	 expr))
   
   (define (labelled)
      (let ((id (consume! 'ID)))
	 (consume! ':)
	 (new-node Labelled id (statement))))
   
   (define (function-declaration)
      (function #t))
   
   (define (function-expression)
      (function #f))
   
   (define (function declaration?)
      (consume! 'function)
      (let* ((id (if (or declaration?
			 (eq? (peek-token-type) 'ID))
		     (consume! 'ID)
		     #f))
	     (params (params))
	     (body (fun-body)))
	 (if declaration?
	     (new-node Fun-binding (new-node Decl id) (new-node Fun params body))
	     (if id
		 (new-node Named-fun
			   (new-node Decl id) (new-node Fun params body))
		 (new-node Fun params body)))))
   
   (define (params)
      (consume! 'LPAREN)
      (if (eq? (peek-token-type) 'RPAREN)
	  (begin
	     (consume-any!)
	     '())
	  (let loop ((rev-params (list (new-node Param (consume! 'ID)))))
	     (if (eq? (peek-token-type) 'COMMA)
		 (begin
		    (consume-any!)
		    (loop (cons (new-node Param (consume! 'ID))
				rev-params)))
		 (begin
		    (consume! 'RPAREN)
		    (reverse! rev-params))))))
   
   (define (fun-body)
      (consume! 'LBRACE)
      (let loop ((rev-ses '()))
	 (if (eq? (peek-token-type) 'RBRACE)
	     (begin
		(consume-any!)
		(new-node Block (reverse! rev-ses)))
	     (loop (cons (source-element) rev-ses)))))
   
   (define (expression in-for-init?)
      (let ((assig (assig-expr in-for-init?)))
	 (let loop ((rev-exprs (list assig)))
	    (if (eq? (peek-token-type) 'COMMA)
		(begin
		   (consume-any!)
		   (loop (cons (assig-expr in-for-init?) rev-exprs)))
		(if (null? (cdr rev-exprs))
		    (car rev-exprs)
		    (new-node Sequence (reverse! rev-exprs)))))))
   
   (define (assig-operator? x)
      (case x
	 ((= *= /= %= += -= <<= >>= >>>= &= ^= BIT_OR=)
	  #t)
	 (else #f)))
   
   (define (assig-expr in-for-init?)
      (define (with-out-= op=)
	 (let* ((s= (symbol->string op=))
		(s=-length (string-length s=))
		(s (substring s= 0 (- s=-length 1)))
		(op (string->symbol s)))
	    op))
      
      (let ((expr (cond-expr in-for-init?)))
	 (if (assig-operator? (peek-token-type))
	     (let* ((op (car (consume-any!))) ;; ops are in car
		    (rhs (assig-expr in-for-init?)))
		;; TODO: weed out bad lhs exprs
		(cond
		   ((and (eq? op '=) (inherits-from? expr (node 'Access)))
		    (new-node Accsig expr rhs))
		   ((eq? op '=)
		    (new-node Vassig expr rhs))
		   ((inherits-from? expr (node 'Access))
		    (new-node Accsig-op expr (new-node Var-ref (with-out-= op)) rhs))
		   (else
		    (new-node Vassig-op expr (new-node Var-ref (with-out-= op)) rhs))))
	     expr)))
   
   (define (cond-expr in-for-init?)
      (let ((expr (binary-expr in-for-init?)))
	 (if (eq? (peek-token-type) '?)
	     (let* ((ignore-? (consume-any!))
		    (then (assig-expr #f))
		    (ignore-colon (consume! ':))
		    (else (assig-expr in-for-init?)))
		(new-node Cond expr then else))
	     expr)))
   
   (define (op-level op)
      (case op
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
	 (else #f)))
   
   ;; left-associative binary expressions
   (define (binary-expr in-for-init?)
      (define (binary-aux level)
	 (if (> level 10)
	     (unary)
	     (let loop ((expr (binary-aux (+fx level 1))))
		(let* ((type (peek-token-type))
		       (new-level (op-level type)))
		   (cond
		      ((and in-for-init? (eq? type 'in))
		       expr)
		      ((not new-level)
		       expr)
		      ((=fx new-level level)
		       ;; ops are in car
		       (let ((token-op (car (consume-any!))))
			  (loop (new-node Binary
					  expr
					  (new-node Var-ref token-op)
					  (binary-aux (+fx level 1))))))
		      (else
		       expr))))))
      (binary-aux 1))
   
   (define (unary)
      (case (peek-token-type)
	 ((delete void typeof ~ ! ++ --)
	  (new-node Unary (new-node Var-ref (car (consume-any!))) (unary)))
	 ((+ -)
	  (new-node Unary (new-node Var-ref (symbol-append 'unary- (car (consume-any!)))) (unary)))
	 (else
	  (postfix))))
   
   (define (postfix)
      (let ((expr (lhs)))
	 (if (not (at-new-line-token?))
	     (case (peek-token-type)
		((++ --)
		 (let ((op (car (consume-any!))))
		    (new-node Postfix expr (new-node Var-ref op))))
		(else
		 expr))
	     expr)))

   ;; we start by getting all news (new-expr)
   ;; the remaining access and calls are then caught by the access-or-call
   ;; invocation allowing call-parenthesis.
   ;;
   ;; the access-or-call in new-expr does not all any parenthesis to be
   ;; consumed as they would be part of the new-expr.
   (define (lhs)
      (access-or-call (new-expr) #t))

   (define (new-expr)
      (if (eq? (peek-token-type) 'new)
	  (let* ((ignore (consume-any!))
		 (class (new-expr))
		 (args (if (eq? (peek-token-type) 'LPAREN)
			   (arguments)
			   '())))
	     (new-node New class args))
	  (access-or-call (primary) #f)))
   
   (define (access-or-call expr call-allowed?)
      (let loop ((expr expr))
	 (case (peek-token-type)
	    ((LBRACKET) (let* ((ignore (consume-any!))
			       (field (expression #f))
			       (ignore-too (consume! 'RBRACKET)))
			   (loop (new-node Access expr field))))
	    ((DOT) (let* ((ignore (consume-any!))
			  (field (consume! 'ID)))
		      (loop (new-node Dot expr field))))
	    
	    ((LPAREN) (if call-allowed?
			  (loop (new-node Call expr (arguments)))
			  expr))
	    (else expr))))
   
   (define (arguments)
      (consume! 'LPAREN)
      (if (eq? (peek-token-type) 'RPAREN)
	  (begin
	     (consume-any!)
	     '())
	  (let loop ((rev-args (list (assig-expr #f))))
	     (if (eq? (peek-token-type) 'RPAREN)
		 (begin
		    (consume-any!)
		    (reverse! rev-args))
		 (let* ((ignore (consume! 'COMMA))
			(arg (assig-expr #f)))
		    (loop (cons arg rev-args)))))))
   
   (define (primary)
      (case (peek-token-type)
	 ((function) (function-expression))
	 ((this) (consume-any!)
		 (new-node This))
	 ((ID) (new-node Var-ref (consume! 'ID)))
	 ((LPAREN) (let ((ignore (consume-any!))
			 (expr (expression #f))
			 (ignore-too (consume! 'RPAREN)))
		      expr))
	 ((LBRACKET) (array-literal))
	 ((LBRACE) (object-literal))
	 ((null) (consume-any!)
		 (new-node Null))
	 ((true false) (new-node Bool (eq? (car (consume-any!)) 'true)))
	 ((NUMBER) (new-node Number (consume! 'NUMBER))) ;; still as string!
	 ((STRING) (new-node String (consume! 'STRING)))
	 ((EOF) (error #f "unexpected end of file" #f))
	 ((/ /=) (let ((reg-exp (read-regexp (peek-token-type))))
		    ;; consume-any *must* be after having read the reg-exp.
		    (consume-any!) ;; the / or /=
		    (new-node Reg-exp reg-exp)))
	 (else
	  (error #f "unexpected token: " (consume-any!)))))
   
   (define (array-literal)
      (consume! 'LBRACKET)
      (let loop ((rev-els '())
		 (length 0))
	 (case (peek-token-type)
	    ((RBRACKET) (consume-any!)
			(new-node Array (reverse! rev-els) length))
	    ((COMMA) (loop rev-els (+fx length 1)))
	    (else (let ((array-el (new-node Array-element length (assig-expr #f))))
		     (if (eq? (peek-token-type) 'COMMA)
			 (begin
			    (consume-any!)
			    (loop (cons array-el rev-els)
				  (+fx length 1)))
			 (begin
			    (consume! 'RBRACKET)
			    (new-node Array (reverse! (cons array-el rev-els))
				      (+fx length 1)))))))))
   
   (define (object-literal)
      (define (property-name)
	 (case (peek-token-type)
	    ((ID) (consume! 'ID))
	    ((STRING) (new-node String (consume! 'STRING)))
	    ((NUMBER) (new-node Number (consume! 'NUMBER)))))
      
      (define (property-init)
	 (let* ((name (property-name))
		(ignore (consume! ':))
		(val (assig-expr #f)))
	    (new-node Property-init name val)))
      
      (consume! 'LBRACE)
      (if (eq? (peek-token-type) 'RBRACE)
	  (begin
	     (consume-any!)
	     (new-node Obj-init '()))
	  (let loop ((rev-props (list (property-init))))
	     (if (eq? (peek-token-type) 'RBRACE)
		 (begin
		    (consume-any!)
		    (new-node Obj-init (reverse! rev-props)))
		 (begin
		    (consume! 'COMMA)
		    (loop (cons (property-init) rev-props)))))))
   
   (set! *input-port* port)
   (program))
