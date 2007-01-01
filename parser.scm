(module parser
   (include "protobject.sch")
   (include "nodes.sch")
   (option (loadq "protobject-eval.sch"))
   (import lexer
	   protobject
	   nodes
	   verbose)
   (export (parse::pobject port)))

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
	  (error token "unexpected token. expected " type))))

(define (consume-statement-semicolon!)
   (cond
      ((eq? (peek-token-type) 'SEMICOLON)
       (consume-any!))
      ((or (eq? (peek-token-type) 'RBRACE)
	   (at-new-line-token?))
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

(define (parse port)
   (set! *input-port* port)
   (program))

(define (program)
   (new Program (source-elements)))

(define (source-elements)
   (let loop ((rev-ses '()))
      (if (eof?)
	  (new Block (reverse! rev-ses))
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
	  (new Block (reverse! rev-stats)))
	 ;; errors will be handled in the statemente-clause
	 (else (loop (cons (statement) rev-stats))))))

(define (var-decl-list in-for-init?)
   (consume! 'var)
   (let loop ((rev-vars (list (var))))
      (case (peek-token-type)
	 ((SEMICOLON) (if (not in-for-init?)
			  (consume-any!))
		      (new Var-decl-list (reverse! rev-vars)))
	 ((COMMA) (consume-any!)
		  (loop (cons (var) rev-vars)))
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
		   (new Var-decl-list rev-vars))))
	 (else (if (and (not in-for-init?)
			(not (null? rev-vars))
			(at-new-line-token?))
		   (new Var-decl-list (reverse! rev-vars))
		   (error #f
			  "unexpected token, error or EOF"
			  (cdr (consume-any!))))))))
	 
(define (var)
   (let ((id (consume! 'ID)))
      (case (peek-token-type)
	 ((=) (consume-any!)
	      (let ((expr (expression #f)))
		 (new Init (new Decl id) expr)))
	 (else (new Decl id)))))

(define (empty-statement)
   (consume! 'SEMICOLON)
   (new NOP))

(define (iff)
   (consume-any!) ;; the 'if'
   (consume! 'LPAREN)
   (let ((test (expression #f)))
      (consume! 'RPAREN)
      (let ((then (statement)))
	 (case (peek-token-type)
	    ((else) (consume-any!)
		    (let ((else (statement)))
		       (new If test then else)))
	    (else (new If test then (new NOP)))))))

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
	    (new For init test incr body)))))

;; for (lhs/var x in obj)
(define (for-in lhs)
   ;; TODO: weed out bad lhs
   (consume! 'in)
   (let ((obj (expression #f))
	 (ignore-RPAREN (consume! 'RPAREN))
	 (body (statement)))
      (new For-in lhs obj body)))

(define (while)
   (consume! 'while)
   (consume! 'LPAREN)
   (let ((test (expression #f)))
      (consume! 'RPAREN)
      (let ((body (statement)))
	 (new While test body))))

(define (do-while)
   (consume! 'do)
   (let ((body (statement)))
      (consume! 'while)
      (consume! 'LPAREN)
      (let ((test (expression #f)))
	 (consume! 'RPAREN)
	 (consume-statement-semicolon!)
	 (new Do body test))))

(define (continue)
   (consume! 'continue)
   (if (and (eq? (peek-token-type) 'ID)
	    (not (at-new-line-token?)))
       (let ((id (consume! 'ID)))
	  (consume-statement-semicolon!)
	  (new Continue id))
       (begin
	  (consume-statement-semicolon!)
	  (new Continue #f))))

(define (break)
   (consume! 'break)
   (if (and (eq? (peek-token-type) 'ID)
	    (not (at-new-line-token?)))
       (let ((id (consume! 'ID)))
	  (consume-statement-semicolon!)
	  (new Break id))
       (begin
	  (consume-statement-semicolon!)
	  (new Break #f))))

(define (return)
   (consume! 'return)
   (if (or (case (peek-token-type) ((EOF ERROR SEMICOLON) #t) (else #f))
	   (at-new-line-token?))
       (begin
	  (consume-statement-semicolon!)
	  (new Return (new Undefined)))
       (let ((expr (expression #f)))
	  (consume-statement-semicolon!)
	  (new Return expr))))

(define (with)
   (consume! 'with)
   (consume! 'LPAREN)
   (let ((expr (expression #f)))
      (consume! 'RPAREN)
      (let ((body (statement)))
	 (new With expr body))))

(define (switch)
   (consume! 'switch)
   (consume! 'LPAREN)
   (let ((key (expression #f)))
      (consume! 'RPAREN)
      (let ((cases (case-block)))
	 (new Switch key cases))))

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
	 (new Case expr body))))

(define (default-clause)
   (consume! 'default)
   (consume! ':)
   (new Default (switch-clause-statements)))

(define (switch-clause-statements)
   (let loop ((rev-stats '()))
      (case (peek-token-type)
	 ((RBRACE EOF ERROR default case)
	  (new Block (reverse! rev-stats)))
	 (else (loop (cons (statement) rev-stats))))))

(define (throw)
   (consume! 'throw)
   (if (at-new-line-token?)
       (error #f "throw must have a value" #f))
   (let ((expr (expression #f)))
      (consume-statement-semicolon!)
      (new Throw expr)))

(define (trie)
   (consume! 'try)
   (let ((body (block)))
      (let ((catch-part #f)
	    (finally-part #f))
	 (if (eq? (peek-token-type) 'catch)
	     (set! catch-part (catch)))
	 (if (eq? (peek-token-type) 'finally)
	     (set! finally-part (finally)))
	 (new Try body catch-part finally-part))))

(define (catch)
   (consume! 'catch)
   (consume! 'LPAREN)
   (let ((id (consume! 'ID)))
      (consume! 'RPAREN)
      (let ((body (block)))
	 ;; not sure, if 'Param' is a really good choice.
	 ;; we'll see...
	 (new Catch (new Param id) body))))

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
      (new Labelled id (statement))))

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
	  (new Fun-binding (new Decl id) (new Fun params body))
	  (if id
	      (new Named-fun
		   (new Decl id) (new Fun params body))
	      (new Fun params body)))))

(define (params)
   (consume! 'LPAREN)
   (if (eq? (peek-token-type) 'RPAREN)
       (begin
	  (consume-any!)
	  '())
       (let loop ((rev-params (list (new Param (consume! 'ID)))))
	  (if (eq? (peek-token-type) 'COMMA)
	      (begin
		 (consume-any!)
		 (loop (cons (new Param (consume! 'ID))
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
	     (new Block (reverse! rev-ses)))
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
		 (new Sequence (reverse! rev-exprs)))))))

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
		((and (eq? op '=) (inherits-from? expr Access))
		 (new Accsig expr rhs))
		((eq? op '=)
		 (new Vassig expr rhs))
		((inherits-from? expr Access)
		 (new Accsig-op expr (new Var-ref (with-out-= op)) rhs))
		(else
		 (new Vassig-op expr (new Var-ref (with-out-= op)) rhs))))
	  expr)))

(define (cond-expr in-for-init?)
   (let ((expr (binary-expr in-for-init?)))
      (if (eq? (peek-token-type) '?)
	  (let* ((ignore-? (consume-any!))
		 (then (assig-expr #f))
		 (ignore-colon (consume! ':))
		 (else (assig-expr in-for-init?)))
	     (new Cond expr then else))
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
		       (loop (new Binary
				  expr
				  (new Var-ref token-op)
				  (binary-aux (+fx level 1))))))
		   (else
		    expr))))))
   (binary-aux 1))

(define (unary)
   (case (peek-token-type)
      ((delete void typeof ++ -- + - ~ !)
       (new Unary (new Var-ref (car (consume-any!))) (unary)))
      (else
       (postfix))))

(define (postfix)
   (let ((expr (lhs)))
      (if (not (at-new-line-token?))
	  (case (peek-token-type)
	     ((++ --)
	      (let ((op (car (consume-any!))))
		 (new Postfix expr (new Var-ref op))))
	     (else
	      expr))
	  expr)))

(define (lhs)
   (new-expr #t))

(define (new-expr call-allowed?)
   (if (eq? (peek-token-type) 'new)
       (let* ((ignore (consume-any!))
	      (class (new-expr #f))
	      (args (if (eq? (peek-token-type) 'LPAREN)
			(arguments)
			'())))
	  (new New class args))
       (access-or-call call-allowed?)))

(define (access-or-call call-allowed?)
   (let loop ((expr (primary)))
      (case (peek-token-type)
	 ((LBRACKET) (let* ((ignore (consume-any!))
			    (field (expression #f))
			    (ignore-too (consume! 'RBRACKET)))
			(loop (new Access expr field))))
	 ((DOT) (let* ((ignore (consume-any!))
		       (field (consume! 'ID)))
		   (loop (new Dot expr field))))
	 
	 ((LPAREN) (if call-allowed?
		       (loop (new Call expr (arguments)))
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
	      (new This))
      ((ID) (new Var-ref (consume! 'ID)))
      ((LPAREN) (let ((ignore (consume-any!))
		      (expr (expression #f))
		      (ignore-too (consume! 'RPAREN)))
		   expr))
      ((LBRACKET) (array-literal))
      ((LBRACE) (object-literal))
      ((null) (consume-any!)
	      (new Null))
      ((true false) (new Bool (eq? (car (consume-any!)) 'true)))
      ((NUMBER) (new Number (consume! 'NUMBER))) ;; still as string!
      ((STRING) (new String (consume! 'STRING)))
      ((EOF) (error #f "unexpected end of file" #f))
      ((/ /=) (let ((reg-exp (read-regexp (peek-token-type))))
		 ;; consume-any *must* be after having read the reg-exp.
		 (consume-any!) ;; the / or /=
		 (new Reg-exp reg-exp)))
      (else
       (error #f "unexpected token: " (consume-any!)))))

(define (array-literal)
   (consume! 'LBRACKET)
   (let loop ((rev-els '())
	      (length 0))
      (case (peek-token-type)
	 ((RBRACKET) (consume-any!)
		     (new Array (reverse! rev-els) length))
	 ((COMMA) (loop rev-els (+fx length 1)))
	 (else (let ((array-el (new Array-element length (assig-expr #f))))
		  (if (eq? (peek-token-type) 'COMMA)
		      (begin
			 (consume-any!)
			 (loop (cons array-el rev-els)
			       (+fx length 1)))
		      (begin
			 (consume! 'RBRACKET)
			 (new Array (reverse! (cons array-el rev-els))
			      (+fx length 1)))))))))

(define (object-literal)
   (define (property-name)
      (case (peek-token-type)
	 ((ID) (consume! 'ID))
	 ((STRING) (new String (consume! 'STRING)))
	 ((NUMBER) (new Number (consume! 'NUMBER)))))
   
   (define (property-init)
      (let* ((name (property-name))
	     (ignore (consume! ':))
	     (val (assig-expr #f)))
	 (new Property-init name val)))
   
   (consume! 'LBRACE)
   (if (eq? (peek-token-type) 'RBRACE)
       (new Obj-init '())
       (let loop ((rev-props (list (property-init))))
	  (if (eq? (peek-token-type) 'RBRACE)
	      (begin
		 (consume-any!)
		 (new Obj-init (reverse! rev-props)))
	      (begin
		 (consume! 'COMMA)
		 (loop (cons (property-init) rev-props)))))))
