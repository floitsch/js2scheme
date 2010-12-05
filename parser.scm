(module parser
   (import lexer
	   nodes
	   verbose
	   config)
   (export (parse::Node port)))

(define (my-error msg obj token)
   (cond
      ((not token)
       (error "parser"
	      msg
	      obj))
      ((token-pos token)
       (match-case (token-pos token)
	  ((at ?fname ?loc)
	   (error/location "parser" msg obj fname loc))
	  (else
	   (my-error msg obj #f))))
      (else
       (my-error msg obj #f))))

(define (unexpected-token-error token #!optional expected-type)
   (cond
      ((or (eq? (token-type token) 'EOF)
	   (and (eq? (token-type token) 'ERROR)
		(eq? (token-val token) 'eof)))
       (my-error "unexpected end of file" 'EOF token))
      (expected-type
       (my-error (format "unexpected token. expected ~a" expected-type)
		 (token-type token)
		 token))
      (else
       (my-error "unexpected token" token token))))
   
(define (parse port)
   ;; fun-body at bottom of file
   
   (define *input-port* #f)
   (define *peeked-tokens* '())
   (define *previous-token-type* #unspecified) ;; including new-line
   
   
   (define (read-regexp intro-token)
      (let ((token (read/rp *Reg-exp-grammar* *input-port*)))
	 (case (token-type token)
	    ((REG-EXP) (string-append (symbol->string intro-token)
				      (token-val token)))
	    ((ERROR)
	     (my-error "bad regular-expression literal" (token-val token) token))
	    (else (unexpected-token-error token)))))
   
   (define (peek-token)
      (if (null? *peeked-tokens*)
	  (begin
	     (set! *peeked-tokens* (list (read/rp *JS-grammar* *input-port*)))
	     ;(display* (cdar *peeked-tokens*) " ")
	     (if (eq? 'NEW_LINE (token-type (car *peeked-tokens*)))
		 (begin
		    (set! *previous-token-type* 'NEW_LINE)
		    (set! *peeked-tokens* (cdr *peeked-tokens*))
		    (peek-token))
		 (car *peeked-tokens*)))
	  (car *peeked-tokens*)))
   
   (define (token-push-back! token)
      (set! *peeked-tokens* (cons token *peeked-tokens*)))
   
   (define (peek-token-type)
      (token-type (peek-token)))
   
   (define (at-new-line-token?)
      (eq? *previous-token-type* 'NEW_LINE))
   
   (define (consume! type)
      (let ((token (consume-any!)))
	 (cond
	    ((or (eq? type #t)
		 (eq? (token-type token) type))
	     (token-val token))
	    (else (unexpected-token-error token type)))))
   
   (define (consume-statement-semicolon!)
      (cond
	 ((eq? (peek-token-type) 'SEMICOLON)
	  (consume-any!))
	 ((or (eq? (peek-token-type) 'RBRACE)
	      (at-new-line-token?)
	      (eq? (peek-token-type) 'EOF))
	  'do-nothing)
	 (else
	  (unexpected-token-error (peek-token)))))
   
   (define (consume-any!)
      (let ((res (peek-token)))
	 (set! *previous-token-type* (token-type res))
	 (set! *peeked-tokens* (cdr *peeked-tokens*))
	 (peek-token) ;; prepare new token.
	 res))
   
   (define (eof?)
      (eq? (peek-token-type) 'EOF))
   
   (define (program)
      (instantiate::Program
	 (body (source-elements))))
   
   (define (source-elements)
      (let loop ((rev-ses '()))
	 (if (eof?)
	     (instantiate::Block
		(els (reverse! rev-ses)))
	     (loop (cons (source-element) rev-ses)))))
   
   (define (source-element)
      (case (peek-token-type)
	 ((function) (function-declaration))
	 ((EOF ERROR)
	  (unexpected-token-error (consume-any!)))
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
	 ((function) (if (config 'strict-fun-decls)
			 (my-error "function keyword at bad position" #f (peek-token))
			 (function-declaration)))
	 ((ID) (labelled-or-expr))
	 ;; errors will be handled in the expr-clause.
	 (else (expression-statement))))
   
   (define (block)
      (consume! 'LBRACE)
      (let loop ((rev-stats '()))
	 (case (peek-token-type)
	    ((RBRACE)
	     (consume-any!)
	     (instantiate::Block
		(els (reverse! rev-stats))))
	    ;; errors will be handled in the statemente-clause
	    (else (loop (cons (statement) rev-stats))))))
   
   (define (var-decl-list in-for-init?)
      (consume! 'var)
      (let loop ((rev-vars (list (var in-for-init?))))
	 (case (peek-token-type)
	    ((SEMICOLON) (if (not in-for-init?)
			     (consume-any!))
			 (instantiate::Var-Decl-List
			    (els (reverse! rev-vars))))
	    ((COMMA) (consume-any!)
		     (loop (cons (var in-for-init?) rev-vars)))
	    ((in) (cond
		     ((not in-for-init?)
		      (my-error "bad token:"
				"in"
				(peek-token)))
		     (else
		      (instantiate::Var-Decl-List
			 (els (reverse! rev-vars))))))
	    (else (if (and (not in-for-init?)
			   (or (at-new-line-token?)
			       (eq? (peek-token-type) 'EOF)))
		      (instantiate::Var-Decl-List
			 (els (reverse! rev-vars)))
		      (unexpected-token-error (consume-any!)))))))
   
   (define (var in-for-init?)
      (let ((id (consume! 'ID)))
	 (case (peek-token-type)
	    ((=) (consume-any!)
		 (let ((expr (assig-expr in-for-init?)))
		    (instantiate::Init
		       (lhs (instantiate::Decl
			       (id id)))
		       (val expr))))
	    (else (instantiate::Decl (id id))))))
   
   (define (empty-statement)
      (consume! 'SEMICOLON)
      (instantiate::NOP))
   
   (define (iff)
      (consume-any!) ;; the 'if'
      (consume! 'LPAREN)
      (let ((test (expression #f)))
	 (consume! 'RPAREN)
	 (let ((then (statement)))
	    (case (peek-token-type)
	       ((else) (consume-any!)
		       (let ((else (statement)))
			  (instantiate::If
			     (test test)
			     (then then)
			     (else else))))
	       (else (instantiate::If
			(test test)
			(then then)
			(else (instantiate::NOP))))))))
   
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
	       (instantiate::For
		  (init init)
		  (test test)
		  (incr incr)
		  (body body))))))
   
   ;; for (lhs/var x in obj)
   (define (for-in lhs)
      ;; TODO: weed out bad lhs
      (let* ((error-token (peek-token))
	     (dummy (consume! 'in))
	     (obj (expression #f))
	     (ignore-RPAREN (consume! 'RPAREN))
	     (body (statement)))
	 (cond
	    ((Var-Decl-List? lhs)
	     (with-access::Var-Decl-List lhs (els)
		(unless (null? (cdr els))
		   (my-error "Only one variable allowed in 'for-in' loop"
			     (Ref-id (cadr els))
			     error-token))
		(instantiate::For-In
		   (lhs lhs)
		   (obj obj)
		   (body body))))
	    ((or (Ref? lhs)
		 (Access? lhs))
	     (instantiate::For-In
		(lhs lhs)
		(obj obj)
		(body body)))
	    (else
	     (my-error "Bad left-hand side in 'for-in' loop construct"
		       (class-name (object-class lhs))
		       error-token)))))

   (define (while)
      (consume! 'while)
      (consume! 'LPAREN)
      (let ((test (expression #f)))
	 (consume! 'RPAREN)
	 (let ((body (statement)))
	    (instantiate::While
	       (test test)
	       (body body)))))
   
   (define (do-while)
      (consume! 'do)
      (let ((body (statement)))
	 (consume! 'while)
	 (consume! 'LPAREN)
	 (let ((test (expression #f)))
	    (consume! 'RPAREN)
	    (consume-statement-semicolon!)
	    (instantiate::Do
	       (body body)
	       (test test)))))
   
   (define (continue)
      (consume! 'continue)
      (if (and (eq? (peek-token-type) 'ID)
	       (not (at-new-line-token?)))
	  (let ((id (consume! 'ID)))
	     (consume-statement-semicolon!)
	     (instantiate::Continue (id id)))
	  (begin
	     (consume-statement-semicolon!)
	     (instantiate::Continue (id #f)))))
   
   (define (break)
      (consume! 'break)
      (if (and (eq? (peek-token-type) 'ID)
	       (not (at-new-line-token?)))
	  (let ((id (consume! 'ID)))
	     (consume-statement-semicolon!)
	     (instantiate::Break (id id)))
	  (begin
	     (consume-statement-semicolon!)
	     (instantiate::Break (id #f)))))
   
   (define (return)
      (consume! 'return)
      (if (or (case (peek-token-type) ((EOF ERROR SEMICOLON) #t) (else #f))
	      (at-new-line-token?))
	  (begin
	     (consume-statement-semicolon!)
	     (instantiate::Return
		(expr (new-undefined))))
	  (let ((expr (expression #f)))
	     (consume-statement-semicolon!)
	     (instantiate::Return
		(expr expr)))))
   
   (define (with)
      (consume! 'with)
      (consume! 'LPAREN)
      (let ((expr (expression #f)))
	 (consume! 'RPAREN)
	 (let ((body (statement)))
	    (instantiate::With
	       (obj-id (gensym 'with))
	       (obj expr)
	       (body body)))))
   
   (define (switch)
      (consume! 'switch)
      (consume! 'LPAREN)
      (let ((key (expression #f)))
	 (consume! 'RPAREN)
	 (let ((cases (case-block)))
	    (instantiate::Switch
	       (key key)
	       (cases cases)))))
   
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
			   (my-error "Only one default-clause allowed"
				     (peek-token)
				     (peek-token))
			   (loop (cons (default-clause) rev-cases)
				 #t))))))
   
   (define (case-clause)
      (consume! 'case)
      (let ((expr (expression #f)))
	 (consume! ':)
	 (let ((body (switch-clause-statements)))
	    (instantiate::Case
	       (expr expr)
	       (body body)))))
   
   (define (default-clause)
      (consume! 'default)
      (consume! ':)
      (instantiate::Default
	 (body (switch-clause-statements))))
   
   (define (switch-clause-statements)
      (let loop ((rev-stats '()))
	 (case (peek-token-type)
	    ((RBRACE EOF ERROR default case)
	     (instantiate::Block
		(els (reverse! rev-stats))))
	    (else (loop (cons (statement) rev-stats))))))
   
   (define (throw)
      (consume! 'throw)
      (if (at-new-line-token?)
	  (my-error "throw must have a value" #f (peek-token)))
      (let ((expr (expression #f)))
	 (consume-statement-semicolon!)
	 (instantiate::Throw
	    (expr expr))))
   
   (define (trie)
      (consume! 'try)
      (let ((body (block)))
	 (let ((catch-part #f)
	       (finally-part #f))
	    (if (eq? (peek-token-type) 'catch)
		(set! catch-part (catch)))
	    (if (eq? (peek-token-type) 'finally)
		(set! finally-part (finally)))
	    (instantiate::Try
	       (body body)
	       (catch catch-part)
	       (finally finally-part)))))
   
   (define (catch)
      (consume! 'catch)
      (consume! 'LPAREN)
      (let ((id (consume! 'ID)))
	 (consume! 'RPAREN)
	 (let ((body (block)))
	    ;; not sure, if 'Param' is a really good choice.
	    ;; we'll see...
	    (instantiate::Catch
	       (obj-id (gensym 'catch))
	       (decl (instantiate::Param
			(id id)))
	       (body body)))))
   
   (define (finally)
      (consume! 'finally)
      (block))
   
   (define (labelled-or-expr)
      (let* ((id-token (consume-any!))
	     (next-token-type (peek-token-type)))
	 [assert (id-token) (eq? (token-type id-token) 'ID)]
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
	 (instantiate::Labelled
	    (id id)
	    (body (statement)))))
   
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
	     (instantiate::Fun-Binding
		(lhs (instantiate::Decl (id id)))
		(val (instantiate::Fun
			(params params)
			(body body))))
	     (if id
		 (instantiate::Named-Fun
		    (obj-id (gensym 'named-fun))
		    (decl (instantiate::Decl (id id)))
		    (body (instantiate::Fun
			     (params params)
			     (body body))))
		 (instantiate::Fun
		    (params params)
		    (body body))))))
   
   (define (params)
      (consume! 'LPAREN)
      (if (eq? (peek-token-type) 'RPAREN)
	  (begin
	     (consume-any!)
	     '())
	  (let loop ((rev-params (list (instantiate::Param (id (consume! 'ID))))))
	     (if (eq? (peek-token-type) 'COMMA)
		 (begin
		    (consume-any!)
		    (loop (cons (instantiate::Param (id (consume! 'ID)))
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
		(instantiate::Block
		   (els (reverse! rev-ses))))
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
		    (instantiate::Sequence
		       (els (reverse! rev-exprs))))))))
   
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
      
      (let* ((error-token (peek-token))
	     (expr (cond-expr in-for-init?)))
	 (if (assig-operator? (peek-token-type))
	     (let* ((op (token-type (consume-any!)))      ;; ops are in type-field
		    (rhs (assig-expr in-for-init?)))
		(cond
		   ((and (eq? op '=) (Access? expr))
		    (instantiate::Accsig
		       (lhs expr)
		       (val rhs)))
		   ((and (eq? op '=) (Ref? expr))
		    (instantiate::Vassig
		       (lhs expr)
		       (val rhs)))
		   ((eq? op '=)
		    (my-error "bad assignment" #f error-token))
		   ((Access? expr)
		    (instantiate::Accsig-Op
		       (lhs expr)
		       (op (instantiate::Ref (id (with-out-= op))))
		       (val rhs)))
		   ((Ref? expr)
		    (instantiate::Vassig-Op
		       (lhs expr)
		       (op (instantiate::Ref (id (with-out-= op))))
		       (val rhs)))
		   (else
		    (my-error "bad assignment" #f error-token))))
	     expr)))
   
   (define (cond-expr in-for-init?)
      (let ((expr (binary-expr in-for-init?)))
	 (if (eq? (peek-token-type) '?)
	     (let* ((ignore-? (consume-any!))
		    (then (assig-expr #f))
		    (ignore-colon (consume! ':))
		    (else (assig-expr in-for-init?)))
		(instantiate::Cond
		   (test expr)
		   (then then)
		   (else else)))
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
		       ;; ops are in token-type field
		       (let ((token-op (token-type (consume-any!))))
			  (loop (instantiate::Binary
				   (op (instantiate::Ref (id token-op)))
				   (args (list expr (binary-aux (+fx level 1))))))))
		      (else
		       expr))))))
      (binary-aux 1))
   
   (define (unary)
      (case (peek-token-type)
	 ((delete void typeof ~ ! ++ --)
	  (instantiate::Unary
	     (op (instantiate::Ref (id (token-type (consume-any!)))))
	     (args (list (unary)))))
	 ((+ -)
	  (instantiate::Unary
	     (op (instantiate::Ref
		    (id (symbol-append 'unary- (token-type (consume-any!))))))
	     (args (list (unary)))))
	 (else
	  (postfix))))
   
   (define (postfix)
      (let ((expr (lhs)))
	 (if (not (at-new-line-token?))
	     (case (peek-token-type)
		((++ --)
		 (let ((op (token-type (consume-any!))))
		    (instantiate::Postfix
		       (op (instantiate::Ref (id op)))
		       (args (list expr)))))
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
	     (instantiate::New
		(class class)
		(args args)))
	  (access-or-call (primary) #f)))
   
   (define (access-or-call expr call-allowed?)
      (define (parse-field-name)
	 (cond
	    ((eq? 'ID (peek-token-type)) (consume! 'ID))
	    ((and (config 'liberal-access-field-name)
		  (reserved-word? (peek-token-type)))
	     (token-val (consume-any!)))
	    (else (unexpected-token-error (consume-any!)))))
	 
      (let loop ((expr expr))
	 (case (peek-token-type)
	    ((LBRACKET) (let* ((ignore (consume-any!))
			       (field (expression #f))
			       (ignore-too (consume! 'RBRACKET)))
			   (loop (instantiate::Access
				    (obj expr)
				    (field field)))))
	    ((DOT) (let* ((ignore (consume-any!))
			  (field (parse-field-name))
			  (str-field (string-append "'"
						    (symbol->string field)
						    "'")))
		      (loop (instantiate::Access
			       (obj expr)
			       (field (instantiate::String (val str-field)))))))
	    ((LPAREN) (if call-allowed?
			  (loop (instantiate::Call
				   (op expr)
				   (args (arguments))))
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
		 (instantiate::This (id 'this)))
	 ((ID) (instantiate::Ref (id (consume! 'ID))))
	 ((LPAREN) (let* ((ignore (consume-any!))
			  (expr (expression #f))
			  (ignore-too (consume! 'RPAREN)))
		      expr))
	 ((LBRACKET) (array-literal))
	 ((LBRACE) (object-literal))
	 ((null) (consume-any!)
		 (new-null))
	 ((true false) (instantiate::Bool
			  (val (eq? (token-type (consume-any!)) 'true))))
	 ((NUMBER) (instantiate::Number
		      (val (consume! 'NUMBER)))) ;; still as string!
	 ((STRING) (instantiate::String
		      (val (consume! 'STRING))))
	 ((/ /=) (let ((reg-exp (read-regexp (peek-token-type))))
		    ;; consume-any *must* be after having read the reg-exp,
		    ;; so that the read-regexp works. Only then can we remove
		    ;; the peeked token.
		    (consume-any!) ;; the / or /=
		    (instantiate::Reg-Exp
		       (pattern reg-exp))))
	 (else
	  (unexpected-token-error (peek-token)))))
   
   (define (array-literal)
      ;; basically: every array-element finishes with a ','.
      ;;   however. the very last one can be avoided if the array-el is not an
      ;;   ellision.
      ;; In other words: [a,] and [a] are equivalent. but [,] and [] are not.
      ;; Whenever we find an (non-empty) array-el, we automatically consume the
      ;; ',' (if it exists).
      (consume! 'LBRACKET)
      (let loop ((rev-els '())
		 (length 0))
	 (case (peek-token-type)
	    ((RBRACKET) (consume-any!)
			(instantiate::Array
			   (els (reverse! rev-els))
			   (length length)))
	    ((COMMA) (consume-any!)
		     (loop rev-els (+fx length 1)))
	    (else (let ((array-el (instantiate::Array-Element
				     (index length)
				     (expr (assig-expr #f)))))
		     (unless (eq? (peek-token-type) 'RBRACKET)
			(consume! 'COMMA))
		     (loop (cons array-el rev-els)
			   (+fx length 1)))))))
   
   (define (object-literal)
      (define (property-name)
	 (define (symbol->lexer-string sym)
	    ;; on the fly conversion to string.
	    (string-append "\""
			   (symbol->string sym)
			   "\""))
	    
	 (case (peek-token-type)
	    ((ID) (instantiate::String (val (symbol->lexer-string (consume! #t)))))
	    ((STRING) (instantiate::String (val (consume! #t))))
	    ((NUMBER) (instantiate::Number (val (consume! #t))))
	    (else
	     (if (and (config 'liberal-object-literal-name)
		      (reserved-word? (peek-token-type)))
		 (instantiate::String (val (symbol->lexer-string (consume! #t))))
		 (unexpected-token-error (peek-token))))))
      
      (define (property-init)
	 (let* ((name (property-name))
		(ignore (consume! ':))
		(val (assig-expr #f)))
	    (instantiate::Property-Init
	       (name name)
	       (val val))))
      
      (consume! 'LBRACE)
      (if (eq? (peek-token-type) 'RBRACE)
	  (begin
	     (consume-any!)
	     (instantiate::Obj-Init
		(props '())))
	  (let loop ((rev-props (list (property-init))))
	     (if (eq? (peek-token-type) 'RBRACE)
		 (begin
		    (consume-any!)
		    (instantiate::Obj-Init
		       (props (reverse! rev-props))))
		 (begin
		    (consume! 'COMMA)
		    (loop (cons (property-init) rev-props)))))))
   
   (set! *input-port* port)
   (program))
