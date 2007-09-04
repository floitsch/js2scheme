(module nodes
   (include "protobject.sch")
   (option (loadq "protobject-eval.sch"))
   (import protobject)
   (export deprecate!
	   Node
	   Program
	   Begin
	   Block
	   Sequence
	   Var-decl-list
	   Var-ref
	   Decl
	   NOP
	   If
	   Loop
	   For
	   While
	   Do
	   For-in
	   Flow-interruption
	   Bind-exit
	   Bind-exit-invoc
	   Continue
	   Break
	   Return
	   With
	   Switch
	   Fall-through
	   Switch-clause
	   Case
	   Default
	   Throw
	   Try
	   Catch
	   Labelled
	   Fun-binding
	   Named-fun
	   Scope
	   Param
	   This-decl
	   Arguments-decl
	   Fun
	   Assig
	   Vassig
	   Init
	   Vassig-op
	   Accsig
	   Accsig-op
	   Cond
	   Call
	   Method-call
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
	   Array-element
	   Obj-init
	   Property-init
	   Reg-exp
	   Let*))

(define (deprecate! C)
   (define-pmethod (deprecated-use . L)
      (error #f
	     "using deprecated node"
	     (pobject-name C)))
   (set! C.proto.traverse deprecated-use)
   (set! C.proto.traverse! deprecated-use))

(define-macro (debug-print . L)
;   (cons 'print L))
   #unspecified)

(define-macro (proto-traverses class . fields)
   (define (starts-with-? sym)
      (char=? #\? (string-ref (symbol->string sym) 0)))

   (define (without-? sym)
      (let ((symstr (symbol->string sym)))
	 (string->symbol (substring symstr 1 (string-length symstr)))))

   (define (access field)
      (symbol-append 'this. field))
   
   (define (gen-method define-name nb-args method-name modify-pass?)
      (define (gen-args)
	 (map (lambda (n)
		 (string->symbol
		  (string-append "arg"
				 (number->string n))))
	      (iota nb-args)))
		   
      (define (call-node node)
	 `(,(symbol-append node (symbol-append (string->symbol ".") method-name))
	   ,@(gen-args)))

      (define (run-over-list field-list)
	 (if modify-pass?
	     (let ((loop (gensym 'loop))
		   (node (gensym 'node))
		   (flist (gensym 'flist)))
		`(let ,loop ((,flist ,(access field-list)))
		      (unless (null? ,flist)
			 (let ((,node (car ,flist)))
			    (set-car! ,flist ,(call-node node))
			    (,loop (cdr ,flist))))))
	     (let ((node (gensym 'node)))
		`(for-each (lambda (,node)
			      ,(call-node node))
			   ,(access field-list)))))
							   
      (define (visit field)
	 (if modify-pass?
	     `(set! ,(access field) ,(call-node (access field)))
	     (call-node (access field))))

      (define (traverse-field field)
	 (cond
	    ((and (pair? field) (starts-with-? (car field)))
	     (let ((field-w/o-? (without-? (car field))))
		`(and ,(access field-w/o-?)
		      ,(run-over-list field-w/o-?))))
	    ((pair? field)
	     (run-over-list (car field)))
	    ((starts-with-? field)
	     (let ((field-w/o-? (without-? field)))
		`(and ,(access field-w/o-?)
		      ,(visit field-w/o-?))))
	    (else
	     (visit field))))
      
      (define (map-fields fields)
	 (map (lambda (field)
		 `(begin
		     (debug-print ',field)
		     ,(traverse-field field)))
	      fields))
      
      `(define-pmethod (,define-name ,@(gen-args))
	  (debug-print (pobject-name this))
	  ,@(cond
	       (modify-pass?
		(append! (map-fields fields) '(this)))
	       ((null? fields)
		'(this.default-traverse-value))
	       (else
		(map-fields fields)))))
      
   (define (gen-traverse nb-args)
      (let* ((method-name (symbol-append 'traverse (string->symbol
						    (number->string nb-args))))
	     (define-name (symbol-append class '-proto- method-name))
	     ;; always just call 'traverse. up to the user to modify it.
	     (method-definition (gen-method define-name nb-args 'traverse #f))
	     (method-name! (symbol-append 'traverse
					  (string->symbol
					   (number->string nb-args))
					  '!))
	     (define-name! (symbol-append class '-proto- method-name!))
	     ;; always just call 'traverse!. up to the user to modify it.
	     (method-definition! (gen-method define-name! nb-args 'traverse! #t)))
	 `(begin
	     ,method-definition
	     ,method-definition!
	     (set! ,(symbol-append class '.proto. method-name) ,define-name)
	     (set! ,(symbol-append class '.proto. method-name!)
		   ,define-name!))))
   `(begin
       ,@(map gen-traverse (iota 3))))

(define-pclass (Node))
(proto-traverses Node)

(define-pclass (Var-ref id)
   (set! this.id id))
(set! Var-ref.proto (empty-pobject Node))
(proto-traverses Var-ref)

(define-pclass (Decl id)
   (set! this.id id))
(set! Decl.proto (empty-pobject Var-ref))
(proto-traverses Decl)

(define-pclass (Param id)
   (set! this.id id))
(set! Param.proto (empty-pobject Decl))
(proto-traverses Param)

(define-pclass (This-decl)
   (set! this.id 'this))
(set! This-decl.proto (empty-pobject Param))
(proto-traverses This-decl)

(define-pclass (Arguments-decl)
   (set! this.id 'arguments))
(set! Arguments-decl.proto (empty-pobject Param))
(proto-traverses Arguments-decl)

(define-pclass (Scope body)
   (set! this.body body))
(set! Scope.proto (empty-pobject Node))

(define-pclass (Program body)
   (set! this.this-decl (new This-decl))
   (set! this.imported '())
   (set! this.runtime '())
   (set! this.implicit-globals '())
   (set! this.body body))
(set! Program.proto (empty-pobject Scope))
(proto-traverses Program this-decl (imported) (runtime) (implicit-globals) body)

(define-pclass (Begin)) ;; els
(set! Begin.proto (empty-pobject Node))
(proto-traverses Begin) ;; should not be necessary

(define-pclass (Block els)
   (set! this.els els))
(set! Block.proto (empty-pobject Begin))
(proto-traverses Block (els))

(define-pclass (Sequence els)
   (set! this.els els))
(set! Sequence.proto (empty-pobject Begin))
(proto-traverses Sequence (els))

(define-pclass (Var-decl-list var-decls)
   (set! this.els var-decls))
(set! Var-decl-list.proto (empty-pobject Begin))
(proto-traverses Var-decl-list (els))

(define-pclass (NOP))
(set! NOP.proto (empty-pobject Node))
(proto-traverses NOP)

(define-pclass (If test then else)
   (set! this.test test)
   (set! this.then then)
   (set! this.else else))
(set! If.proto (empty-pobject Node))
(proto-traverses If test then else)

(define-pclass (Loop))
(set! Loop.proto (empty-pobject Node))
(proto-traverses Loop) ;; should not be necessary

(define-pclass (For init test incr body)
   (set! this.init init)
   (set! this.test test)
   (set! this.incr incr)
   (set! this.body body))
(set! For.proto (empty-pobject Loop))
(proto-traverses For ?init ?test ?incr body)

(define-pclass (While test body)
   (set! this.test test)
   (set! this.body body))
(set! While.proto (empty-pobject Loop))
(proto-traverses While test body)

(define-pclass (Do body test)
   (set! this.body body)
   (set! this.test test))
(set! Do.proto (empty-pobject Loop))
(proto-traverses Do body test)

(define-pclass (For-in lhs obj body)
   (set! this.lhs lhs)
   (set! this.obj obj)
   (set! this.body body))
(set! For-in.proto (empty-pobject Loop))
(proto-traverses For-in lhs obj body)

(define-pclass (Flow-interruption))
(set! Flow-interruption.proto (empty-pobject Node))
(proto-traverses Flow-interruption) ;; should not be needed

(define-pclass (Bind-exit label body)
   (set! this.label label)
   (set! this.body body))
(set! Bind-exit.proto (empty-pobject Node))
(proto-traverses Bind-exit body)

(define-pclass (Bind-exit-invoc label expr)
   (set! this.label label)
   (set! this.expr expr))
(set! Bind-exit-invoc.proto (empty-pobject Flow-interruption))
(proto-traverses Bind-exit-invoc expr)
   
(define-pclass (Continue id)
   (set! this.id id))
(set! Continue.proto (empty-pobject Node))
(proto-traverses Continue)

(define-pclass (Break id)
   (set! this.id id))
(set! Break.proto (empty-pobject Node))
(proto-traverses Break)

(define-pclass (Return expr)
   (set! this.expr expr))
(set! Return.proto (empty-pobject Node))
(proto-traverses Return expr)

(define-pclass (With obj body)
   (set! this.obj obj)
   (set! this.intercepted '())
   (set! this.body body))
(set! With.proto (empty-pobject Node))
(proto-traverses With obj (intercepted) body)

(define-pclass (Switch key cases)
   (set! this.key key)
   (set! this.cases cases))
(set! Switch.proto (empty-pobject Node))
(proto-traverses Switch key (cases))

(define-pclass (Fall-through))
(set! Fall-through.proto (empty-pobject Node))
(proto-traverses Fall-through)

(define-pclass (Switch-clause) ; body
   'do-nothing)
(set! Switch-clause.proto (empty-pobject Node))
(proto-traverses Switch-clause) ;; should never be necessary
   
(define-pclass (Case expr body)
   (set! this.expr expr)
   (set! this.body body))
(set! Case.proto (empty-pobject Switch-clause))
(proto-traverses Case expr body)

(define-pclass (Default body)
   (set! this.body body))
(set! Default.proto (empty-pobject Switch-clause))
(proto-traverses Default body)

(define-pclass (Throw expr)
   (set! this.expr expr))
(set! Throw.proto (empty-pobject Flow-interruption))
(proto-traverses Throw expr)

(define-pclass (Try body catch finally)
   (set! this.body body)
   (set! this.catch catch)
   (set! this.finally finally))
(set! Try.proto (empty-pobject Node))
(proto-traverses Try body ?catch ?finally)

(define-pclass (Catch exception body)
   (set! this.exception exception)
   (set! this.body body))
(set! Catch.proto (empty-pobject Node))
(proto-traverses Catch exception body)

(define-pclass (Labelled id body)
   (set! this.id id)
   (set! this.body body))
(set! Labelled.proto (empty-pobject Node))
(proto-traverses Labelled body)

(define-pclass (Assig lhs val)) ;; lhs val
(set! Assig.proto (empty-pobject Node))
(proto-traverses Assig) ;; should not be necessary

(define-pclass (Vassig lhs val)
   (set! this.lhs lhs)
   (set! this.val val))
(set! Vassig.proto (empty-pobject Assig))
(proto-traverses Vassig lhs val)

(define-pclass (Vassig-op lhs op val)
   (set! this.lhs lhs)
   (set! this.op op)
   (set! this.val val))
(set! Vassig-op.proto (empty-pobject Vassig))
(proto-traverses Vassig-op lhs op val)

(define-pclass (Init decl val)
   (set! this.lhs decl)
   (set! this.val val))
(set! Init.proto (empty-pobject Vassig))
(proto-traverses Init lhs val)

(define-pclass (Accsig lhs val)
   (set! this.lhs lhs)
   (set! this.val val))
(set! Accsig.proto (empty-pobject Assig))
(proto-traverses Accsig lhs val)

(define-pclass (Accsig-op lhs op val)
   (set! this.lhs lhs)
   (set! this.op op)
   (set! this.val val))
(set! Accsig-op.proto (empty-pobject Accsig))
(proto-traverses Accsig-op lhs op val)

(define-pclass (Fun-binding decl fun)
   (set! this.lhs decl)
   (set! this.val fun))
(set! Fun-binding.proto (empty-pobject Vassig))
(proto-traverses Fun-binding lhs val)

;; will be usually replaced by a Vassig after symbol-pass
(define-pclass (Named-fun decl fun)
   (set! this.decl decl)
   (set! this.fun fun))
(set! Named-fun.proto (empty-pobject Scope))
(proto-traverses Named-fun decl fun)

(define-pclass (Fun params body)
   (set! this.params params)
   (set! this.this-decl (new This-decl))
   (set! this.arguments-decl (new Arguments-decl))
   (set! this.body body))
(set! Fun.proto (empty-pobject Scope))
(proto-traverses Fun (params) ?this-decl ?arguments-decl body)

(define-pclass (Cond test then else)
   (set! this.test test)
   (set! this.then then)
   (set! this.else else))
(set! Cond.proto (empty-pobject If))
(proto-traverses Cond test then else)

(define-pclass (Call op args)
   (set! this.op op)
   (set! this.args args))
(set! Call.proto (empty-pobject Node))
(proto-traverses Call op (args))

(define-pclass (Method-call op args)
   (set! this.op op)
   (set! this.args args))
(set! Method-call.proto (empty-pobject Call))
(proto-traverses Method-call op (args))

(define-pclass (Binary left op right)
   (set! this.op op)
   (set! this.args (list left right)))
(set! Binary.proto (empty-pobject Call))
(proto-traverses Binary op (args))

(define-pclass (Unary op right)
   (set! this.op op)
   (set! this.args (list right)))
(set! Unary.proto (empty-pobject Call))
(proto-traverses Unary op (args))

(define-pclass (Postfix expr op)
   (set! this.postfix? #t)
   (set! this.op op)
   (set! this.args (list expr)))
(set! Postfix.proto (empty-pobject Call))
(proto-traverses Postfix op (args))

(define-pclass (New class args)
   (set! this.class class)
   (set! this.args args))
(set! New.proto (empty-pobject Node))
(proto-traverses New class (args))

(define-pclass (Access obj field)
   (set! this.obj obj)
   (set! this.field field))
(set! Access.proto (empty-pobject Node))
(proto-traverses Access obj field)

(define-pclass (Dot obj field)
   (set! this.obj obj)
   (set! this.field (new String (symbol->string field))))
(set! Dot.proto (empty-pobject Access))
(proto-traverses Dot obj field)

;; we consider This to be a var-ref and not to be
;; literal.
(define-pclass (This)
   (set! this.id 'this))
(set! This.proto (empty-pobject Var-ref))
(proto-traverses This)

(define-pclass (Literal val)
   (set! this.val val))
(set! Literal.proto (empty-pobject Node))
(proto-traverses Literal)

(define-pclass (Undefined)
   (set! this.val 'undefined))
(set! Undefined.proto (empty-pobject Literal))
(proto-traverses Undefined)
(define-pclass (Null)
   (set! this.val 'null))
(set! Null.proto (empty-pobject Literal))
(proto-traverses Null)
(define-pclass (Bool val)
   (set! this.val val))
(set! Bool.proto (empty-pobject Literal))
(proto-traverses Bool)
(define-pclass (Number val)
   (set! this.val val))
(set! Number.proto (empty-pobject Literal))
(proto-traverses Number)
(define-pclass (String val)
   (set! this.val val))
(set! String.proto (empty-pobject Literal))
(proto-traverses String)

(define-pclass (Array els length)
   (set! this.els els)
   (set! this.length length))
(set! Array.proto (empty-pobject Node))
(proto-traverses Array (els))

(define-pclass (Array-element index expr)
   (set! this.index index)
   (set! this.expr expr))
(set! Array-element.proto (empty-pobject Node))
(proto-traverses Array-element expr)

(define-pclass (Obj-init props)
   (set! this.props props))
(set! Obj-init.proto (empty-pobject Node))
(proto-traverses Obj-init (props))

(define-pclass (Property-init name val)
   (set! this.name (if (symbol? name)
		       (new String (symbol->string name))
		       name))
   (set! this.val val))
(set! Property-init.proto (empty-pobject Node))
(proto-traverses Property-init name val)

(define-pclass (Reg-exp pattern)
   (set! this.pattern pattern))
(set! Reg-exp.proto (empty-pobject Node))
(proto-traverses Reg-exp)

;;
;; Scheme nodes
;;

(define-pclass (Let* vassigs body)
   (set! this.vassigs vassigs)
   (set! this.body body))
(set! Let*.proto (empty-pobject Node))
(proto-traverses Let* (vassigs) body)
