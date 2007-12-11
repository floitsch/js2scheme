(module nodes
   (include "protobject.sch")
   (option (loadq "protobject-eval.sch"))
   (import protobject)
   (export (node n::symbol)
	   (nodes-init!)))


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
       ,@(map gen-traverse (iota 4))))


(define (node n)
   (hashtable-get (thread-parameter '*nodes*) n))

;; === nodes-init =====
(define (nodes-init!)
(define nodes (make-hashtable))
(thread-parameter-set! '*nodes* nodes)

(define-macro (define-node signature . Lrest)
   `(begin
       (define-pclass ,signature ,@Lrest)
       (hashtable-put! nodes ',(car signature) ,(car signature))))

(define-node (Node))
(proto-traverses Node)

(define-node (Var-ref id)
   (set! this.id id))
(set! Var-ref.proto (empty-pobject Node))
(proto-traverses Var-ref)

(define-node (Decl id)
   (set! this.id id))
(set! Decl.proto (empty-pobject Var-ref))
(proto-traverses Decl)

(define-node (Param id)
   (set! this.id id))
(set! Param.proto (empty-pobject Decl))
(proto-traverses Param)

(define-node (This-decl)
   (set! this.id 'this))
(set! This-decl.proto (empty-pobject Param))
(proto-traverses This-decl)

(define-node (Arguments-decl)
   (set! this.id 'arguments))
(set! Arguments-decl.proto (empty-pobject Param))
(proto-traverses Arguments-decl)

(define-node (Scope body)
   (set! this.body body))
(set! Scope.proto (empty-pobject Node))

(define-node (Program body)
   (set! this.this-decl (new This-decl))
   (set! this.imported '())
   (set! this.runtime '())
   (set! this.implicit-globals '())
   (set! this.body body))
(set! Program.proto (empty-pobject Scope))
(proto-traverses Program this-decl (imported) (runtime) (implicit-globals) body)

(define-node (Begin)) ;; els
(set! Begin.proto (empty-pobject Node))
(proto-traverses Begin) ;; should not be necessary

(define-node (Block els)
   (set! this.els els))
(set! Block.proto (empty-pobject Begin))
(proto-traverses Block (els))

(define-node (Sequence els)
   (set! this.els els))
(set! Sequence.proto (empty-pobject Begin))
(proto-traverses Sequence (els))

(define-node (Var-decl-list var-decls)
   (set! this.els var-decls))
(set! Var-decl-list.proto (empty-pobject Begin))
(proto-traverses Var-decl-list (els))

(define-node (NOP))
(set! NOP.proto (empty-pobject Node))
(proto-traverses NOP)

(define-node (If test then else)
   (set! this.test test)
   (set! this.then then)
   (set! this.else else))
(set! If.proto (empty-pobject Node))
(proto-traverses If test then else)

(define-node (Loop))
(set! Loop.proto (empty-pobject Node))
(proto-traverses Loop) ;; should not be necessary

(define-node (For init test incr body)
   (set! this.init init)
   (set! this.test test)
   (set! this.incr incr)
   (set! this.body body))
(set! For.proto (empty-pobject Loop))
(proto-traverses For ?init ?test ?incr body)

(define-node (While test body)
   (set! this.test test)
   (set! this.body body))
(set! While.proto (empty-pobject Loop))
(proto-traverses While test body)

(define-node (Do body test)
   (set! this.body body)
   (set! this.test test))
(set! Do.proto (empty-pobject Loop))
(proto-traverses Do body test)

(define-node (For-in lhs obj body)
   (set! this.lhs lhs)
   (set! this.obj obj)
   (set! this.body body))
(set! For-in.proto (empty-pobject Loop))
(proto-traverses For-in lhs obj body)

(define-node (Flow-interruption))
(set! Flow-interruption.proto (empty-pobject Node))
(proto-traverses Flow-interruption) ;; should not be needed

(define-node (Bind-exit label body)
   (set! this.label label)
   (set! this.body body))
(set! Bind-exit.proto (empty-pobject Node))
(proto-traverses Bind-exit body)

(define-node (Bind-exit-invoc label expr)
   (set! this.label label)
   (set! this.expr expr))
(set! Bind-exit-invoc.proto (empty-pobject Flow-interruption))
(proto-traverses Bind-exit-invoc expr)
   
(define-node (Continue id)
   (set! this.id id))
(set! Continue.proto (empty-pobject Node))
(proto-traverses Continue)

(define-node (Break id)
   (set! this.id id))
(set! Break.proto (empty-pobject Node))
(proto-traverses Break)

(define-node (Return expr)
   (set! this.expr expr))
(set! Return.proto (empty-pobject Node))
(proto-traverses Return expr)

(define-node (With obj body)
   (set! this.obj obj)
   (set! this.intercepted '())
   (set! this.body body))
(set! With.proto (empty-pobject Node))
(proto-traverses With obj (intercepted) body)

(define-node (Obj-init props)
   (set! this.props props))
(set! Obj-init.proto (empty-pobject Node))
(proto-traverses Obj-init (props))

;; From a symbol-resolution point of view Catch und Named-fun both are nearly
;; equivalent to Withs. In addition to pushing an object onto the stack, they
;; declare however a new variable. The new variable has to be inside the
;; object. For efficiency reasons (especially for Named-fun) a Scope object is
;; used. The original obj will be the prototype of the Scope-object. This is
;; possible as neither the Named-Fun nor the exception can be
;; deleted. (Otherwise a delete would remove the scope-object element and then
;; 'show' the object-element.).
(define-node (Decl-With decl body)
   (set! this.decl decl)
   ;; this.obj will be replaced in expand1.
   (set! this.obj (new Obj-init '()))
   (set! this.intercepted '())
   (set! this.body body))
(set! Decl-With.proto (empty-pobject With))
(proto-traverses Decl-With decl obj (intercepted) body)

(define-node (Switch key cases)
   (set! this.key key)
   (set! this.cases cases))
(set! Switch.proto (empty-pobject Node))
(proto-traverses Switch key (cases))

(define-node (Fall-through))
(set! Fall-through.proto (empty-pobject Node))
(proto-traverses Fall-through)

(define-node (Switch-clause) ; body
   'do-nothing)
(set! Switch-clause.proto (empty-pobject Node))
(proto-traverses Switch-clause) ;; should never be necessary
   
(define-node (Case expr body)
   (set! this.expr expr)
   (set! this.body body))
(set! Case.proto (empty-pobject Switch-clause))
(proto-traverses Case expr body)

(define-node (Default body)
   (set! this.body body))
(set! Default.proto (empty-pobject Switch-clause))
(proto-traverses Default body)

(define-node (Throw expr)
   (set! this.expr expr))
(set! Throw.proto (empty-pobject Flow-interruption))
(proto-traverses Throw expr)

(define-node (Try body catch finally)
   (set! this.body body)
   (set! this.catch catch)
   (set! this.finally finally))
(set! Try.proto (empty-pobject Node))
(proto-traverses Try body ?catch ?finally)

(define-node (Catch exception body)
   (set! this.decl exception)
   (set! this.obj (new Obj-init '()))
   (set! this.intercepted '())
   (set! this.body body))
(set! Catch.proto (empty-pobject Decl-With))
(proto-traverses Catch decl obj (intercepted) body)

(define-node (Labelled id body)
   (set! this.id id)
   (set! this.body body))
(set! Labelled.proto (empty-pobject Node))
(proto-traverses Labelled body)

(define-node (Assig lhs val)) ;; lhs val
(set! Assig.proto (empty-pobject Node))
(proto-traverses Assig) ;; should not be necessary

(define-node (Vassig lhs val)
   (set! this.lhs lhs)
   (set! this.val val))
(set! Vassig.proto (empty-pobject Assig))
(proto-traverses Vassig lhs val)

(define-node (Vassig-op lhs op val)
   (set! this.lhs lhs)
   (set! this.op op)
   (set! this.val val))
(set! Vassig-op.proto (empty-pobject Vassig))
(proto-traverses Vassig-op lhs op val)

(define-node (Init decl val)
   (set! this.lhs decl)
   (set! this.val val))
(set! Init.proto (empty-pobject Vassig))
(proto-traverses Init lhs val)

(define-node (Accsig lhs val)
   (set! this.lhs lhs)
   (set! this.val val))
(set! Accsig.proto (empty-pobject Assig))
(proto-traverses Accsig lhs val)

(define-node (Accsig-op lhs op val)
   (set! this.lhs lhs)
   (set! this.op op)
   (set! this.val val))
(set! Accsig-op.proto (empty-pobject Accsig))
(proto-traverses Accsig-op lhs op val)

(define-node (Fun-binding decl fun)
   (set! this.lhs decl)
   (set! this.val fun))
(set! Fun-binding.proto (empty-pobject Vassig))
(proto-traverses Fun-binding lhs val)

(define-node (Named-fun decl fun)
   (set! this.obj (new Obj-init '()))
   (set! this.intercepted '())
   (set! this.decl decl)
   (set! this.body fun))
(set! Named-fun.proto (empty-pobject Decl-With))
(proto-traverses Named-fun decl obj (intercepted) body)

(define-node (Fun params body)
   (set! this.params params)
   (set! this.this-decl (new This-decl))
   (set! this.arguments-decl (new Arguments-decl))
   (set! this.body body))
(set! Fun.proto (empty-pobject Scope))
(proto-traverses Fun this-decl arguments-decl (params) body)

(define-node (Cond test then else)
   (set! this.test test)
   (set! this.then then)
   (set! this.else else))
(set! Cond.proto (empty-pobject If))
(proto-traverses Cond test then else)

(define-node (Call op args)
   (set! this.op op)
   (set! this.args args))
(set! Call.proto (empty-pobject Node))
(proto-traverses Call op (args))

(define-node (Eval-call op eval-scm-id
			args top-level-object env-vars)
   (set! this.op op)
   (set! this.eval-scm-id eval-scm-id)
   (set! this.args args)
   (set! this.top-level-object top-level-object)
   (set! this.env-vars env-vars))
(set! Eval-call.proto (empty-pobject Call))
(proto-traverses Eval-call op (args))

(define-node (Method-call op args)
   (set! this.op op)
   (set! this.args args))
(set! Method-call.proto (empty-pobject Call))
(proto-traverses Method-call op (args))

(define-node (Binary left op right)
   (set! this.op op)
   (set! this.args (list left right)))
(set! Binary.proto (empty-pobject Call))
(proto-traverses Binary op (args))

(define-node (Unary op right)
   (set! this.op op)
   (set! this.args (list right)))
(set! Unary.proto (empty-pobject Call))
(proto-traverses Unary op (args))

(define-node (Postfix expr op)
   (set! this.postfix? #t)
   (set! this.op op)
   (set! this.args (list expr)))
(set! Postfix.proto (empty-pobject Call))
(proto-traverses Postfix op (args))

(define-node (Delete-property-call op obj prop)
   (set! this.op op)
   (set! this.args (list obj prop)))
(set! Delete-property-call.proto (empty-pobject Binary))
(proto-traverses Delete-property-call op (args))

(define-node (Delete-call op v)
   (set! this.op op)
   (set! this.args (list v)))
(set! Delete-call.proto (empty-pobject Unary))
(proto-traverses Delete-call op (args))

(define-node (New class args)
   (set! this.class class)
   (set! this.args args))
(set! New.proto (empty-pobject Node))
(proto-traverses New class (args))

(define-node (Access obj field)
   (set! this.obj obj)
   (set! this.field field))
(set! Access.proto (empty-pobject Node))
(proto-traverses Access obj field)

(define-node (Dot obj field)
   (set! this.obj obj)
   (set! this.field (new String (symbol->string field))))
(set! Dot.proto (empty-pobject Access))
(proto-traverses Dot obj field)

;; we consider This to be a var-ref and not to be
;; literal.
(define-node (This)
   (set! this.id 'this))
(set! This.proto (empty-pobject Var-ref))
(proto-traverses This)

(define-node (Literal val)
   (set! this.val val))
(set! Literal.proto (empty-pobject Node))
(proto-traverses Literal)

(define-node (Undefined)
   (set! this.val 'undefined))
(set! Undefined.proto (empty-pobject Literal))
(proto-traverses Undefined)
(define-node (Null)
   (set! this.val 'null))
(set! Null.proto (empty-pobject Literal))
(proto-traverses Null)
(define-node (Bool val)
   (set! this.val val))
(set! Bool.proto (empty-pobject Literal))
(proto-traverses Bool)
(define-node (Number val)
   (set! this.val val))
(set! Number.proto (empty-pobject Literal))
(proto-traverses Number)
(define-node (String val)
   (set! this.val val))
(set! String.proto (empty-pobject Literal))
(proto-traverses String)

(define-node (Array els length)
   (set! this.els els)
   (set! this.length length))
(set! Array.proto (empty-pobject Node))
(proto-traverses Array (els))

(define-node (Array-element index expr)
   (set! this.index index)
   (set! this.expr expr))
(set! Array-element.proto (empty-pobject Node))
(proto-traverses Array-element expr)

(define-node (Property-init name val)
   (set! this.name (if (symbol? name)
		       (new String (symbol->string name))
		       name))
   (set! this.val val))
(set! Property-init.proto (empty-pobject Node))
(proto-traverses Property-init name val)

(define-node (Reg-exp pattern)
   (set! this.pattern pattern))
(set! Reg-exp.proto (empty-pobject Node))
(proto-traverses Reg-exp)

;;
;; Scheme nodes
;;

(define-node (Let* vassigs body)
   (set! this.vassigs vassigs)
   (set! this.body body))
(set! Let*.proto (empty-pobject Node))
(proto-traverses Let* (vassigs) body)
)
