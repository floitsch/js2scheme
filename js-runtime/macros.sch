(define-macro (js-call f this . Largs)
   (define *nb-named-params* 3)
   (let* ((fun (gensym 'fun))
	  (nb-params (length Largs))
	  (arg-bindings (map (lambda (arg)
				(list (gensym 'arg) arg))
			     Largs))
	  ;; we need at least *nb-named-params* to make the code simpler
	  (arg-bindings-good-nb (if (>= nb-params *nb-named-params*)
				    arg-bindings
				    (append! arg-bindings
					     (map (lambda (i)
						     (list (gensym 'ignored)
							   '(js-undefined)))
						  (iota (- *nb-named-params*
							   nb-params))))))
	  (named-params (map! car (take arg-bindings-good-nb
					*nb-named-params*)))
	  (rest-vec (list->vector (map (lambda (p)
					  (list 'unquote (car p)))
				       (drop arg-bindings-good-nb
					     *nb-named-params*)))))
      `(let ((,fun ,f))
	  (let* ,arg-bindings-good-nb
	     (,fun ,(or this '*js-global-this*)
		   ,fun
		   ,nb-params
		   ,@named-params
		   ,(list 'quasiquote rest-vec))))))

(define-macro (js-method-call o m . Largs)
   (if (and (symbol? o)
	    (string? m))
       (let ((tmp-o (gensym 'o))
	     (tmp-this (gensym 'this)))
	  `(let* ((,tmp-this (any->object ,o))
		  (,tmp-o (safe-js-object ,tmp-this)))
	      (js-call (js-property-get ,tmp-o ,m)
		       ,tmp-this
		       ,@Largs)))
       (let ((tmp-o (gensym 'o))
	     (tmp-field (gensym 'field))
	     (tmp-object-o (gensym 'object-o))
	     (tmp-object-this (gensym 'this))
	     (tmp-string-field (gensym 'string-field)))
	  ;; we need all these tmp-variables, to ensure the correct order of
	  ;; evaluation.
	  `(let* ((,tmp-o ,o)
		  (,tmp-field ,m)
		  (,tmp-object-this (any->object ,tmp-o))
		  (,tmp-object-o (safe-js-object ,tmp-object-this))
		  (,tmp-string-field (any->string ,tmp-field)))
	      (js-call (js-property-get ,tmp-object-o
					     ,tmp-string-field)
		       ,tmp-object-this
		       ,@Largs)))))

(define-macro (js-fun-lambda maybe-this
			     maybe-this-callee
			     arguments
			     formals
			     . Lbody)
   (define *nb-named-params* 3)

   ;; rename formals that are shadowed.
   ;; last one wins.
   (define (unique-formals ids)
      (cond
	 ((null? ids)
	  '())
	 ((null? (cdr ids))
	  ids)
	 ((memq (car ids) (cdr ids))
	  (cons (gensym 'shadowed) (unique-formals (cdr ids))))
	 (else
	  (cons (car ids)
		(unique-formals (cdr ids))))))

   ;; get *nb-named-params* ids to be put as formals in the lambda.
   ;; if there aren't enough, create 'ignored vars.
   (define (named-params ids)
      (let loop ((ids ids)
		 (nb 0))
	 (cond
	    ((= nb *nb-named-params*)
	     '())
	    ((null? ids)
	     (cons (gensym 'ignored) (loop ids (+ nb 1))))
	    (else
	     (cons (car ids) (loop (cdr ids) (+ nb 1)))))))

   ;; there are only *nb-named-params* parameters directly given as parameters.
   ;; the remaining ones need to be bound inside a 'let'.
   ;; vec-bindings generates the bindings. (if any).
   (define (vec-bindings ids par-vec vec-size)
      (cond
	 ((<= (length ids) *nb-named-params*)
	  '())
	 (else
	  (map (lambda (id counter)
		  `(,id (if (< ,counter ,vec-size)
			    (vector-ref ,par-vec
					,counter)
			    (js-undefined))))
	       (drop ids *nb-named-params*)
	       (iota (- (length ids) *nb-named-params*))))))

   (let* ((par-vec (gensym 'param-vec))
	  (vec-size (gensym 'vec-size))
	  (par-nb (gensym 'param-size))
	  (par-callee (or maybe-this-callee 'this-callee))
	  (this (or maybe-this (gensym 'ignored-this)))
	  
	  (u-formals (unique-formals formals))

	  (params (named-params u-formals))
	  (bindings (vec-bindings u-formals par-vec vec-size)))
      
      `(lambda (,this ,par-callee ,par-nb ,@params ,par-vec)
	  (let* ((,vec-size (-fx ,par-nb ,*nb-named-params*))
		 ,@bindings)
	     ,(cond
		 ;; shortcut for accessing var-args.
		 ;; (used in runtime).
		 ((pair? arguments)
		  `(let ((,(car arguments) ,par-nb)
			 (,(cadr arguments)
			  (lambda (i)
			     (cond
				,@(map (lambda (var j)
					  `((=fx i ,j) ,var))
				       params
				       (iota *nb-named-params*))
				(else
				 (vector-ref ,par-vec
					     (-fx i ,*nb-named-params*)))))))
		      ,@Lbody))
		 (arguments
		  `(let ((,arguments (make-arguments ,*nb-named-params*
						     ,par-callee
						     ,par-nb
						     ,params
						     ,par-vec)))
		      ,@Lbody))
		 (else
		  `(begin ,@Lbody)))))))

(define-macro (js-fun this this-callee arguments text-repr formals . Lbody)
   (let ((f (gensym 'f)))
      `(let ((,f (js-fun-lambda ,this ,this-callee ,arguments ,formals
				,@Lbody)))
	  (create-function ,f
			   ,(length formals)
			   ,text-repr)
	  ,f)))

(define-macro (js-new f . Largs)
   (let ((c (gensym 'class))
	 (f-eval (gensym 'f-eval))
	 (construct (gensym 'construct))
	 (new (gensym 'new))
	 (o (gensym 'o))
	 (o-res (gensym 'o-res)))
      `(let ((,f-eval ,f))
	  (if (not (procedure? ,f-eval))
	      (type-error "not a procedure" ,f-eval)
	      (let* ((,(symbol-append c '::Js-Function)
		      (procedure-object ,f-eval))
		     (,construct (Js-Function-construct ,c))
		     (,new (Js-Function-new ,c))
		     (,o (,construct ,c))
		     (,o-res (js-call ,new ,o ,@Largs)))
		 (if (js-object ,o-res)
		     ,o-res
		     ,o))))))

;; should be in Arguments.scm but I can't yet export macros to eval.
;; 10.1.8
(define-macro (make-arguments nb-named-params
			      callee nb-args param-vars par-vec)
   (let ((arguments (gensym 'arguments))
	 (counter (gensym 'counter))
	 (new-val (gensym 'new-val))
	 (nb-named (length param-vars)))
      `(let ((,arguments (instantiate::Js-Arguments
			    (props (make-props-hashtable))
			    (proto (js-object-prototype)))))
	  (js-property-generic-set! ,arguments
				    "callee"
				    ,callee
				    (get-Attributes dont-enum))
	  (js-property-generic-set! ,arguments
				    "length"
				    (fixnum->flonum ,nb-args)
				    (get-Attributes dont-enum))
	  ;; named vars are added as scope-vars
	  ,@(map (lambda (id c)
		    `(when (< ,c ,nb-args)
			(scope-var-add ,arguments
				       ,(number->string c)
				       ,id
				       (get-Attributes dont-enum))))
		 param-vars
		 (iota nb-named))
	  ;; remaining ones are added as vector-refs
	  (for-each (lambda (,counter)
		       (js-property-generic-set!
			,arguments
			(number->string ,counter)
			(instantiate::Ref
			   (getter (lambda ()
				      (vector-ref ,par-vec
						  (- ,counter
						     ,nb-named-params))))
			   (setter (lambda (,new-val)
				      (vector-set! ,par-vec
						   (- ,counter
						      ,nb-named-params)
						   ,new-val))))
			(get-Attributes dont-enum)))
		    (iota (- ,nb-args ,nb-named) ,nb-named))
	  ,arguments)))

;; should be in scope-object.scm, but I can't yet export macros to eval.
(define-macro (scope-var-add scope-object
			     id v attributes)
   (let ((str-id (gensym 'str-id))
	 (ref (gensym 'ref))
	 (new-val (gensym 'new-val)))
      `(let ((,str-id (if (symbol? ,id) (symbol->string ,id) ,id))
	     (,ref (instantiate::Ref
		      (getter (lambda () ,v))
		      (setter (lambda (,new-val) (set! ,v ,new-val))))))
	  (js-property-generic-set! ,scope-object
				    ,str-id
				    ,ref
				    ,attributes))))

(define-macro (jsop-&& e1 e2)
   `(and (any->bool ,e1) ,e2))

(define-macro (jsop-OR e1 e2)
   (let ((tmp (gensym 'tmp)))
      `(let ((,tmp ,e1))
	  (if (any->bool ,tmp)
	      ,tmp
	      ,e2))))
