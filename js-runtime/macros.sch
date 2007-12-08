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

(define-macro (make-arguments . L)
   ;; TODO
   ''TODO)

(define-macro (js-fun-lambda maybe-this maybe-this-callee arguments formals . Lbody)
   (define *nb-named-params* 3)

   (let* ((named-params (map (lambda (i)
				(string->symbol
				 (string-append
				  "js-par-"
				  (number->string i))))
			     (iota *nb-named-params*)))
	  (par-vec (gensym 'param-vec))
	  (vec-size (gensym 'vec-size))
	  (par-nb (gensym 'param-size))
	  (par-callee (or maybe-this-callee 'this-callee))
	  (params formals)
	  (bindings (let loop ((params params)
			       (named-params named-params)
			       (counter 0)
			       (rev-res '()))
		       (cond
			  ((and (null? params)
				(null? named-params))
			   (reverse! rev-res))
			  ((null? params)
			   (loop params
				 (cdr named-params)
				 counter
				 ;; add binding to avoid empty bindings-list
				 (cons (list (car named-params)
					     (car named-params))
				       rev-res)))
			  ((null? named-params)
			   (loop (cdr params)
				 named-params
				 (+ counter 1)
				 (cons (list (car params)
					     `(if (< ,counter ,vec-size)
						  (vector-ref ,par-vec
							      ,counter)
						  *js-Undefined*))
				       rev-res)))
			  (else
			   (loop (cdr params)
				 (cdr named-params)
				 counter
				 (cons (list (car params)
					     (car named-params))
				       rev-res))))))
	  (param-vars (map car bindings))

	  (this (or maybe-this (gensym 'ignored-this))))
      `(lambda (,this ,par-callee ,par-nb ,@named-params ,par-vec)
	  (let ((,vec-size (-fx ,par-nb ,*nb-named-params*)))
	     (let ,bindings
		,(cond
		    ((pair? arguments)
		     `(let ((,(car arguments) ,par-nb)
			    (,(cadr arguments)
			     (lambda (i)
				(cond
				   ,@(map (lambda (var j)
					     `((=fx i ,j) ,var))
					  named-params
					  (iota *nb-named-params*))
				   (else
				    (vector-ref ,par-vec
						(-fx i ,*nb-named-params*)))))))
			 ,@Lbody))
		    (arguments
		     `(let ((,arguments (make-arguments ,par-callee
							,par-nb
							,param-vars
							,par-vec)))
			 ,@Lbody))
		    (else
		     `(begin ,@Lbody))))))))

(define-macro (js-fun this this-callee arguments formals . Lbody)
   (let ((tmp-f (gensym 'f)))
      `(let* ((,tmp-f (js-fun-lambda ,this ,this-callee ,arguments ,formals ,@Lbody))
	      ;; may fail, as Object can be modified by user
	      (fun-prototype (js-new *js-Object*)))
	  (register-function-object! ,tmp-f ;; lambda
				     ,tmp-f ;; new
				     create-empty-object-lambda
				     fun-prototype ;; prototype
				     ,(length formals) ;; nb args
				     "TODO") ;; text-representation
	  ,tmp-f)))

(define-macro (js-new f . Largs)
   (let ((c (gensym 'class))
	 (f-eval (gensym 'f-eval))
	 (construct (gensym 'construct))
	 (new (gensym 'new))
	 (o (gensym 'o))
	 (o-res (gensym 'o-res)))
      `(let ((,f-eval ,f))
	  (if (not (procedure? ,f-eval))
	      (type-error ,f-eval)
	      (let* ((,(symbol-append c '::Js-Function) (procedure-object ,f-eval))
		     (,construct (Js-Function-construct ,c))
		     (,new (Js-Function-new ,c))
		     (,o (,construct ,c))
		     (,o-res (js-call ,new ,o ,@Largs)))
		 (if (js-object ,o-res)
		     ,o-res
		     ,o))))))

