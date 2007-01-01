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
							   '*js-Undefined*))
						  (iota (- *nb-named-params*
							   nb-params))))))
	  (named-params (map! car (take arg-bindings-good-nb
					*nb-named-params*)))
	  (rest-vec (list->vector (map (lambda (p)
					  (list 'unquote (car p)))
				       (drop arg-bindings-good-nb
					     *nb-named-params*)))))
      `(let ((,fun ,f))
	  (let* ,arg-bindings
	     (,fun ,(or this '*global-this*)
		   ,fun
		   ,nb-params
		   ,@named-params
		   ,(list 'quasiquote rest-vec))))))

(define-macro (js-fun formals body)
   (define *nb-named-params* 3)
   (let* ((named-params (map (lambda (i)
				(string->symbol
				 (string-append
				  "par-"
				  (number->string i))))
			     (iota *nb-named-params*)))
	  (par-vec 'par-vec)
	  (vec-size 'vec-size)
	  (par-nb 'par-nb)
	  (par-callee 'this-callee)
	  (params (cddr formals))
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
						  '*js-Undefined*))
				       rev-res)))
			  (else
			   (loop (cdr params)
				 (cdr named-params)
				 counter
				 (cons (list (car params)
					     (car named-params))
				       rev-res))))))
	  (param-vars (map car bindings))

	  (arguments (cadr formals))
	  (this (or (car formals) (gensym 'ignored-this))))
      `(lambda (,this ,par-callee ,par-nb ,@named-params ,par-vec)
	  (let ((,vec-size (-fx ,par-nb ,(length named-params))))
	     (let* ,bindings
		,(if arguments
		     `(let ((,arguments (make-arguments ,par-callee
							,par-nb
							,param-vars
							,par-vec)))
			 ,body)
		     body))))))

(define-macro (js-new o . Largs)
   (let ((c (gensym 'class))
	 (new-fun (gensym 'new-fun)))
      `(let* ((,c (any->object ,o))
	      (,new-fun (safe-new-fun ,c)))
	  (js-call ,new-fun 'ignored ,@Largs))))
