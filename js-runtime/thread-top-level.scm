(module thread-top-level
   (export
    (macro thread-top-level)))

(define-macro (thread-top-level globals-class . tl-els)
   (define *globals-id* (gensym 'globals))

   (define (without-type s)
      (let* ((str (symbol->string s))
	     (type-pos (string-contains str "::")))
	 (string->symbol
	  (if type-pos
	      (substring str 0 type-pos)
	      str))))
   (define (instrumented s)
      (symbol-append *globals-id* '- (without-type s)))
   (define (extract-fields c)
      ;; skip 'class and class-name.
      (map without-type (cddr c)))

   (define (produce-intercepts defines generics except)
      (map (lambda (f)
	      (let ((fun-name (car (cadr f)))
		    (args (cdr (cadr f))))
		 (if (memq fun-name except) ;; don't shadow ids of except.
		     #unspecified
		     `(define (,fun-name ,@args)
			 (,(instrumented fun-name)
			  ,*globals-id* ,@args)))))
	   (append generics defines)))

   (define (wrapped-body class-name globals body except)
      (let ((filtered-globals (filter (lambda (g)
					 (not (memq g except)))
				      globals)))
	 (if (null? filtered-globals)
	     body
	     `(,(symbol-append 'with-access:: class-name)
	       ,*globals-id*
	       ,filtered-globals
	       ,body))))
   
   ;; globals that are not thread-safe.
   (define (produce-free-globals globals global-defs)
      (map (lambda (def)
	      (if (memq (cadr def) globals)
		  #unspecified
		  def))
	   global-defs))

   (define (produce-globals-gen c-name globals global-defs)
      (let ((id/vals (map cdr global-defs)))
	 (tprint global-defs)
	 (tprint id/vals)
	 (tprint globals)
	 `(define (,(symbol-append *globals-id* '-gen))
	     (,(symbol-append 'instantiate:: c-name)
	      ,@(map (lambda (field)
			(let ((clause (assq field id/vals)))
			   (if (not clause)
			       (error "thread-top-level"
				      "global without definition"
				      field)
			       clause)))
		     globals)))))
	 
   ;; entry points
   (define (produce-exports defines generics)
      (map (lambda (f)
	      (let ((fun-name (car (cadr f)))
		    (args (cdr (cadr f))))
		 `(define (,fun-name ,@args)
		     (,(instrumented fun-name)
		      (,(symbol-append *globals-id* '-gen))
		      ,@args))))
	   (append generics defines)))

   (define (produce-instrumented class-name globals defines generics methods)
      (define (extract-arg-ids args)
	 (cond
	    ((null? args)
	     '())
	    ((not (pair? args))
	     (without-type args))
	    (else
	     (cons (without-type (car args))
		   (extract-arg-ids (cdr args))))))

      (map (lambda (f)
	      (let* ((def (car f))
		     (fun-name (car (cadr f)))
		     (args (cdr (cadr f)))
		     (arg-ids (extract-arg-ids args))
		     (body `(begin ,(cddr f))))
		 `(,def (,(instrumented fun-name)
			 ,*globals-id* ,@args)
			,@(produce-intercepts defines generics arg-ids)
			,(wrapped-body class-name globals body arg-ids))))
	   (append defines generics methods)))
      
   (define (produce-code global-defs defines generics methods)
      (let ((globals (extract-fields globals-class))
	    (class-name (cadr globals-class)))
	 `(begin
	     ,@(produce-free-globals globals global-defs)
	     ,(produce-globals-gen class-name globals global-defs)
	     ,@(produce-exports defines generics)
	     ,@(produce-instrumented class-name globals
				     defines generics methods))))
	     
   (let loop ((tl-els tl-els)
	      (rev-globals '())
	      (rev-defines '())
	      (rev-generics '())
	      (rev-methods '()))
      (if (null? tl-els)
	  (produce-code (reverse! rev-globals)
			(reverse! rev-defines)
			(reverse! rev-generics)
			(reverse! rev-methods))
	  (match-case (car tl-els)
	     ((define (?fun ???-) ???-)
	      (loop (cdr tl-els)
		    rev-globals
		    (cons (car tl-els) rev-defines)
		    rev-generics rev-methods))
	     ((define ???-)
	      (loop (cdr tl-els)
		    (cons (car tl-els) rev-globals)
		    rev-defines
		    rev-generics rev-methods))
	     ((define-generic ???-)
	      (loop (cdr tl-els)
		    rev-globals rev-defines
		    (cons (car tl-els) rev-generics)
		    rev-methods))
	     ((define-method ???-)
	      (loop (cdr tl-els)
		    rev-globals rev-defines rev-generics
		    (cons (car tl-els) rev-methods)))
	     ((begin . ?rest)
	      (loop (append rest tl-els)
		    rev-globals rev-defines
		    rev-generics rev-methods))
	     (else
	      (error "thread-top-level"
		     "expression inside construct"
		     (car tl-els)))))))
