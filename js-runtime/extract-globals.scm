(module extract-globals
   (main my-main))

(define (extract-define-globals expr)
   (map (lambda (def)
	   (let ((id (if (pair? (cadr def))
			 (caadr def)
			 (cadr def))))
	      (list id (symbol-append 'jsg- id))))
	(cdr expr)))

(define (extract-operator expr)
   (define (starts-with? str prefix)
      (let ((prefix-length (string-length prefix)))
	 (and (>= (string-length str) prefix-length)
	      (string=? prefix (substring str 0 prefix-length)))))

   (let* ((formals (cadr expr))
	  (id (car formals))
	  (id-str (symbol->string id)))
      (if (starts-with? id-str "jsop-")
	  (let* ((id-w/o-prefix-str (substring id-str
					       (string-length "jsop-")
					       (string-length id-str)))
		 (id-w/o-prefix (string->symbol id-w/o-prefix-str)))
	     (list id-w/o-prefix id 'operator))
	  #f)))

(define (search-for-global-adds expr)
   (define (global-add!-binding expr)
      (let ((id (if (pair? (cadr expr)) ; quote
		    (cadr (cadr expr))
		    (string->symbol (cadr expr)))))
	 (list id (caddr expr))))

   (cond
      ((and (pair? expr) (memq (car expr) '(global-declared-add!
					    global-implicit-add!
					    global-runtime-add!
					    global-special-add!)))
       (list (global-add!-binding expr)))
      ((and (pair? expr) (eq? (car expr) 'define))
       (search-for-global-adds (cddr expr)))
      ((and (pair? expr) (eq? (car expr) 'module))
       '())
      ((pair? expr)
       (let loop ((expr expr)
		  (res '()))
	  (cond
	     ((null? expr)
	      res)
	     ((pair? expr)
	      (let ((tmp (search-for-global-adds (car expr))))
		 (loop (cdr expr)
		       (if (null? tmp)
			   res
			   (append tmp res)))))
	     (else
	      (let ((tmp (search-for-global-adds expr)))
		 (if (null? tmp)
		     res
		     (append tmp res)))))))
      (else
       '())))
				   
(define (my-main args)
   (let loop ((bindings '()))
      (let ((expr (read)))
	 (if (eof-object? expr)
	     (pp `(define *runtime-variables* ',bindings))
	     (loop (append
		    (if (pair? expr)
			(cond
			   ((eq? (car expr) 'define-runtime-globals)
			    (extract-define-globals expr))
			   ((eq? (car expr) 'define-inline)
			    (let ((tmp (extract-operator expr)))
			       (if tmp (list tmp) '())))
			   ((eq? (car expr) 'define-macro)
			    (let ((tmp (extract-operator expr)))
			       (if tmp (list tmp) '())))
			   ((and (eq? (car expr) 'define)
				 (pair? (cadr expr))
				 (extract-operator expr))
			    => (lambda (operator)
				  (list operator)))
			   (else
			    (search-for-global-adds expr)))
			'())
		    bindings))))))
