(module extract-globals
   (main m))

(define (extract-define-globals expr)
   (map (lambda (def)
	   (let ((id (if (pair? (cadr def))
			 (caadr def)
			 (cadr def))))
	      (list id (symbol-append 'jsg- id))))
	(cdr expr)))

(define (extract-operator expr)
   (let* ((formals (cadr expr))
	  (id (car formals))
	  (exported-id (symbol-append 'jsop- id)))
      (list (list id exported-id 'operator))))

(define (search-for-global-adds expr)
   (define (global-add!-binding expr)
      (let ((id (if (pair? (cadr expr)) ; quote
		    (cadr (cadr expr))
		    (string->symbol (cadr expr)))))
	 (list id (caddr expr))))

   (cond
      ((and (pair? expr) (eq? (car expr) 'global-add!))
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
				   
(define (m args)
   (let loop ((bindings '()))
      (let ((expr (read)))
	 (if (eof-object? expr)
	     (print `(define *runtime-variables* ',bindings))
	     (loop (append
		    (if (pair? expr)
			(cond
			   ((eq? (car expr) 'define-globals)
			    (extract-define-globals expr))
			   ((eq? (car expr) 'define-operator)
			    (extract-operator expr))
			   (else
			    (search-for-global-adds expr)))
			'())
		    bindings))))))
