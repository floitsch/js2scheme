;; fill-sensitive-classes and fill-insensitive-classes are ascii only.
;; -------------------------------------------------

(define (fill-sensitive-classes! size v)
   (let loop ((i 0))
      (when (<fx i size)
	 (let ((re-class (instantiate::RE-class
			    (char-set (new-empty-char-set))
			    (constant? #t))))
	    (with-access::RE-class re-class (char-set)
	       (char-set-add-n! char-set i))
	    (vector-set! v i re-class)
	    (loop (+fx i 1))))))

(define (fill-insensitive-classes! size v sensitive-v)
   (let loop ((i 0))
      (when (<fx i size)
	 (let* ((c (integer->js-char i))
		(c-up (js-char-upcase c))
		(c-down (js-char-downcase c)))
	    (if (js-char=? c-up c-down)
		(vector-set! v i (vector-ref sensitive-v i))
		(let ((re-class (instantiate::RE-class
				   (char-set (new-empty-char-set))
				   (constant? #t))))
		   (with-access::RE-class re-class (char-set)
		      (char-set-add-n! char-set (js-char->integer c-up))
		      (char-set-add-n! char-set (js-char->integer c-down)))
		   (vector-set! v i re-class)))
	    (loop (+fx i 1))))))

(define-macro (create-constant-classes . Lclasses)
   (define (inverted-sym s)
      (let ((str (symbol->string s)))
	 (if (string-prefix? "not-" str)
	     (string->symbol (substring str 4 (string-length str)))
	     (symbol-append 'not- s))))
      
   (define (class-name s)
      (symbol-append '* s '-class*))
   (define (inv-class-name s)
      (symbol-append '* (inverted-sym s) '-class*))
   (define (class-keyword s)
      (symbol->keyword s))
   (define (inv-class-keyword s)
      (symbol->keyword (inverted-sym s)))

   ;; given a class-name like *digit-class* and its elements create the
   ;; instructions to fill the empty class with the given elements.
   ;; els can be chars, integers, lambdas or lists, where a list represents a
   ;; range. Lambdas are executed (using 'eval') and are supposed to return a
   ;; list of integers.
   (define (fill-class name els)
      (let ((set (gensym 'set)))
	 `(let ((,set (RE-class-char-set ,name)))
	     ,(let loop ((els els)
			 (res '())) ;; in reverse order. does not matter.
		 (cond
		    ((null? els)
		     `(begin ,@res))
		    ((and (pair? (car els))
			  (eq? (caar els) 'lambda))
		     (let* ((f (eval (car els)))
			    (ns (f)))
			(loop (cdr els)
			      (cons `(begin
					,@(map (lambda (n)
						  `(char-set-add-n! ,set ,n))
					       ns))
				    res))))
		    ((and (pair? (car els))
			  (char? (caar els)))
		     (let ((from (caar els))
			   (to (cadar els)))
			(loop (cdr els)
			      (cons
			       `(char-set-add-range-n! ,set
						       ,(char->integer from)
						       ,(char->integer to))
			       res))))
		    ((pair? (car els))
		     (let ((from (caar els))
			   (to (cadar els)))
			(loop (cdr els)
			      (cons `(char-set-add-range-n! ,set ,from ,to)
				    res))))
		    ((char? (car els))
		     (let ((n (char->integer (car els))))
			(loop (cdr els)
			      (cons `(char-set-add-n! ,set ,n) res))))
		    (else
			(loop (cdr els)
			      (cons `(char-set-add-n! ,set ,(car els))
				    res))))))))

   ;; given a char-class-description
   ;;  like '(digit (#\0 #\9))
   ;; create:
   ;;   (define *digit-class* (instantiate::RE-class ...)) [empty]
   ;;   (define *not-digit-class* (instantiate::....       [empty]
   ;;   <code to fill the empty *digit-class*>
   ;;   <copy the inverted chars from *digit-class* to *not-digit-class*
   (define (create-constant-class desc)
      (let ((def-name (class-name (car desc)))
	    (inv-def-name (inv-class-name (car desc)))
	    (els (cdr desc)))
	 `(begin
	     (define ,def-name (instantiate::RE-class
				  (char-set (new-empty-char-set))
				  (constant? #t)))
	     (define ,inv-def-name (instantiate::RE-class
				      (char-set (new-empty-char-set))
				      (constant? #t)))
	     ,(fill-class def-name els)
	     (char-set-copy-inverted (RE-class-char-set ,def-name)
				     (RE-class-char-set ,inv-def-name)))))

   (define (create-constant-class-pattern? names)
      (let ((keywords (map class-keyword names))
	    (inv-keywords (map inv-class-keyword names)))
	 `(define (constant-class-pattern? s)
	     (and (memq s '(,@keywords ,@inv-keywords)) #t))))

   (define (create-merge-constant-class names)
      `(define (merge-constant-class re-class id)
	  (merge-classes!
	   re-class
	   (case id
	      ,@(map (lambda (name)
			`((,(class-keyword name)) ,(class-name name)))
		     names)
	      ,@(map (lambda (name)
			`((,(inv-class-keyword name)) ,(inv-class-name name)))
		     names)))))
      
   (let ((names (map car Lclasses)))
      `(begin
	  ,@(map create-constant-class Lclasses)
	  ,(create-constant-class-pattern? names)
	  ,(create-merge-constant-class names))))
