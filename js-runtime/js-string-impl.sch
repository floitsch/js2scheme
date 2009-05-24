(directives
   (import jsre-conversion
	   jsre-base-object)
   (export
    (final-class Js-Base-String
       (str::bstring read-only))
    (inline js-string?::bool str)
    (inline utf8->js-string-literal::Js-Base-String utf8-str::bstring)
    (inline utf8->js-string::Js-Base-String utf8-str::bstring)
    (inline js-string->utf8::bstring str::Js-Base-String)
    (inline js-string=utf8?::bool js-str::Js-Base-String utf8-str::bstring)
    (inline js-string-ref::js-char str::Js-Base-String i::bint)
    (inline js-string=?::bool str1::Js-Base-String str2::Js-Base-String)
    (inline symbol->js-string::Js-Base-String sym::symbol)
    (inline js-string-hash::bint str::Js-Base-String)
    (inline js-string-null?::bool str::Js-Base-String)
    (inline js-string-length::bint str::Js-Base-String)
    (inline js-substring::Js-Base-String str::Js-Base-String
	    from::bint to::bint)
    (macro js-string-append)
    (js-string-append::Js-Base-String . Lstrs)
    (inline js-string<?::bool str1::Js-Base-String str2::Js-Base-String)
    (inline js-substring-at_utf8?::bool str1::Js-Base-String str::bstring
	    pos::bint)
    (inline js-substring-at?::bool str1::Js-Base-String str::Js-Base-String
	    pos::bint)
    (inline js-string->real::double str::Js-Base-String)
    (inline js-string->integer::bint str::Js-Base-String #!optional (base #f))
    (inline integer->js-string::Js-Base-String i::bint)
    (inline real->js-string::Js-Base-String d::double)
    (inline llong->js-string::Js-Base-String i::llong #!optional (base #f))
    (inline js-string-prefix? str1::Js-Base-String str2::Js-Base-String
	    from::bint to::bint index::bint)
    (inline js-string-prefix-ci? str1::Js-Base-String str2::Js-Base-String
	    from::bint to::bint index::bint)
    (inline vector->js-string::Js-Base-String v::vector)
    (inline js-char->js-string::Js-Base-String c::js-char)
    (inline js-string-contains str1::Js-Base-String str2::Js-Base-String
	    #!optional (pos::bint 0))
    (inline js-string-compare3::bint str1::Js-Base-String str2::Js-Base-String)
    (inline js-string-contains-char? str::Js-Base-String c::char)
    (inline js-string-downcase::Js-Base-String str::Js-Base-String)
    (inline js-string-upcase::Js-Base-String str::Js-Base-String)
    (js-string-uc-iterator str::Js-Base-String)
    (open-js-string-buffer size)
    (js-string-buffer-uc-push! buf c::long)
    (js-string-buffer-verbatim-push! buf js-c::js-char)
    (close-js-string-buffer buf)
    (js-string-uc-char-size uc-c::long)
    ))

(define-inline (utf8->js-string-literal str)
   (utf8->js-string str))

(define-inline (js-string?::bool str)
   (Js-Base-String? str))

(define-inline (utf8->js-string::Js-Base-String utf8-str)
   (instantiate::Js-Base-String (str utf8-str)))
(define-inline (js-string->utf8 str::Js-Base-String)
   (Js-Base-String-str str))

(define-inline (js-string=utf8? js-str utf8-str)
   (string=? (Js-Base-String-str js-str) utf8-str))

(define-inline (js-string-ref str::Js-Base-String i::bint)
   (let ((c (string-ref (Js-Base-String-str str) i)))
      (char->js-char c)))

(define-inline (js-string=? str1::Js-Base-String str2::Js-Base-String)
   (string=? (Js-Base-String-str str1) (Js-Base-String-str str2)))

(define-inline (symbol->js-string::Js-Base-String sym::symbol)
   (instantiate::Js-Base-String (str (symbol->string sym))))

(define-inline (js-string-hash::bint str::Js-Base-String)
   (string-hash (Js-Base-String-str str)))

(define-inline (js-string-null?::bool str::Js-Base-String)
   (string-null? (Js-Base-String-str str)))

(define-inline (js-string-length::bint str::Js-Base-String)
   (string-length (Js-Base-String-str str)))

(define-inline (js-substring::Js-Base-String str::Js-Base-String
					     from::bint to::bint)
   (instantiate::Js-Base-String
      (str (substring (Js-Base-String-str str) from to))))

(define-macro (js-string-append . L)
   `(utf8->js-string
     (string-append ,@(map (lambda (s) `(Js-Base-String-str ,s)) L))))
(define (js-string-append::Js-Base-String . L)
   (utf8->js-string (apply string-append (map Js-Base-String-str L))))

(define-inline (js-string<?::bool str1::Js-Base-String str2::Js-Base-String)
   (string<? (Js-Base-String-str str1) (Js-Base-String-str str2)))

(define-inline (js-substring-at_utf8?::bool str1::Js-Base-String
					    str2::bstring
					    pos::bint)
   (substring-at? (Js-Base-String-str str1) str2 pos))

(define-inline (js-substring-at?::bool str1::Js-Base-String
				       str2::Js-Base-String
				       pos::bint)
   (js-substring-at_utf8? str1 (Js-Base-String-str str2) pos))

(define-inline (js-string->real::double str::Js-Base-String)
   (string->real (Js-Base-String-str str)))
(define-inline (js-string->integer::bint str::Js-Base-String
					 #!optional (base #f))
   (if base
       (string->integer (Js-Base-String-str str) base))
       (string->integer (Js-Base-String-str str)))

(define-inline (integer->js-string::Js-Base-String i::bint)
   (utf8->js-string (integer->string i)))
(define-inline (real->js-string::Js-Base-String d::double)
   (utf8->js-string (real->string d)))
(define-inline (llong->js-string::Js-Base-String l::llong #!optional (base #f))
   (if base
       (utf8->js-string (llong->string l base))
       (utf8->js-string (llong->string l))))

(define-inline (js-string-prefix? str1::Js-Base-String str2::Js-Base-String
				  from to index)
   (string-prefix? (Js-Base-String-str str1) (Js-Base-String-str str2)
		   from to index))

(define-inline (js-string-prefix-ci? str1::Js-Base-String str2::Js-Base-String
				     from to index)
   (string-prefix-ci? (Js-Base-String-str str1) (Js-Base-String-str str2)
		      from to index))

;; 'inline', since it is used at only one place anyways and this way Bigloo can
;; type.
(define-inline (vector->js-string v)
   (let* ((len (vector-length v))
	  (str (make-string len)))
      (let loop ((i 0))
	 (if (>= i len)
	     (utf8->js-string str)
	     (let* ((cv (vector-ref v i)))
		(string-set! str i (any->uint8fx (vector-ref v i)))
		(loop (+fx i 1)))))))

;; c is guaranteed to be a char retrieved by 'js-string-ref'.
(define-inline (js-char->js-string c)
   (utf8->js-string (string (js-char->char c))))

(define-inline (js-string-contains str1::Js-Base-String str2::Js-Base-String
				   #!optional (pos::bint 0))
   (string-contains (Js-Base-String-str str1) (Js-Base-String-str str2) pos))

(define-inline (js-string-compare3 str1::Js-Base-String str2::Js-Base-String)
   (string-compare3 (Js-Base-String-str str1) (Js-Base-String-str str2)))

;; internally used function. Just return true if 'c' is inside the str.
;; 'c' is guaranteed to be an ascii char. (as of 20090524 c is always '$').
(define-inline (js-string-contains-char? str::Js-Base-String c::char)
   (and (string-index (Js-Base-String-str str) c) #t))

;; downcase must work in unicode (if the implementation allows UC)
(define-inline (js-string-downcase::Js-Base-String base-str::Js-Base-String)
   ;; TODO: we only allow ascii in this implementation.
   (let* ((str (Js-Base-String-str base-str))
	  (len (string-length str)))
      (let loop ((i 0))
	 (cond
	    ((>=fx i len) base-str) ;; no uppercase-chars. return the old string.
	    ((char-upper-case? (string-ref str i))
	     (utf8->js-string (string-downcase str)))
	    (else (loop (+fx i 1)))))))

;; upcase must work in unicode (if the implementation allows UC)
(define-inline (js-string-upcase::Js-Base-String base-str::Js-Base-String)
   ;; TODO: we only allow ascii in this implementation.
   (let* ((str (Js-Base-String-str base-str))
	  (len (string-length str)))
      (let loop ((i 0))
	 (cond
	    ((>=fx i len) base-str) ;; no uppercase-chars. return the old string.
	    ((char-lower-case? (string-ref str i))
	     (utf8->js-string (string-upcase str)))
	    (else (loop (+fx i 1)))))))

;; raises an error if the string is not a correct Unicode string.
;; the iterator (a closure) must return integers or #f (eos).
(define (js-string-uc-iterator str::Js-Base-String)
   (define-macro (+fx+ x . L)
      (if (null? L)
	  x
	  `(+fx ,x (+fx+ ,@L))))
   (define (&<< x and-v shift-by)
      (bit-lsh (bit-and x and-v) shift-by))

   (let* ((str (Js-Base-String-str str))
	  (str-len (string-length str))
	  (i 0))
      (lambda ()
	 (define (get-safe-surrogate j)
	    (when (>=fx j str-len)
	       (error "unicode iterator"
		      "bad utf8 sequence"
		      #f))
	    (let ((ci (char->integer (string-ref str j))))
	       (when (not (=fx #x80 (bit-and #xC ci)))
		  (error "unicode iterator"
			 "bad utf8 surrogate"
			 #f))
	       ci))

	 (if (>= i str-len)
	     #f ;; eos
	     (let ((ci (char->integer (string-ref str i))))
		(cond
		   ((<fx ci 128)
		    ;; ascii-char.
		    (set! i (+fx i 1))
		    ci)
		   ((=fx #xF0 (bit-and #xF8 ci))
		    ;; 4 char encoding
		    (let* ((b2 (get-safe-surrogate (+fx i 1)))
			   (b3 (get-safe-surrogate (+fx i 2)))
			   (b4 (get-safe-surrogate (+fx i 3))))
		       (set! i (+fx i 4))
		       (+fx+ (&<< ci #x07 18)
			     (&<< b2 #x3F 12)
			     (&<< b3 #x3F 6)
			     (&<< b4 #x3F 0))))
		   ((=fx #xE0 (bit-and #xF0 ci))
		    ;; 3 char encoding
		    (let* ((b2 (get-safe-surrogate (+fx i 1)))
			   (b3 (get-safe-surrogate (+fx i 2))))
		       (set! i (+fx i 3))
		       (+fx+ (&<< ci #x0F 12)
			     (&<< b2 #x3F 6)
			     (&<< b3 #x3F 0))))
		   ((=fx #xC0 (bit-and #xE0 ci))
		    (let ((b (get-safe-surrogate (+fx i 1))))
		       (set! i (+fx i 2))
		       ;; 2 char encoding
		       (+fx (&<< ci #x1F 6)
			    (bit-and #x3F b))))
		   (else
		    (error "unicode iterator"
			   "bad utf8 sequence"
			   #f))))))))

(define (open-js-string-buffer size)
   (cons 0 (make-string size)))

(define (js-string-buffer-uc-push! buf c::long)
   (let ((str (cdr buf))
	 (i (car buf)))
      ;; only updates the local var!
      (define (push! c)
	 (string-set! str i (integer->char c))
	 (set! i (+fx i 1)))
      (cond
	 ((<=fx c #x7F)
	  (push! c))
	 ((<=fx c #x7FF)
	  (let ((b1 (+fx #xC0 (bit-ursh c 6)))
		(b2 (+fx #x80 (bit-and c #x3F))))
	     (push! b1)
	     (push! b2)))
	 ((<=fx c #xFFFF)
	  (let ((b1 (+fx #xE0 (bit-ursh c 12)))
		(b2 (+fx #x80
			 (bit-and (bit-ursh c 6) #x3F)))
		(b3 (+fx #x80 (bit-and c #x3F))))
	     (push! b1)
	     (push! b2)
	     (push! b3)))
	 (else
	  (let ((b1 (+fx #xF0 (bit-ursh c 18)))
		(b2 (+fx #x80
			 (bit-and (bit-ursh c 12) #x3F)))
		(b3 (+fx #x80
			 (bit-and (bit-ursh c 6) #x3F)))
		(b4 (+fx #x80 (bit-and c #x3F))))
	     (push! b1)
	     (push! b2)
	     (push! b3)
	     (push! b4))))
      (set-car! buf i)))

;; js-c must be a char that has been received by js-string-ref.
(define (js-string-buffer-verbatim-push! buf js-c::js-char)
   (let ((str (cdr buf))
	 (i (car buf)))
      (string-set! buf i (js-char->char js-c))
      (set-car! buf (+fx i 1))))

(define (close-js-string-buffer buf)
   (utf8->js-string (cdr buf)))

;; how many "native" chars does the given uc-char use?
(define (js-string-uc-char-size uc-c::long)
   (cond
      ((<=fx uc-c #x7F) 1)
      ((<=fx uc-c #x7FF) 2)
      ((<=fx uc-c #xFFFF) 3)
      (else 4)))
