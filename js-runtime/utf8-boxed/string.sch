(directives
   (import jsre-conversion
	   jsre-base-object)
   (export
    (final-class js-string
       (str::bstring read-only)
       (hash::long read-only))
    (macro ascii->js-string-literal)
    (inline utf8->js-string-literal::js-string utf8-str::bstring
	    #!optional (needs-unescaping? #f))
    (inline utf8->js-string::js-string utf8-str::bstring)
    (inline js-string->utf8::bstring str::js-string)
    (inline js-string=ascii?::bool js-str::js-string ascii-str::bstring)
    (inline js-string-ref::js-char str::js-string i::long)
    (inline js-string=?::bool str1::js-string str2::js-string)
    (inline js-string-hash::long str::js-string)
    (inline js-string-null?::bool str::js-string)
    (inline js-string-length::long str::js-string)
    (inline js-substring::js-string str::js-string
	    from::long to::long)
    (macro js-string-append)
    (js-string-append::js-string . Lstrs)
    (inline js-string<?::bool str1::js-string str2::js-string)
    (inline js-ascii-substring-at?::bool str1::bstring str::js-string
	    pos::long)
    (inline js-substring-at?::bool str1::js-string str::js-string
	    pos::long)
    (inline js-string->real::double str::js-string)
    (inline js-string->integer::long str::js-string #!optional (base #f))
    (inline integer->js-string::js-string i::long)
    (inline real->js-string::js-string d::double)
    (inline llong->js-string::js-string i::llong #!optional (base #f))
    (inline vector->js-string::js-string v::vector)
    (inline js-char->js-string::js-string c::js-char)
    (inline js-string-contains str1::js-string str2::js-string
	    #!optional (pos::long 0))
    (inline js-string-compare3::long str1::js-string str2::js-string)
    (inline js-string-downcase::js-string str::js-string)
    (inline js-string-upcase::js-string str::js-string)
    (js-string-uc-iterator str::js-string)
    (open-js-string-buffer size)
    (js-string-buffer-uc-push! buf c::long)
    (js-string-buffer-verbatim-push! buf js-c::js-char)
    (close-js-string-buffer buf)
    (inline js-string-uc-char-size uc-c::long)
    ))

(define-macro (ascii->js-string-literal str) `(utf8->js-string ,str))

(define-inline (utf8->js-string-literal str #!optional (needs-unescaping? #f))
   (utf8->js-string str))

(define-inline (js-string?::bool str)
   (js-string? str))

(define-inline (utf8->js-string::js-string utf8-str)
   (instantiate::js-string (str utf8-str)
				(hash (string-hash utf8-str))))
(define-inline (js-string->utf8 str::js-string)
   (js-string-str str))

(define-inline (js-string=ascii? js-str ascii-str)
   (string=? (js-string-str js-str) ascii-str))

(define-inline (js-string-ref str::js-string i::long)
   (string-ref (js-string-str str) i))

(define-inline (js-string=? str1::js-string str2::js-string)
   (and (=fx (js-string-hash str1) (js-string-hash str2))
	(string=? (js-string-str str1) (js-string-str str2))))

(define-inline (js-string-hash::long str::js-string)
   (js-string-hash str))

(define-inline (js-string-null?::bool str::js-string)
   (string-null? (js-string-str str)))

(define-inline (js-string-length::long str::js-string)
   (string-length (js-string-str str)))

(define-inline (js-substring::js-string str::js-string
					     from::long to::long)
   (utf8->js-string (substring (js-string-str str) from to)))

(define-macro (js-string-append . L)
   `(utf8->js-string
     (string-append ,@(map (lambda (s) `(js-string-str ,s)) L))))
(define (js-string-append::js-string . L)
   (utf8->js-string (apply string-append (map js-string-str L))))

(define-inline (js-string<?::bool str1::js-string str2::js-string)
   (string<? (js-string-str str1) (js-string-str str2)))

(define-inline (js-ascii-substring-at?::bool str1::bstring
					     str2::js-string
					     pos::long)
   (substring-at? str1 (js-string-str str2) pos))

(define-inline (js-substring-at?::bool str1::js-string
				       str2::js-string
				       pos::long)
   (substring-at? (js-string-str str1) (js-string-str str2) pos))

(define-inline (js-string->real::double str::js-string)
   (string->real (js-string-str str)))
(define-inline (js-string->integer::long str::js-string
					 #!optional (base #f))
   (if base
       (string->integer (js-string-str str) base)
       (string->integer (js-string-str str))))

(define-inline (integer->js-string::js-string i::long)
   (utf8->js-string (integer->string i)))
(define-inline (real->js-string::js-string d::double)
   (utf8->js-string (real->string d)))
(define-inline (llong->js-string::js-string l::llong #!optional (base #f))
   (if base
       (utf8->js-string (llong->string l base))
       (utf8->js-string (llong->string l))))

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
(define-inline (js-char->js-string c::js-char)
   (utf8->js-string (string c)))

(define-inline (js-string-contains str1::js-string str2::js-string
				   #!optional (pos::long 0))
   (string-contains (js-string-str str1) (js-string-str str2) pos))

(define-inline (js-string-compare3 str1::js-string str2::js-string)
   (string-compare3 (js-string-str str1) (js-string-str str2)))

;; downcase must work in unicode (if the implementation allows UC)
(define-inline (js-string-downcase::js-string base-str::js-string)
   (with-access::js-string base-str (str)
      (let ((downcased (utf8-downcase str)))
	 (if (eq? downcased str)
	     base-str
	     (utf8->js-string downcased)))))

;; upcase must work in unicode (if the implementation allows UC)
(define-inline (js-string-upcase::js-string base-str::js-string)
   (with-access::js-string base-str (str)
      (let ((upcased (utf8-upcase str)))
	 (if (eq? upcased str)
	     base-str
	     (utf8->js-string upcased)))))

;; raises an error if the string is not a correct Unicode string.
;; the iterator (a closure) must return integers or #f (eos).
(define (js-string-uc-iterator str::js-string)
   (utf8-uc-iterator (js-string-str str)))

(define (open-js-string-buffer size)
   (open-utf8-string-buffer size))

(define (js-string-buffer-uc-push! buf c::long)
   (utf8-buffer-uc-push! buf c))

;; js-c must be a char that has been received by js-string-ref.
(define (js-string-buffer-verbatim-push! buf js-c::js-char)
   (utf8-buffer-verbatim-push! buf js-c))

(define (close-js-string-buffer buf)
   (utf8->js-string (close-utf8-buffer buf)))

;; how many "native" chars does the given uc-char use?
(define-inline (js-string-uc-char-size uc-c::long)
   (utf8-char-length uc-c))
