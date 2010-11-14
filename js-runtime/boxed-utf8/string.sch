(directives
   (import jsre-conversion
	   jsre-base-object)
   (export
    (final-class boxed-string
       (str::bstring read-only)
       (hash::long read-only))
    (macro ascii->js-string-literal)
    (inline js-string?::bool str)
    (inline utf8->js-string-literal::boxed-string utf8-str::bstring
	    #!optional (needs-unescaping? #f))
    (inline utf8->js-string::boxed-string utf8-str::bstring)
    (inline js-string->utf8::bstring str::boxed-string)
    (inline js-string=ascii?::bool js-str::boxed-string ascii-str::bstring)
    (inline js-string-ref::char str::boxed-string i::long)
    (inline js-string=?::bool str1::boxed-string str2::boxed-string)
    (inline js-string-hash::long str::boxed-string)
    (inline js-string-null?::bool str::boxed-string)
    (inline js-string-length::long str::boxed-string)
    (inline js-substring::boxed-string str::boxed-string
	    from::long to::long)
    (macro js-string-append)
    (js-string-append::boxed-string . Lstrs)
    (inline js-string<?::bool str1::boxed-string str2::boxed-string)
    (inline js-ascii-substring-at?::bool str1::bstring str::boxed-string
	    pos::long)
    (inline js-substring-at?::bool str1::boxed-string str::boxed-string
	    pos::long)
    (inline js-string->real::double str::boxed-string)
    (inline js-string->integer::long str::boxed-string #!optional (base #f))
    (inline integer->js-string::boxed-string i::long)
    (inline real->js-string::boxed-string d::double)
    (inline llong->js-string::boxed-string i::llong #!optional (base #f))
    (inline vector->js-string::boxed-string v::vector)
    (inline js-char->js-string::boxed-string c::char)
    (inline js-string-contains str1::boxed-string str2::boxed-string
	    #!optional (pos::long 0))
    (inline js-string-compare3::long str1::boxed-string str2::boxed-string)
    (inline js-string-downcase::boxed-string str::boxed-string)
    (inline js-string-upcase::boxed-string str::boxed-string)
    (js-string-uc-iterator str::boxed-string)
    (open-js-string-buffer size)
    (js-string-buffer-uc-push! buf c::long)
    (js-string-buffer-verbatim-push! buf js-c::char)
    (close-js-string-buffer buf)
    (inline js-string-uc-char-size uc-c::long)
    ))

(define-macro (ascii->js-string-literal str) `(utf8->js-string ,str))

(define-inline (utf8->js-string-literal str #!optional (needs-unescaping? #f))
   (utf8->js-string str))

(define-inline (js-string?::bool str)
   (boxed-string? str))

(define-inline (utf8->js-string::boxed-string utf8-str)
   (instantiate::boxed-string (str utf8-str)
				(hash (string-hash utf8-str))))
(define-inline (js-string->utf8 str::boxed-string)
   (boxed-string-str str))

(define-inline (js-string=ascii? js-str ascii-str)
   (string=? (boxed-string-str js-str) ascii-str))

(define-inline (js-string-ref str::boxed-string i::long)
   (string-ref (boxed-string-str str) i))

(define-inline (js-string=? str1::boxed-string str2::boxed-string)
   (and (=fx (boxed-string-hash str1) (boxed-string-hash str2))
	(string=? (boxed-string-str str1) (boxed-string-str str2))))

(define-inline (js-string-hash::long str::boxed-string)
   (boxed-string-hash str))

(define-inline (js-string-null?::bool str::boxed-string)
   (string-null? (boxed-string-str str)))

(define-inline (js-string-length::long str::boxed-string)
   (string-length (boxed-string-str str)))

(define-inline (js-substring::boxed-string str::boxed-string
					     from::long to::long)
   (utf8->js-string (substring (boxed-string-str str) from to)))

(define-macro (js-string-append . L)
   `(utf8->js-string
     (string-append ,@(map (lambda (s) `(boxed-string-str ,s)) L))))
(define (js-string-append::boxed-string . L)
   (utf8->js-string (apply string-append (map boxed-string-str L))))

(define-inline (js-string<?::bool str1::boxed-string str2::boxed-string)
   (string<? (boxed-string-str str1) (boxed-string-str str2)))

(define-inline (js-ascii-substring-at?::bool str1::bstring
					     str2::boxed-string
					     pos::long)
   (substring-at? str1 (boxed-string-str str2) pos))

(define-inline (js-substring-at?::bool str1::boxed-string
				       str2::boxed-string
				       pos::long)
   (substring-at? (boxed-string-str str1) (boxed-string-str str2) pos))

(define-inline (js-string->real::double str::boxed-string)
   (string->real (boxed-string-str str)))
(define-inline (js-string->integer::long str::boxed-string
					 #!optional (base #f))
   (if base
       (string->integer (boxed-string-str str) base)
       (string->integer (boxed-string-str str))))

(define-inline (integer->js-string::boxed-string i::long)
   (utf8->js-string (integer->string i)))
(define-inline (real->js-string::boxed-string d::double)
   (utf8->js-string (real->string d)))
(define-inline (llong->js-string::boxed-string l::llong #!optional (base #f))
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
(define-inline (js-char->js-string c::char)
   (utf8->js-string (string c)))

(define-inline (js-string-contains str1::boxed-string str2::boxed-string
				   #!optional (pos::long 0))
   (string-contains (boxed-string-str str1) (boxed-string-str str2) pos))

(define-inline (js-string-compare3 str1::boxed-string str2::boxed-string)
   (string-compare3 (boxed-string-str str1) (boxed-string-str str2)))

;; downcase must work in unicode (if the implementation allows UC)
(define-inline (js-string-downcase::boxed-string base-str::boxed-string)
   (with-access::boxed-string base-str (str)
      (let ((downcased (utf8-downcase str)))
	 (if (eq? downcased str)
	     base-str
	     (utf8->js-string downcased)))))

;; upcase must work in unicode (if the implementation allows UC)
(define-inline (js-string-upcase::boxed-string base-str::boxed-string)
   (with-access::boxed-string base-str (str)
      (let ((upcased (utf8-upcase str)))
	 (if (eq? upcased str)
	     base-str
	     (utf8->js-string upcased)))))

;; raises an error if the string is not a correct Unicode string.
;; the iterator (a closure) must return integers or #f (eos).
(define (js-string-uc-iterator str::boxed-string)
   (utf8-uc-iterator (boxed-string-str str)))

(define (open-js-string-buffer size)
   (open-utf8-string-buffer size))

(define (js-string-buffer-uc-push! buf c::long)
   (utf8-buffer-uc-push! buf c))

;; js-c must be a char that has been received by js-string-ref.
(define (js-string-buffer-verbatim-push! buf js-c::char)
   (utf8-buffer-verbatim-push! buf js-c))

(define (close-js-string-buffer buf)
   (utf8->js-string (close-utf8-buffer buf)))

;; how many "native" chars does the given uc-char use?
(define-inline (js-string-uc-char-size uc-c::long)
   (utf8-char-length uc-c))
