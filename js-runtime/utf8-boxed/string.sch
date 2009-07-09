(directives
   (import jsre-conversion
	   jsre-base-object)
   (export
    (final-class Js-Base-String
       (str::bstring read-only)
       (hash::long read-only))
    (inline js-string?::bool str)
    (macro ascii->js-string-literal)
    (inline utf8->js-string-literal::Js-Base-String utf8-str::bstring
	    #!optional (needs-unescaping? #f))
    (inline utf8->js-string::Js-Base-String utf8-str::bstring)
    (inline js-string->utf8::bstring str::Js-Base-String)
    (inline js-string=ascii?::bool js-str::Js-Base-String ascii-str::bstring)
    (inline js-string-ref::js-char str::Js-Base-String i::long)
    (inline js-string=?::bool str1::Js-Base-String str2::Js-Base-String)
    (inline js-string-hash::long str::Js-Base-String)
    (inline js-string-null?::bool str::Js-Base-String)
    (inline js-string-length::long str::Js-Base-String)
    (inline js-substring::Js-Base-String str::Js-Base-String
	    from::long to::long)
    (macro js-string-append)
    (js-string-append::Js-Base-String . Lstrs)
    (inline js-string<?::bool str1::Js-Base-String str2::Js-Base-String)
    (inline js-ascii-substring-at?::bool str1::bstring str::Js-Base-String
	    pos::long)
    (inline js-substring-at?::bool str1::Js-Base-String str::Js-Base-String
	    pos::long)
    (inline js-string->real::double str::Js-Base-String)
    (inline js-string->integer::long str::Js-Base-String #!optional (base #f))
    (inline integer->js-string::Js-Base-String i::long)
    (inline real->js-string::Js-Base-String d::double)
    (inline llong->js-string::Js-Base-String i::llong #!optional (base #f))
    (inline vector->js-string::Js-Base-String v::vector)
    (inline js-char->js-string::Js-Base-String c::js-char)
    (inline js-string-contains str1::Js-Base-String str2::Js-Base-String
	    #!optional (pos::long 0))
    (inline js-string-compare3::long str1::Js-Base-String str2::Js-Base-String)
    (inline js-string-downcase::Js-Base-String str::Js-Base-String)
    (inline js-string-upcase::Js-Base-String str::Js-Base-String)
    (js-string-uc-iterator str::Js-Base-String)
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
   (Js-Base-String? str))

(define-inline (utf8->js-string::Js-Base-String utf8-str)
   (instantiate::Js-Base-String (str utf8-str)
				(hash (string-hash utf8-str))))
(define-inline (js-string->utf8 str::Js-Base-String)
   (Js-Base-String-str str))

(define-inline (js-string=ascii? js-str ascii-str)
   (string=? (Js-Base-String-str js-str) ascii-str))

(define-inline (js-string-ref str::Js-Base-String i::long)
   (string-ref (Js-Base-String-str str) i))

(define-inline (js-string=? str1::Js-Base-String str2::Js-Base-String)
   (and (=fx (Js-Base-String-hash str1) (Js-Base-String-hash str2))
	(string=? (Js-Base-String-str str1) (Js-Base-String-str str2))))

(define-inline (js-string-hash::long str::Js-Base-String)
   (Js-Base-String-hash str))

(define-inline (js-string-null?::bool str::Js-Base-String)
   (string-null? (Js-Base-String-str str)))

(define-inline (js-string-length::long str::Js-Base-String)
   (string-length (Js-Base-String-str str)))

(define-inline (js-substring::Js-Base-String str::Js-Base-String
					     from::long to::long)
   (utf8->js-string (substring (Js-Base-String-str str) from to)))

(define-macro (js-string-append . L)
   `(utf8->js-string
     (string-append ,@(map (lambda (s) `(Js-Base-String-str ,s)) L))))
(define (js-string-append::Js-Base-String . L)
   (utf8->js-string (apply string-append (map Js-Base-String-str L))))

(define-inline (js-string<?::bool str1::Js-Base-String str2::Js-Base-String)
   (string<? (Js-Base-String-str str1) (Js-Base-String-str str2)))

(define-inline (js-ascii-substring-at?::bool str1::bstring
					     str2::Js-Base-String
					     pos::long)
   (substring-at? str1 (Js-Base-String-str str2) pos))

(define-inline (js-substring-at?::bool str1::Js-Base-String
				       str2::Js-Base-String
				       pos::long)
   (substring-at? (Js-Base-String-str str1) (Js-Base-String-str str2) pos))

(define-inline (js-string->real::double str::Js-Base-String)
   (string->real (Js-Base-String-str str)))
(define-inline (js-string->integer::long str::Js-Base-String
					 #!optional (base #f))
   (if base
       (string->integer (Js-Base-String-str str) base)
       (string->integer (Js-Base-String-str str))))

(define-inline (integer->js-string::Js-Base-String i::long)
   (utf8->js-string (integer->string i)))
(define-inline (real->js-string::Js-Base-String d::double)
   (utf8->js-string (real->string d)))
(define-inline (llong->js-string::Js-Base-String l::llong #!optional (base #f))
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

(define-inline (js-string-contains str1::Js-Base-String str2::Js-Base-String
				   #!optional (pos::long 0))
   (string-contains (Js-Base-String-str str1) (Js-Base-String-str str2) pos))

(define-inline (js-string-compare3 str1::Js-Base-String str2::Js-Base-String)
   (string-compare3 (Js-Base-String-str str1) (Js-Base-String-str str2)))

;; downcase must work in unicode (if the implementation allows UC)
(define-inline (js-string-downcase::Js-Base-String base-str::Js-Base-String)
   (with-access::Js-Base-String base-str (str)
      (let ((downcased (utf8-downcase str)))
	 (if (eq? downcased str)
	     base-str
	     (utf8->js-string downcased)))))

;; upcase must work in unicode (if the implementation allows UC)
(define-inline (js-string-upcase::Js-Base-String base-str::Js-Base-String)
   (with-access::Js-Base-String base-str (str)
      (let ((upcased (utf8-upcase str)))
	 (if (eq? upcased str)
	     base-str
	     (utf8->js-string upcased)))))

;; raises an error if the string is not a correct Unicode string.
;; the iterator (a closure) must return integers or #f (eos).
(define (js-string-uc-iterator str::Js-Base-String)
   (utf8-uc-iterator (Js-Base-String-str str)))

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
