(directives
   (export
    (final-class Js-Base-String
       (str::bstring read-only))
    (inline js-string?::bool str)
    (inline utf8->js-string-literal::Js-Base-String utf8-str::bstring)
    (inline utf8->js-string::Js-Base-String utf8-str::bstring)
    (inline js-string->utf8::bstring str::Js-Base-String)
    (inline js-string=utf8?::bool js-str::Js-Base-String utf8-str::bstring)
    (inline js-string-ref::char str::Js-Base-String i::bint) ;; TODO: rm char
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
    (inline char->js-string::Js-Base-String c::char) ;; TODO rm char-type
    (inline js-string-contains str1::Js-Base-String str2::Js-Base-String
	    #!optional (pos::bint 0))
    (inline js-string-compare3::bint str1::Js-Base-String str2::Js-Base-String)
    (inline js-string-index str1::Js-Base-String c::char) ;; TODO rm char-type
    (inline js-string-downcase::Js-Base-String str::Js-Base-String)
    (inline js-string-upcase::Js-Base-String str::Js-Base-String)))

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
   (string-ref (Js-Base-String-str str) i))

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
	     (let* ((cv (vector-ref v i))
		    (c (if (>=fx cv 256)
			   #\- ;; TODO we only allow ascii...
			   (integer->char cv))))
		(string-set! str i c)
		(loop (+fx i 1)))))))

(define-inline (char->js-string c)
   (utf8->js-string (string c)))

(define-inline (js-string-contains str1::Js-Base-String str2::Js-Base-String
				   #!optional (pos::bint 0))
   (string-contains (Js-Base-String-str str1) (Js-Base-String-str str2) pos))

(define-inline (js-string-compare3 str1::Js-Base-String str2::Js-Base-String)
   (string-compare3 (Js-Base-String-str str1) (Js-Base-String-str str2)))

(define-inline (js-string-index str::Js-Base-String c)
   (string-index (Js-Base-String-str str) c))

(define-inline (js-string-downcase::Js-Base-String base-str::Js-Base-String)
   (let* ((str (Js-Base-String-str base-str))
	  (len (string-length str)))
      (let loop ((i 0))
	 (cond
	    ((>=fx i len) base-str) ;; no uppercase-chars. return the old string.
	    ((char-upper-case? (string-ref str i))
	     (utf8->js-string (string-downcase str)))
	    (else (loop (+fx i 1)))))))

(define-inline (js-string-upcase::Js-Base-String base-str::Js-Base-String)
   (let* ((str (Js-Base-String-str base-str))
	  (len (string-length str)))
      (let loop ((i 0))
	 (cond
	    ((>=fx i len) base-str) ;; no uppercase-chars. return the old string.
	    ((char-lower-case? (string-ref str i))
	     (utf8->js-string (string-upcase str)))
	    (else (loop (+fx i 1)))))))
