(directives
   (type (alias bstring js-string))
   (import jsre-conversion
	   jsre-base-object)
   (export
    (macro ascii->js-string-literal)
    (utf8->js-string-literal::js-string str::bstring #!optional (unescape? #f))
    (inline js-string?::bool str)
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
    ;; TODO: js-string-append macro is currently disabled due to type-problems.
    ;(macro js-string-append)
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

(define-macro (ascii->js-string-literal str) str)

(define (utf8->js-string-literal str #!optional (unescape? #f))
   (define (char-hex? c)
      (case c
	 ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9
	       #\a #\b #\c #\d #\e #\f
	       #\A #\B #\C #\D #\E #\F) #t)
	 (else #f)))
   (define (hex-char->number c)
      (case c
	 ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
	  (-fx (char->integer c) (char->integer #\0)))
	 ((#\a #\b #\c #\d #\e #\f)
	  (+fx 10 (-fx (char->integer c) (char->integer #\a))))
	 (else
	  (+fx 10 (-fx (char->integer c) (char->integer #\A))))))
   (define (x-escape? str i)
      (and (<=fx i (-fx (string-length str) 4))
	   (char=? #\\ (string-ref str i))
	   (char=? #\x (string-ref str (+fx i 1)))
	   (char-hex? (string-ref str (+fx i 2)))
	   (char-hex? (string-ref str (+fx i 3)))))
   (define (u-escape? str i)
      (and (<=fx i (-fx (string-length str) 6))
	   (char=? #\\ (string-ref str i))
	   (char=? #\u (string-ref str (+fx i 1)))
	   (char-hex? (string-ref str (+fx i 2)))
	   (char-hex? (string-ref str (+fx i 3)))
	   (char-hex? (string-ref str (+fx i 4)))
	   (char-hex? (string-ref str (+fx i 5)))))
   (define (contains-escaped-chars? str)
      (let loop ((i 0))
	 (cond
	    ((=fx i (string-length str))
	     #f)
	    ((or (x-escape? str i)
		 (u-escape? str i))
	     #t)
	    (else (loop (+fx i 1))))))

   (define (unescape str)
      (let loop ((i 0)
		 (rev-chars '()))
	 (cond
	    ((=fx i (string-length str))
	     (list->string (reverse! rev-chars)))
	    ((x-escape? str i)
	     (let ((d1 (hex-char->number (string-ref str (+fx i 2))))
		   (d2 (hex-char->number (string-ref str (+fx i 3)))))
		(loop (+fx i 4)
		      (cons (integer->char (+fx (*fx 16 d1) d2)) rev-chars))))
	    ((u-escape? str i)
	     (let* ((d1 (hex-char->number (string-ref str (+fx i 2))))
		    (d2 (hex-char->number (string-ref str (+fx i 3))))
		    (d3 (hex-char->number (string-ref str (+fx i 4))))
		    (d4 (hex-char->number (string-ref str (+fx i 5))))
		    (n1 (+fx (*fx 16 d1) d2))
		    (n2 (+fx (*fx 16 d3) d4))
		    (n (+fx (*fx n1 256) n2))
		    (tmp-str (make-string 4))
		    (nb-chars (utf8-string-set! tmp-str 0 n)))
		(let liip ((j 0)
			   (rev-cs '()))
		   (cond
		      ((=fx j nb-chars)
		       (loop (+fx i 6)
			     (append! rev-cs rev-chars)))
		      (else (liip (+fx j 1)
				  (cons (string-ref tmp-str j) rev-cs)))))))
	    (else (loop (+fx i 1) (cons (string-ref str i) rev-chars))))))
   (let ((needs-unescaping? (and unescape?
				 (contains-escaped-chars? str))))
      (if (not needs-unescaping?)
	  str
	  (unescape str))))
(define-inline (js-string?::bool str) (string? str))
(define-inline (utf8->js-string::js-string utf8-str) utf8-str)
(define-inline (js-string->utf8 str::js-string) str)
(define-inline (js-string=ascii? js-str ascii-str) (string=? js-str ascii-str))
(define-inline (js-string-ref str::js-string i::long)
   (string-ref str i))
(define-inline (js-string=? str1::js-string str2::js-string)
   (string=? str1 str2))
(define-inline (js-string-hash::long str::js-string)
   (string-hash str))

(define-inline (js-string-null?::bool str::js-string)
   (string-null? str))

(define-inline (js-string-length::long str::js-string)
   (string-length str))

(define-inline (js-substring::js-string str::js-string
					     from::long to::long)
   (substring str from to))

(define-macro (js-string-append . L)
   `(string-append ,@L))
(define (js-string-append::js-string . L)
   (apply string-append L))

(define-inline (js-string<?::bool str1::js-string str2::js-string)
   (string<? str1 str2))

(define-inline (js-ascii-substring-at?::bool str1::bstring
					     str2::js-string
					     pos::long)
   (substring-at? str1 str2 pos))

(define-inline (js-substring-at?::bool str1::js-string
				       str2::js-string
				       pos::long)
   (substring-at? str1 str2 pos))

(define-inline (js-string->real::double str::js-string)
   (string->real str))
(define-inline (js-string->integer::long str::js-string
					 #!optional (base #f))
   (if base
       (string->integer str base)
       (string->integer str)))

(define-inline (integer->js-string::js-string i::long)
   (integer->string i))
(define-inline (real->js-string::js-string d::double)
   (real->string d))
(define-inline (llong->js-string::js-string l::llong #!optional (base #f))
   (if base
       (llong->string l base)
       (llong->string l)))

;; 'inline', since it is used at only one place anyways and this way Bigloo can
;; type.
(define-inline (vector->js-string v)
   (let* ((len (vector-length v))
	  (str (make-string len)))
      (let loop ((i 0))
	 (if (>= i len)
	     str
	     (let* ((cv (vector-ref v i)))
		(string-set! str i (any->uint8fx (vector-ref v i)))
		(loop (+fx i 1)))))))

;; c is guaranteed to be a char retrieved by 'js-string-ref'.
(define-inline (js-char->js-string c::js-char)
   (string c))

(define-inline (js-string-contains str1::js-string str2::js-string
				   #!optional (pos::long 0))
   (string-contains str1 str2 pos))

(define-inline (js-string-compare3 str1::js-string str2::js-string)
   (string-compare3 str1 str2))

;; downcase must work in unicode (if the implementation allows UC)
(define-inline (js-string-downcase::js-string str::js-string)
   (utf8-downcase str))

;; upcase must work in unicode (if the implementation allows UC)
(define-inline (js-string-upcase::js-string str::js-string)
   (utf8-upcase str))

;; raises an error if the string is not a correct Unicode string.
;; the iterator (a closure) must return integers or #f (eos).
(define (js-string-uc-iterator str::js-string)
   (utf8-uc-iterator str))

(define (open-js-string-buffer size)
   (open-utf8-string-buffer size))

(define (js-string-buffer-uc-push! buf c::long)
   (utf8-buffer-uc-push! buf c))

;; js-c must be a char that has been received by js-string-ref.
(define (js-string-buffer-verbatim-push! buf js-c::js-char)
   (utf8-buffer-verbatim-push! buf js-c))

(define (close-js-string-buffer buf)
   (close-utf8-buffer buf))

;; how many "native" chars does the given uc-char use?
(define-inline (js-string-uc-char-size uc-c::long)
   (utf8-char-length uc-c))
