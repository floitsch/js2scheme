(directives
   (type (alias ucs2string Js-Base-String))
   (import jsre-conversion
	   jsre-base-object)
   (from utf)
   (export
    (macro ascii->js-string-literal)
    (utf8->js-string-literal::Js-Base-String str::bstring #!optional (unescape? #f))
    (inline js-string?::bool str)
    (inline utf8->js-string::Js-Base-String utf8-str::bstring)
    (inline js-string->utf8::bstring str::Js-Base-String)
    (inline js-string=ascii?::bool js-str::Js-Base-String utf8-str::bstring)
    (inline js-string-ref::js-char str::Js-Base-String i::long)
    (inline js-string=?::bool str1::Js-Base-String str2::Js-Base-String)
    (js-string-hash::long str::Js-Base-String)
    (inline js-string-null?::bool str::Js-Base-String)
    (inline js-string-length::long str::Js-Base-String)
    (js-substring::Js-Base-String str::Js-Base-String
				  from::long to::long)
    (js-string-append::Js-Base-String . Lstrs)
    (inline js-string<?::bool str1::Js-Base-String str2::Js-Base-String)
    (js-ascii-substring-at?::bool str1::bstring str::Js-Base-String
	    pos::long)
    (inline js-substring-at?::bool str1::Js-Base-String str::Js-Base-String
	    pos::long)
    (js-string->real::double str::Js-Base-String)
    (js-string->integer::long str::Js-Base-String #!optional (base #f))
    (integer->js-string::Js-Base-String i::long)
    (real->js-string::Js-Base-String d::double)
    (llong->js-string::Js-Base-String i::llong #!optional (base #f))
    (inline vector->js-string::Js-Base-String v::vector)
    (inline js-char->js-string::Js-Base-String c::js-char)
    (js-string-contains str1::Js-Base-String str2::Js-Base-String
			#!optional (pos::long 0))
    (js-string-compare3::long str1::Js-Base-String str2::Js-Base-String)
    (inline js-string-downcase::Js-Base-String str::Js-Base-String)
    (inline js-string-upcase::Js-Base-String str::Js-Base-String)
    (js-string-uc-iterator str::Js-Base-String)
    (open-js-string-buffer size)
    (js-string-buffer-uc-push! buf c::long)
    (js-string-buffer-verbatim-push! buf js-c::js-char)
    (close-js-string-buffer buf)
    (inline js-string-uc-char-size uc-c::long)
    ))

(define-macro (ascii->js-string-literal str) (utf8-string->ucs2-string str))

(define (utf8->js-string-literal str #!optional (unescape? #f))
   (define (char-hex? c)
      (let ((ci (ucs2->integer c)))
	 (and (<fx ci 128)
	      (case (integer->char ci)
		 ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9
		       #\a #\b #\c #\d #\e #\f
		       #\A #\B #\C #\D #\E #\F) #t)
		 (else #f)))))
   (define (hex-char->number c)
      (let ((c (ucs2->char c)))
	 (case c
	    ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
	     (-fx (char->integer c) (char->integer #\0)))
	    ((#\a #\b #\c #\d #\e #\f)
	     (+fx 10 (-fx (char->integer c) (char->integer #\a))))
	    (else
	     (+fx 10 (-fx (char->integer c) (char->integer #\A)))))))
   (define (x-escape? str i)
      (and (<=fx i (-fx (ucs2-string-length str) 4))
	   (=fx (char->integer #\\) (ucs2->integer (ucs2-string-ref str i)))
	   (=fx (char->integer #\x)
		(ucs2->integer (ucs2-string-ref str (+fx i 1))))
	   (char-hex? (ucs2-string-ref str (+fx i 2)))
	   (char-hex? (ucs2-string-ref str (+fx i 3)))))
   (define (u-escape? str i)
      (and (<=fx i (-fx (ucs2-string-length str) 6))
	   (=fx (char->integer #\\) (ucs2->integer (ucs2-string-ref str i)))
	   (=fx (char->integer #\u)
		(ucs2->integer (ucs2-string-ref str (+fx i 1))))
	   (char-hex? (ucs2-string-ref str (+fx i 2)))
	   (char-hex? (ucs2-string-ref str (+fx i 3)))
	   (char-hex? (ucs2-string-ref str (+fx i 4)))
	   (char-hex? (ucs2-string-ref str (+fx i 5)))))
   (define (contains-escaped-chars? str)
      (let loop ((i 0))
	 (cond
	    ((=fx i (ucs2-string-length str))
	     #f)
	    ((or (x-escape? str i)
		 (u-escape? str i))
	     #t)
	    (else (loop (+fx i 1))))))
   (define (unescape str)
      (let loop ((i 0)
		 (rev-chars '()))
	 (cond
	    ((=fx i (ucs2-string-length str))
	     (list->ucs2-string (reverse! rev-chars)))
	    ((x-escape? str i)
	     (let ((d1 (hex-char->number (ucs2-string-ref str (+fx i 2))))
		   (d2 (hex-char->number (ucs2-string-ref str (+fx i 3)))))
		(loop (+fx i 4)
		      (cons (integer->16bit-char (+fx (*fx 16 d1) d2)) rev-chars))))
	    ((u-escape? str i)
	     (let* ((a (hex-char->number (ucs2-string-ref str (+fx i 2))))
		    (b (hex-char->number (ucs2-string-ref str (+fx i 3))))
		    (c (hex-char->number (ucs2-string-ref str (+fx i 4))))
		    (d (hex-char->number (ucs2-string-ref str (+fx i 5))))
		    (n (+fx d (*fx 16 (+fx c (*fx 16 (+fx b (*fx 16 a))))))))
		(loop (+fx i 6)
		      (cons (integer->16bit-char n) rev-chars))))
	    (else (loop (+fx i 1) (cons (ucs2-string-ref str i) rev-chars))))))
   (let* ((utf16-str (utf8-string->utf16-string str))
	  (needs-unescaping? (and unescape?
				  (contains-escaped-chars? utf16-str))))
       (if needs-unescaping?
	  (unescape utf16-str)
	  utf16-str)))

(define-inline (Js-Base-String?::bool str)
   (ucs2-string? str))
(define-inline (js-string?::bool str)
   (ucs2-string? str))

(define-inline (utf8->js-string::Js-Base-String utf8-str)
   (utf8-string->utf16-string utf8-str))

(define-inline (js-string->utf8 str::Js-Base-String)
   (utf16-string->utf8-string str))

(define-inline (js-string=ascii? js-str ascii-str)
   (and (=fx (string-length ascii-str) (ucs2-string-length js-str))
	(let loop ((i 0))
	   (cond
	      ((=fx i (string-length ascii-str))
	       #t)
	      ((not (=fx (char->integer (string-ref ascii-str i))
			 (ucs2->integer (ucs2-string-ref js-str i))))
	       #f)
	      (else (loop (+fx i 1)))))))

(define-inline (js-string-ref str::Js-Base-String i::long)
   (ucs2-string-ref str i))

(define-inline (js-string=? str1::Js-Base-String str2::Js-Base-String)
   (ucs2-string=? str1 str2))

(define (js-string-hash::long str::Js-Base-String)
   ;; TODO: replace with ucs2-string-hash once it exists.
   (let loop ((i 0)
	      (res 0))
      (if (=fx i (ucs2-string-length str))
	  (absfx res)
	  (loop (+fx i 1)
		(+fx res (+fx (bit-lsh res 3)
			      (ucs2->integer (ucs2-string-ref str i))))))))

(define-inline (js-string-null?::bool str::Js-Base-String)
   (zerofx? (ucs2-string-length str)))

(define-inline (js-string-length::long str::Js-Base-String)
   (ucs2-string-length str))

(define (js-substring::Js-Base-String str::Js-Base-String
				      from::long to::long)
   (let ((str-len (ucs2-string-length str))
	 (len (-fx to from)))
      (when (not (and (<=fx from to)
		      (>=fx from 0)
		      (<=fx to str-len)))
	 (error 'js-substring
		"Substring requires: 0<=from<=to<=str-len"
		(list 0 from to str-len)))
      (let ((res-str (make-ucs2-string len)))
	 (let loop ((i 0))
	    (if (=fx i len)
		(let ((tmp::Js-Base-String res-str)) tmp)
		(begin
		   (ucs2-string-set! res-str i
				     (ucs2-string-ref str (+fx from i)))
		   (loop (+fx i 1))))))))

(define (js-string-append::Js-Base-String . L)
   (define (combined-length L)
      (let loop ((L L)
		 (res 0))
	 (if (null? L)
	     res
	     (loop (cdr L)
		   (+fx (ucs2-string-length (car L))
			res)))))
   (define (blit! str target at)
      (let ((str-len (ucs2-string-length str)))
	 (let loop ((i 0))
	    (if (=fx i str-len)
		(+fx at i)
		(begin
		   (ucs2-string-set! target (+fx at i) (ucs2-string-ref str i))
		   (loop (+fx i 1)))))))
   (let ((res (make-ucs2-string (combined-length L))))
      (let loop ((L L)
		 (i 0))
	 (if (null? L)
	     (let ((tmp::Js-Base-String res)) tmp)
	     (loop (cdr L)
		   (blit! (car L) res i))))))

(define-inline (js-string<?::bool str1::Js-Base-String str2::Js-Base-String)
   (ucs2-string<? str1 str2))

(define (js-ascii-substring-at?::bool str1::bstring
				      str2::Js-Base-String
				      pos::long)
   (let ((str-len1 (string-length str1))
	 (str-len2 (ucs2-string-length str2)))
      (and (<=fx (+fx str-len1 pos) str-len2)
	   (let loop ((i 0))
	      (if (=fx i str-len1)
		  #t
		  (and (=fx (char->integer (string-ref str1 i))
			    (ucs2->integer (ucs2-string-ref str2 (+fx i pos))))
		       (loop (+fx i 1))))))))

(define-inline (js-substring-at?::bool str1::Js-Base-String
				       substring::Js-Base-String
				       pos::long)
   (let ((str-len1 (ucs2-string-length str1))
	 (sub-len (ucs2-string-length substring)))
      (and (<=fx (+fx sub-len pos) str-len1)
	   (let loop ((i 0))
	      (if (=fx i sub-len)
		  #t
		  (and (ucs2=? (ucs2-string-ref substring i)
			       (ucs2-string-ref str1 (+fx i pos)))
		       (loop (+fx i 1))))))))

(define (ascii-ucs2-str->bstring str)
   (let* ((str-len (ucs2-string-length str))
	  (t (make-string str-len)))
      (let loop ((i 0))
	 (if (=fx i str-len)
	     t
	     (begin
		(string-set! t i (integer->char
				  (ucs2->integer (ucs2-string-ref str i))))
		(loop (+fx i 1)))))))
(define (ascii-str->ucs2-str str::bstring)
   (let* ((str-len (string-length str))
	  (res (make-ucs2-string str-len)))
      (let loop ((i 0))
	 (if (=fx i str-len)
	     res
	     (begin
		(ucs2-string-set! res i (integer->js-char
					 (char->integer
					  (string-ref str i))))
		(loop (+fx i 1)))))))

;; precondition: str must only contain valid real-chars.
;; in particular they must be ascii chars.
(define (js-string->real::double str::Js-Base-String)
   (string->real (ascii-ucs2-str->bstring str)))

;; speed is not important. happens only during regexp-parse.
(define (js-string->integer::long str::Js-Base-String
				  #!optional (base #f))
   (let ((str (ascii-ucs2-str->bstring str)))
      (if base
	  (string->integer str base)
	  (string->integer str))))

(define (integer->js-string::Js-Base-String i::long)
   (case i
      ((0)  #u"0")  ((1)  #u"1")  ((2) #u"2") ((3) #u"3")
      ((4)  #u"4")  ((5)  #u"5")  ((6) #u"6") ((7) #u"7")
      ((8)  #u"8")  ((9)  #u"9")  ((10) #u"10") ((11) #u"11")
      ((12) #u"12") ((13) #u"13") ((14) #u"14") ((15) #u"15")
      ((16) #u"16") ((17) #u"17") ((18) #u"18") ((19) #u"19")
      (else
       (ascii-str->ucs2-str (integer->string i)))))

;; speed is not important. Only used in Error.
(define (real->js-string::Js-Base-String d::double)
   (ascii-str->ucs2-str (real->string d)))

(define (llong->js-string::Js-Base-String l::llong #!optional (base #f))
   (if base
       (ascii-str->ucs2-str (llong->string l base))
       (ascii-str->ucs2-str (llong->string l))))

;; 'inline', since it is used at only one place anyways and this way Bigloo can
;; type.
(define-inline (vector->js-string::Js-Base-String v)
   (let* ((len (vector-length v))
	  (str (make-ucs2-string len)))
      (let loop ((i 0))
	 (if (>= i len)
	     (let ((tmp::Js-Base-String str)) tmp)
	     (let* ((cv (vector-ref v i)))
		(ucs2-string-set! str i (any->uint16fx (vector-ref v i)))
		(loop (+fx i 1)))))))

(define-inline (js-char->js-string c)
   (ucs2-string c))

;; for now stupid n^2 implementation.
(define (js-string-contains str1::Js-Base-String str2::Js-Base-String
			    #!optional (pos::long 0))
   (let* ((str1-len (ucs2-string-length str1))
	  (str2-len (ucs2-string-length str2))
	  (stop (-fx str1-len str2-len)))
      ;; is str2 substring at str1[at]
      (define (substring-at? at)
	 (let loop ((i 0))
	    (cond
	       ((=fx i str2-len)
		#t)
	       ((ucs2=? (ucs2-string-ref str1 (+fx at i))
			(ucs2-string-ref str2 i))
		(loop (+fx i 1)))
	       (else #f))))

      (let loop ((i pos))
	 (cond
	    ((>fx i stop) #f)
	    ((substring-at? i) i)
	    (else (loop (+fx i 1)))))))

(define (js-string-compare3 str1::Js-Base-String str2::Js-Base-String)
   (let ((str1-len (ucs2-string-length str1))
	 (str2-len (ucs2-string-length str2)))
      (if (<fx str2-len str1-len)
	  (* -1 (js-string-compare3 str2 str1))
	  (let loop ((i 0))
	     (cond
		((and (=fx i str1-len)
		      (=fx i str2-len))
		 0)
		((=fx i str1-len)
		 -1)
		((ucs2>? (ucs2-string-ref str1 i) (ucs2-string-ref str2 i))
		 1)
		((ucs2<? (ucs2-string-ref str1 i) (ucs2-string-ref str2 i))
		 -1)
		(else
		 (loop (+fx i 1))))))))

;; downcase must work in unicode (if the implementation allows UC)
(define-inline (js-string-downcase::Js-Base-String str::Js-Base-String)
   (tprint "downcasing " str)
   (utf16-downcase str))

;; upcase must work in unicode (if the implementation allows UC)
(define-inline (js-string-upcase::Js-Base-String str::Js-Base-String)
   (utf16-upcase str))

;; raises an error if the string is not a correct Unicode string.
;; the iterator (a closure) must return integers or #f (eos).
(define (js-string-uc-iterator str::Js-Base-String)
   (utf16-uc-iterator str))

(define (open-js-string-buffer size)
   (open-utf16-string-buffer size))

(define (js-string-buffer-uc-push! buf c::long)
   (utf16-buffer-uc-push! buf c))

;; js-c must be a char that has been received by js-string-ref.
(define (js-string-buffer-verbatim-push! buf js-c::js-char)
   (utf16-buffer-verbatim-push! buf js-c))

(define (close-js-string-buffer buf)
   (close-utf16-buffer buf))

;; how many "native" chars does the given uc-char use?
(define-inline (js-string-uc-char-size uc-c::long)
   (utf16-char-length uc-c))
