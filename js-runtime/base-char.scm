(module jsre-base-char
   (type
    (alias char js-char))
   (export
    (inline js-char?::bool c) ;; can't make difference between char and js-char
    (inline js-char->integer::int c::js-char)
    (inline integer->js-char::js-char i::int)
    (inline char->js-char::js-char c::char) ;; identity
    (inline js-char->char::char c::js-char) ;; identity
    (inline js-char-upcase::js-char c::js-char)
    (inline js-char-downcase::js-char c::js-char)
    (inline js-char=?::bool c1::js-char c2::js-char)
    (inline js-char>=? c1 c2)
    (inline js-char<=? c1 c2)
    (inline js-char=char?::bool c1::js-char c2::char)
    (inline char=js-char?::bool c1::char c2::js-char)
    (inline js-char>=char?::bool c1::js-char c2::char)
    (inline js-char<=char?::bool c1::js-char c2::char)
    (inline js-char-numeric?::bool c::js-char)
    (inline js-char-alphabetic?::bool c::js-char)
    (inline js-char-whitespace?::bool c::js-char)
    (inline js-char-lowercase?::bool c::js-char)
    (inline js-char-uppercase?::bool c::js-char)
    (inline js-char-byte?::bool c::js-char)
    (macro js-char-case)
    
    ))
   

(define-inline (js-char->integer c)
   (char->integer c))

(define-inline (integer->js-char i)
   (integer->char i))

(define-inline (char->js-char c)
   c)

;; c is guaranteed to fit into an ascii-char.
(define-inline (js-char->char c)
   c)

;; in the case of UTF16 (which is the only one specified in the spec) the
;; result of this procedure must either return the upcase version of c, _or_ c
;; itself if the result does not fit into one 16bit char.
;; The spec does not handle surogate utf16 chars. As a result one must expect
;; separate surrogate pairs as entry. These (invalid) 16bit chars should
;; (probably?) be returned verbatim.
(define-inline (js-char-upcase c)
   (char-upcase c))

;; see js-char-upcase
(define-inline (js-char-downcase c)
   (char-downcase c))

(define-inline (js-char=? c1 c2)
   (char=? c1 c2))
(define-inline (js-char>=? c1 c2)
   (char>=? c1 c2))
(define-inline (js-char<=? c1 c2)
   (char<=? c1 c2))
(define-inline (js-char=char? c1 c2)
   (js-char=? c1 (char->js-char c2)))
(define-inline (char=js-char? c1 c2)
   (js-char=? (char->js-char c1) c2))
(define-inline (js-char>=char? c1 c2)
   (char>=? c1 c2))
(define-inline (js-char<=char? c1 c2)
   (char<=? c1 c2))

(define-inline (js-char? c)
   (char? c))

(define-inline (js-char-numeric? c)
   ;; TODO
   (let ((cn (char->integer c)))
      (and (< cn 256)
	   (char-numeric? c))))
(define-inline (js-char-alphabetic? c)
   ;; TODO
   (let ((cn (char->integer c)))
      (and (<fx cn 256)
	   (char-alphabetic? c))))
(define-inline (js-char-whitespace? c)
   ;; TODO
   (let ((cn (char->integer c)))
      (and (<fx cn 256)
	   (char-whitespace? c))))
(define-inline (js-char-lowercase? c)
   ;; TODO
   (let ((cn (char->integer c)))
      (and (<fx cn 256)
	   (char-lower-case? c))))
(define-inline (js-char-uppercase? c)
   ;; TODO
   (let ((cn (char->integer c)))
      (and (<fx cn 256)
	   (char-upper-case? c))))

(define-inline (js-char-byte? c)
   #t)

(define-macro (js-char-case c . Lclauses)
   (let* ((else-clause (assq 'else Lclauses))
	  (else-sym (gensym 'else))
	  (int-c-sym (gensym 'cv)))
      `(let ((,else-sym (lambda () ,@(if else-clause
					 (cdr else-clause)
					 '(#unspecified)))))
	  (if (and (js-char? ,c)
		   (< 256 (js-char->integer ,c)))
	      (case (js-char->char ,c)
		 ,@(map (lambda (clause)
			   (if (eq? (car clause) 'else)
			       `(else (,else-sym))
			       clause))
			Lclauses))
	      (,else-sym)))))

