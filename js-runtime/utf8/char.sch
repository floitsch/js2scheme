(directives
   (type (alias char js-char))
   (export
    (inline js-char-max::long)
    (inline js-char?::bool c) ;; can't make difference between char and js-char
    (inline js-char->integer::int c::js-char)
    (inline integer->js-char::js-char i::int)
    (inline char->js-char::js-char c::char) ;; identity
    (inline js-char->char::char c::js-char) ;; identity
    (inline js-char-upcase::js-char c::js-char)
    (inline js-char-downcase::js-char c::js-char)
    (inline js-char=?::bool c1::js-char c2::js-char)
    (inline js-char>=?::bool c1::js-char c2::js-char)
    (inline js-char<=?::bool c1::js-char c2::js-char)
    (inline js-char=char?::bool c1::js-char c2::char)
    (inline char=js-char?::bool c1::char c2::js-char)
    (inline js-char>=char?::bool c1::js-char c2::char)
    (inline js-char<=char?::bool c1::js-char c2::char)
    (inline js-char-whitespace?::bool c::js-char)
    ))

(define-inline (js-char-max) 256)

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

(define-inline (js-char-whitespace? c) (char-whitespace? c))
