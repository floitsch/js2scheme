;; Copyright (c) 2007-2011, Florian Loitsch
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are met:
;;    * Redistributions of source code must retain the above copyright
;;      notice, this list of conditions and the following disclaimer.
;;    * Redistributions in binary form must reproduce the above copyright
;;      notice, this list of conditions and the following disclaimer in the
;;      documentation and/or other materials provided with the distribution.
;;    * Neither the name of the <organization> nor the
;;      names of its contributors may be used to endorse or promote products
;;      derived from this software without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;; DISCLAIMED. IN NO EVENT SHALL <COPYRIGHT HOLDER> BE LIABLE FOR ANY
;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
;; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
;; ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(module jsre-base-char
   (export
    (inline js-char-max::long)
    (inline js-char?::bool c) ;; can't make difference between char and js-char
    (inline js-char->integer::int c::ucs2)
    (inline integer->js-char::ucs2 i::int)
    (inline char->js-char::ucs2 c::char)
    (inline js-char->char::char c::ucs2)
    (inline js-char-upcase::ucs2 c::ucs2)
    (inline js-char-downcase::ucs2 c::ucs2)
    (inline js-char=?::bool c1::ucs2 c2::ucs2)
    (inline js-char>=?::bool c1::ucs2 c2::ucs2)
    (inline js-char<=?::bool c1::ucs2 c2::ucs2)
    (inline js-char=char?::bool c1::ucs2 c2::char)
    (inline char=js-char?::bool c1::char c2::ucs2)
    (inline js-char>=char?::bool c1::ucs2 c2::char)
    (inline js-char<=char?::bool c1::ucs2 c2::char)
    (inline js-char-whitespace?::bool c::ucs2)
    ))

(define-inline (js-char-max) #xFFFF)

(define-inline (js-char->integer c)
   (ucs2->integer c))

(define-inline (integer->js-char i) (integer->ucs2-ur i))

(define-inline (char->js-char c)
   (integer->js-char (char->integer c)))

;; c is guaranteed to fit into an ascii-char.
(define-inline (js-char->char c)
   (ucs2->char c))

;; in the case of UTF16 (which is the only one specified in the spec) the
;; result of this procedure must either return the upcase version of c, _or_ c
;; itself if the result does not fit into one 16bit char.
;; The spec does not handle surogate utf16 chars. As a result one must expect
;; separate surrogate pairs as entry. These (invalid) 16bit chars should
;; (probably?) be returned verbatim.
(define-inline (js-char-upcase c)
   (let ((tmp (unchecked-uc-upper (ucs2->integer c))))
      (if (<fx tmp 0)
	  c
	  (integer->js-char tmp))))
;; see js-char-upcase
(define-inline (js-char-downcase c)
   (let ((tmp (unchecked-uc-lower (ucs2->integer c))))
      (if (<fx tmp 0)
	  c
	  (integer->js-char tmp))))

(define-inline (js-char=? c1 c2)
   (ucs2=? c1 c2))
(define-inline (js-char>=? c1 c2)
   (ucs2>=? c1 c2))
(define-inline (js-char<=? c1 c2)
   (ucs2<=? c1 c2))
(define-inline (js-char=char? c1 c2)
   (js-char=? c1 (char->js-char c2)))
(define-inline (char=js-char? c1 c2)
   (js-char=? (char->js-char c1) c2))
(define-inline (js-char>=char? c1 c2)
   (ucs2>=? c1 (char->js-char c2)))
(define-inline (js-char<=char? c1 c2)
   (ucs2<=? c1 (char->js-char c2)))

(define-inline (js-char? c)
   (ucs2? c))

(define-inline (js-char-whitespace? c) (uc-whitespace? (ucs2->integer c)))
