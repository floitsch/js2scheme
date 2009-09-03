(module jsre-RegExp-classes
   (import jsre-RegExp-char-set
	   jsre-base-string)
   (use jsre-conversion
	jsre-base-object)
   (static
    (final-class RE-class
       (char-set::RE-char-set read-only)
       (constant?::bool (default #f) read-only)))
   (export
    (RegExp-class-pattern?::bool pattern)
    (RegExp-class-create char/class case-sensitive?)
    (RegExp-class-match re-class c::js-char)
    (RegExp-class-subset?::bool re-class1 re-class2)
    (RegExp-class-overlap?::bool re-class1 re-class2)
    (word-boundary str::js-string index::bint)
    (js-char-terminator? c::js-char)
    (canonicalize-char::js-char c::js-char ignore-case?::bool))
   (include "RegExp-constant-classes.scm"))

(define (canonicalize-char::js-char c::js-char ignore-case?::bool)
   (if (not ignore-case?)
       c
       (let* ((ci (js-char->integer c))
	      (u (unchecked-uc-upper ci)))
	  (cond
	     ((<fx u 0) c)
	     ((and (>=fx ci 128) (<fx u 128)) c)
	     ((>fx u (js-char-max)) ;; should never happen
	      c)
	     (else (integer->js-char u))))))

(define (RegExp-class-pattern? pattern)
   (match-case pattern
      ((or (? js-char?)
	   (? constant-class-pattern?)
	   (or (:neg-char ?-)
	       (:one-of-chars ???-)))
       #t)
      (else #f)))

(define (RegExp-class-match re-class c)
   (with-access::RE-class re-class (char-set)
      (char-set-match char-set (js-char->integer c))))

;; is cl1 a subset of cl2?
(define (RegExp-class-subset? cl1 cl2)
   (char-set-subset? (RE-class-char-set cl1)
		     (RE-class-char-set cl2)))

;; is the intersection of cl1 and cl2 not empty?
(define (RegExp-class-overlap? cl1 cl2)
   (char-set-overlap? (RE-class-char-set cl1)
		      (RE-class-char-set cl2)))

(define *empty-class* (instantiate::RE-class
			 (char-set (new-empty-char-set))
			 (constant? #t)))

(define *precomputed-size* 128)

;; precomputed char-classes.
(define *case-sensitive-classes* (make-vector *precomputed-size*))
(define *case-insensitive-classes* (make-vector *precomputed-size*))

;; in RegExp-constant-classes
(fill-sensitive-classes! *precomputed-size* *case-sensitive-classes*)
(fill-insensitive-classes! *precomputed-size*
			   *case-insensitive-classes* *case-sensitive-classes*)


;; in RegExp-constant-classes
(create-constant-classes
 (digit (#\0 #\9))
 (space #x09 #x0B #x0C #x20 #xA0)
 (not-any #xA #xD #x2028 #x2029) ;; last two ones can only work with 16bit chars.
 (word (#\a #\z) (#\A #\Z) (#\0 #\9) #\_)
 (xdigit (#\a #\f) (#\A #\F) (#\0 #\9))
 (alnum (#\a #\z) (#\A #\Z) (#\0 #\9))
 (alpha (#\a #\z) (#\A #\Z))
 (ascii (0 127))
 (blank #\space #\tab)
 (cntrl #x32)
 (graph (lambda () (filter (lambda (i)
			      (not (char-whitespace? (integer->char i))))
			   (iota (-fx 256 32) 32))))
 (lower (lambda () (filter (lambda (i) (char-lower-case? (integer->char i)))
			   (iota 256 0))))
 (upper (lambda () (filter (lambda (i) (char-upper-case? (integer->char i)))
			   (iota 256 0))))
 (print (32 256))
 (punct (lambda () (filter (lambda (i)
			      (let ((c (integer->char i)))
				 (not (or (char-whitespace? c)
					  (char-alphabetic? c)
					  (char-numeric? c)))))
			   (iota (-fx 256 32) 32))))
 )

(define (class-duplicate re-class::RE-class)
   (with-access::RE-class re-class (char-set)
      (instantiate::RE-class
	 (char-set (char-set-duplicate char-set)))))

(define (class-invert! re-class::RE-class)
   (if (RE-class-constant? re-class)
       (instantiate::RE-class
	  (char-set (char-set-invert (RE-class-char-set re-class))))
       (begin
	  (char-set-invert! (RE-class-char-set re-class))
	  re-class)))
   
(define (merge-classes! re-class1::RE-class re-class2::RE-class)
   (cond
      ((eq? re-class1 *empty-class*)
       re-class2)
      ((eq? re-class2 *empty-class*)
       re-class1)
      (else
       (let ((res (cond
		     ((not (RE-class-constant? re-class1))
		      re-class1)
		     ((not (RE-class-constant? re-class2))
		      re-class2)
		     (else
		      (instantiate::RE-class
			 (char-set (new-empty-char-set)))))))
	  (char-set-merge-into! (RE-class-char-set res)
				(RE-class-char-set re-class1)
				(RE-class-char-set re-class2))
	  res))))

(define (class-add-n! re-class::RE-class n::long)
   (with-access::RE-class re-class (char-set)
      (if (RE-class-constant? re-class)
	  (class-add-n! (class-duplicate re-class) n)
	  (begin
	     (char-set-add-n! char-set n)
	     re-class))))
(define (class-add-range-n! re-class::RE-class from::long to::long)
   (with-access::RE-class re-class (char-set)
      (if (RE-class-constant? re-class)
	  (class-add-range-n! (class-duplicate re-class) from to)
	  (begin
	     (char-set-add-range-n! (RE-class-char-set re-class) from to)
	     re-class))))

;; For now just build a range based on ASCII chars.
(define (RegExp-class-create char/class case-sensitive?)
   (define (add-n! re-class n)
      (class-add-n! re-class n))
   (define (add-char! re-class c)
      (add-n! re-class
	      (js-char->integer (canonicalize-char c case-sensitive?))))

   ;; handles case-sensitivity
   (define (add-char-range re-class from to)
      (let ((from (if (js-char? from)
		      (js-char->integer from)
		      from))
	    (to (if (js-char? to)
		    (js-char->integer to)
		    to)))
	 (let loop ((re-class re-class)
		    (from from))
	    (if (>= from to)
		re-class
		(loop (add-char! re-class (integer->js-char from))
		      (+fx from 1))))))

   ;; does not use case-sensitivity
   (define (add-range re-class from to)
      (let ((from (if (js-char? from)
		      (js-char->integer from)
		      from))
	    (to (if (js-char? to)
		    (js-char->integer to)
		    to)))
	 (class-add-range-n! re-class from to)))

   (define (add-element! re-class char/class)
      (match-case char/class
	 ((and (? js-char?) ?c)
	  (add-char! re-class c))
	 ((:range ?from ?to)
	  (add-range re-class from to))
	 ((:char-range ?from ?to)
	  (add-char-range re-class from to))
	 ((? constant-class-pattern?)
	  ;; in RegExp-constant-classes
	  (merge-constant-class re-class char/class))
	 ((:neg-char ?inverted-char/re-class)
	  (let ((tmp (add-element! *empty-class* inverted-char/re-class)))
	     (let ((tt (class-invert! tmp)))
		(merge-classes! re-class tt))))
;	     (merge-classes! re-class (class-invert! tmp))))
	 ((:one-of-chars . ?chars/classes)
	  (let loop ((re-class re-class)
		     (chars/classes chars/classes))
	     (if (null? chars/classes)
		 re-class
		 (let ((re-c (add-element! re-class (car chars/classes))))
		    (loop re-c (cdr chars/classes))))))
	 (else
	  (error "RegExp Class"
		 "forgot character class"
		 char/class))))

   (cond
      ((js-char? char/class) ;; covers the most important cases.
       (canonicalize-char char/class case-sensitive?))
      (else
       (let* ((re-class (add-element! *empty-class* char/class))
	      (sc (char-set-get-single-char (RE-class-char-set re-class))))
	  (if sc
	      (integer->js-char sc)
	      re-class)))))

(define (word-boundary str index)
   (define (word-char? c)
      (let ((ci (js-char->integer c)))
	 (or (and (>=fx ci (char->integer #\0))
		  (<=fx ci (char->integer #\9)))
	     (and (>=fx ci (char->integer #\a))
		  (<=fx ci (char->integer #\z)))
	     (and (>=fx ci (char->integer #\A))
		  (<=fx ci (char->integer #\Z)))
	     (=fx ci (char->integer #\_)))))
   
   (let ((len (js-string-length str)))
      (cond
	 ((zerofx? len)
	  #f)
	 ((zerofx? index)
	  (word-char? (js-string-ref str index)))
	 ((>=fx index len)
	  (word-char? (js-string-ref str (-fx len 1))))
	 ((word-char? (js-string-ref str (-fx index 1)))
	  (not (word-char? (js-string-ref str index))))
	 (else ;; index-1 is not word-char
	  (word-char? (js-string-ref str index))))))

(define (js-char-terminator? c)
   (let ((n (js-char->integer c)))
      (or (=fx n #xA) ;; Linefeed
	  (=fx n #xD) ;; Carriage Return
	  (=fx n #x2028) ;; Line separator
	  (=fx n #x2029)))) ;; Paragraph separator
