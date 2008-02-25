(module jsre-RegExp-parse
;   (main my-main)
   (export (js-regexp->scm-regexp pattern::bstring)))

;; Bigloo's regexp-parser does not feature multi-lines... so we can't reuse it.

;; TODO: IdentityEscape talks about SourceCharacter but not IdentifierPart
;;       The way I see it, this excludes a bunch of characters.
;;       Normally all other escape-prefixes are thus unique (if it starts with
;;       a 'x' it must be a hex-escape).
;; TODO: hex-conversion down the code.

(define (js-regexp->scm-regexp js-pattern)
(bind-exit (return)
   ;; 15.10.1
   ;; function-body at bottom of fun

(define str js-pattern)
(define str-len (string-length str))
(define pos 0)

;; takes care of #f
(define (c=? c1 c2)
   (and c1 c2 (char=? c1 c2)))

(define (c-hex? c)
   (and (char? c)
	(or (and (char>=? c #\a)
		 (char<=? c #\f))
	    (and (char>=? c #\A)
		 (char<=? c #\F))
	    (and (char>=? c #\0)
		 (char<=? c #\9)))))

(define (c-numeric? c)
   (and c (char-numeric? c)))

(define (char->symbol c)
   (string->symbol (string c)))


(define (current-pos)
   pos)

(define (next-char!)
   (let ((tmp (peek-char)))
      (when tmp
	 (set! pos (+fx pos 1)))
      tmp))

(define (back!)
   (set! pos (-fx pos 1)))

(define (consume-char!)
   (next-char!))

(define (peek-char)
   (if (< pos str-len)
       (string-ref str pos)
       #f))

(define (read-number)
      (let ((start-pos (current-pos)))
	 (let loop ()
	    (when (c-numeric? (peek-char))
	       (consume-char!)
	       (loop)))
	 (let* ((end-pos (current-pos)))
	    (if (=fx start-pos end-pos)
		(return #f)
		(string->integer (substring str start-pos end-pos))))))
   

(define (disjunction)
   (let loop ((rev-res (list (alternative))))
      (if (c=? (peek-char) #\|)
	  (begin
	     (consume-char!) ;; eat '|'
	     (loop (cons (alternative) rev-res)))
	  (if (null? (cdr rev-res))
	      (car rev-res)
	      (cons 'disjunction (reverse! rev-res))))))

(define (alternative)
   (let loop ((rev-res '()))
      (let ((c (peek-char)))
	 (case c
	    ((#f #\| #\))
	     ;; end of string, | for next alternative, or end of cluster
	     (cons 'sequence (reverse! rev-res)))
	    (else
	     (loop (cons (term) rev-res)))))))

(define (term)
   (let ((c (peek-char)))
      ;; do assertion directly in here:
      (case c
	 ((#\^ #\$) ;; ^ or $ (begin/end of line)
	  (consume-char!)
	  `(,(char->symbol c)))
	 ((#\\) ;; either \b \B or \AtomEscape
	  (consume-char!)
	  (let ((c (peek-char)))
	     (cond
		;; Remaining Assertions \b and \B
		((c=? c #\b) (consume-char!)
			     '(word-boundary))
		((c=? c #\B) (consume-char!)
			     '(not-word-boundary))
		(else
		 (back!)
		 (quantified-atom)))))
	 (else
	  (quantified-atom)))))

(define (quantified-atom)
   (let* ((at (atom))
	  (c (peek-char)))
      (case c
	 ((#\* #\+ #\? #\{)
	  (let ((quant (quantifier)))
	     (if (c=? (peek-char) #\?)
		 (begin
		    ;; non greedy
		    (consume-char!)
		    `(quantified ,at ,quant #f))
		 ;; greedy
		 `(quantified ,at ,quant #t))))
	 (else at))))

(define (quantifier)
   (let ((c (consume-char!)))
      (case c
	 ((#\*) '(repetitions 0 #f))
	 ((#\?) '(repetitions 0 1))
	 ((#\+) '(repetitions 1 #f))
	 (else ;; {
	  (let* ((n1 (read-number))
		 (n2 n1))
	     (when (c=? #\, (peek-char))
		(consume-char!)
		(if (c-numeric? (peek-char))
		    (begin
		       (set! n2 (read-number))
		       (unless (<=fx n1 n2)
			  (return #f)))
		    (set! n2 #f)))
	     (if (c=? (consume-char!) #\})
		 `(repetitions ,n1 ,n2)
		 (return #f)))))))
	  
(define (atom)
   (let ((c (peek-char)))
      (case c
	 ((#\.) (consume-char!)
		'(any))
	 ((#\\) (atom-escape))
	 ((#\() (cluster))
	 ((#\[) (character-class))
	 (else (consume-char!)
	       `(char ,c)))))

(define (atom-escape)
   (consume-char!)
   (let ((c (peek-char)))
      (cond
	 ((not c) (return #f))
	 ;; \ AtomEscape with AtomEscape either:
	 ;;   DecimalEscape
	 ;;   CharacterClassEscape
	 ;;   CharacterEscape
	 ;; ---
	 ;; DecimalEscape
	 ;; 15.10.2.11
	 ((char-numeric? c)
	  (decimal-escape))
	 ;; CharacterClassEscape
	 ;; 15.10.2.12
	 ((string-index "dDsSwW" c) ;; c is one of "dDsSwW"
	  (character-class-escape))
	 (else
	  (character-escape)))))

(define (character-escape)
   (let ((c (peek-char)))
      (cond
	 ;; 15.10.2.10
	 ;; Character-Escape with either:
	 ;;  ControlEscape
	 ;;  c ControlLetter
	 ;;  HexEscapeSequence
	 ;;  UnicodeEscapeSequence
	 ;;  IdentityEscape
	 ;; ---
	 ((string-index "tnvfr" c)
	  (control-escape))
	 ;; c ControlLetter
	 ((c=? c #\c)
	  ;; must be followed by a-zA-Z
	  (consume-char!)
	  (control-letter))
	 ((or (c=? c #\x)
	      (c=? c #\u))
	  (consume-char!)
	  (hex-escape (if (c=? c #\x) 2 4)))
	 ;; IdentityEscape
	 (else
	  ;; TODO: get chars of IdentifierPart.
	  ;;       for now just accept it.
	  `(char ,(consume-char!))))))
   
(define (back-reference)
   ;; back-reference
   (let ((start (current-pos)))
      (consume-char!)
      (let ((n (read-number)))
	 `(back-ref ,n))))

(define (decimal-escape)
   (let ((c (peek-char)))
      (if (c=? c #\0) ;; must not be followed by digit.
	  (begin
	     ;; null-char
	     (consume-char!)
	     (if (c-numeric? (peek-char))
		 (return #f)
		 '(char #\null)))
	  (back-reference))))

;; we known already, that (peek-char) is one of "dDsSwW".
(define (character-class-escape)
   (let ((c (consume-char!)))
      (cond
	 ((c=? c #\d) '(number))
	 ((c=? c #\D) '(not-number))
	 ((c=? c #\s) '(white-space))
	 ((c=? c #\S) '(not-white-space))
	 ((c=? c #\w) '(a-zA-Z0-9_))
	 ((c=? c #\W) '(not-a-zA-Z0-9_)))))

;; we known already, that (peek-char) is one of "tnvfr".
(define (control-escape)
   (let ((c (consume-char!)))
      (cond
	 ((c=? c #\t) `(char (string->char "\t")))
	 ((c=? c #\n) `(char (string->char "\n")))
	 ((c=? c #\v) `(char (string->char "\v")))
	 ((c=? c #\f) `(char (string->char "\f")))
	 ((c=? c #\r) `(char (string->char "\r"))))))

(define (control-letter)
   (if (not (char-alphabetic? (peek-char)))
       (return #f)
       (let* ((c (consume-char!))
	      (n (char->integer c))
	      (rem (modulo c 32)))
	  (if (zero? rem)
	      '(char #\null)
	      `(char ,(integer->char rem))))))

(define (hex-escape nb-hexs)
   (let ((start-pos (current-pos))
	 (end-pos (+fx nb-hexs (current-pos))))
      ;; verify und consume
      (let loop ((i (current-pos)))
	 (when (<fx i end-pos)
	    (if (not (c-hex? (consume-char!)))
		(return #f)
		(loop (+fx i 1)))))
      (let ((hex-str (substring str start-pos end-pos)))
	 `(char ,(integer->char (string->integer hex-str 16))))))


(define (cluster)
   (consume-char!) ;; the (
   (let* ((c1 (consume-char!))  ;; maybe ?
	  (c2 (consume-char!))) ;; maybe :, = or !
      (cond
	 ((or (not c1)
	      (not c2)
	      (not (c=? c1 #\?))
	      (not (string-index ":=!" c2)))
	  (back!)
	  (back!)
	  (let* ((d (disjunction))
		 (c (consume-char!)))
	     (if (not (c=? c #\)))
		 (return #f)
		 `(cluster ,d))))
	 ((c=? c2 #\:)
	  (let* ((d (disjunction))
		 (c (consume-char!)))
	     (if (not (c=? c #\)))
		 (return #f)
		 d)))
	 ((c=? c2 #\=)
	  (let* ((d (disjunction))
		 (c (consume-char!)))
	     (if (not (c=? c #\)))
		 (return #f)
		 `(pos-lookahead-cluster ,d))))
	 ((c=? c2 #\!)
	  (let* ((d (disjunction))
		 (c (consume-char!)))
	     (if (not (c=? c #\)))
		 (return #f)
		 `(neg-lookahead-cluster ,d)))))))

(define (character-class)
   (consume-char!) ;; the [
   (let ((invert? (c=? (peek-char) #\^)))
      (when invert?
	 (consume-char!))
      ;; first just get elements
      ;; we will build ranges afterwards
      (let ((chars (let loop ((rev-res '()))
		      (let ((c (consume-char!)))
			 (case c
			    ((#f) (return #f))
			    ((#\]) (back!)
				   (reverse! rev-res))
			    ((#\\) (loop (cons (class-escape) rev-res)))
			    (else (loop (cons c rev-res))))))))
	 ;; now build the ranges.
	 (let loop ((res chars))
	    (unless (or (null? res)
			(null? (cdr res))
			(null? (cddr res)))
	       (match-case res
		  (((and (? char?) ?c-from) #\- (and (? char?) ?c-to) ???-)
		   (let ((c-from-n (char->integer c-from))
			 (c-to-n (char->integer c-to)))
		      (unless (<=fx c-from-n c-to-n)
			 (return #f))
		      (set-car! res `(range ,c-from-n ,c-to-n))
		      (set-cdr! res (cdddr res)))))
	       (loop (cdr res))))
	 ;; chars is now a list of chars and ranges.
	 `(class ,chars ,invert?))))

(define (class-escape)
   (let ((c (peek-char)))
      (cond
	 ((not c)                   (return #f))
	 ;; inside classes decimal-escapes must not be back-refs.
	 ;; -> only choice left is "\0"
	 ((char-numeric? c)         (let ((d (decimal-escape)))
				       (unless (char? d)
					   (return #f))
				       d))
	 ((c=? c #\b)               `(char ,(integer->char 8))) ;; backspace
	 ((string-index "dDsSwW" c) (character-class-escape))
	 (else                      (character-escape)))))


;;=======================================
;; fun starts here
;;=======================================
(let ((res (disjunction)))
   (if (<fx (current-pos) str-len)
       #f
       res))))

; (define (my-main args)
;    (pp (js-regexp->scm-regexp "^ab(c|d)*e+?[^f][g-i-j]$k(?:l|mno)$"))
;    (pp (js-regexp->scm-regexp "^ab(c|d)*e{3}?"))
;    (pp (js-regexp->scm-regexp "abc{4,3}"))
;    (pp (js-regexp->scm-regexp "c{1,}"))
;    (pp (js-regexp->scm-regexp "c{1,2}"))
;    (pp (js-regexp->scm-regexp "c{1,2}}"))
;    (pp (js-regexp->scm-regexp "c{1,2}{3,2}"))
;    (pp (js-regexp->scm-regexp "c{1,2}{3,2}"))
;    (pp (js-regexp->scm-regexp "c)")))
