(module jsre-RegExp-parse
   (import jsre-base-string)
   (use jsre-base-object
	jsre-conversion)
;   (main my-main)
   (export (js-regexp->scm-regexp pattern::js-string)))

;; Bigloo's regexp-parser does not feature multi-lines... so we can't reuse it.

;; TODO: IdentityEscape talks about SourceCharacter but not IdentifierPart
;;       The way I see it, this excludes a bunch of characters.
;;       Normally all other escape-prefixes are thus unique (if it starts with
;;       a 'x' it must be a hex-escape).
;; TODO: hex-conversion down the code.

(define-macro (js-char-case c . Lclauses)
   (let ((gs (gensym 'char)))
      `(let ((,gs ,c))
	  (case (if (and (js-char? ,gs)
			 (< (js-char->integer ,gs) 256))
		    (js-char->char ,gs)
		    ,gs)
	     ,@Lclauses))))

(define (js-regexp->scm-regexp js-pattern)
(bind-exit (return)
   ;; 15.10.1
   ;; function-body at bottom of fun

(define str js-pattern)
(define str-len (js-string-length str))
(define pos 0)

;; takes care of #f
(define (c=? c1 c2)
   (and c1 c2 (js-char=? c1 c2)))
(define (cc=? c1 c2)
   [assert (c1 c2) (and (or (not c1) (js-char? c1))
			(or (not c2) (char? c2)))]
   (and c1 c2 (js-char=char? c1 c2)))

(define (cchar>=? c1::js-char c2::char)
   (>= (js-char->integer c1) (char->integer c2)))
(define (cchar<=? c1::js-char c2::char)
   (>= (js-char->integer c1) (char->integer c2)))

(define (cc-any-of? c::js-char chars::bstring)
   (and (< (js-char->integer c) 256)
	(string-index chars (js-char->char c))
	#t))

(define (c-hex? c)
   (and (js-char? c)
	(or (and (cchar>=? c #\a)
		 (cchar<=? c #\f))
	    (and (cchar>=? c #\A)
		 (cchar<=? c #\F))
	    (and (cchar>=? c #\0)
		 (cchar<=? c #\9)))))

;; numeric is only for decimal chars ('0'-'9')
(define (c-numeric? c)
   (and c
	(let ((ci (js-char->integer c)))
	   (and (>=fx ci (char->integer #\0))
		(<=fx ci (char->integer #\9))))))
;; alphabetic is only for ascii chars.
(define (c-alphabetic? c)
   (and c
	(let ((ci (js-char->integer c)))
	   (or (and (>=fx ci (char->integer #\a))
		    (<=fx ci (char->integer #\z))))
	   (or (and (>=fx ci (char->integer #\A))
		    (<=fx ci (char->integer #\Z)))))))

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
   (if (<fx pos str-len)
       (js-string-ref str pos)
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
		(js-string->integer (js-substring str start-pos end-pos))))))
   

(define (disjunction)
   (let loop ((rev-res (list (alternative))))
      (if (cc=? (peek-char) #\|)
	  (begin
	     (consume-char!) ;; eat '|'
	     (loop (cons (alternative) rev-res)))
	  (if (null? (cdr rev-res))
	      (car rev-res)
	      (cons ':or (reverse! rev-res))))))

(define (alternative)
   (let loop ((rev-res '()))
      (let ((c (peek-char)))
	 (js-char-case c
	    ((#f #\| #\))
	     ;; end of string, | for next alternative, or end of cluster
	     (cons ':seq (reverse! rev-res)))
	    (else
	     (loop (cons (term) rev-res)))))))

(define (term)
   (let ((c (peek-char)))
      ;; do assertion directly in here:
      (js-char-case c
	 ((#\^) (consume-char!)
		':bol) ;; begin of line/string
	 ((#\$) (consume-char!)
		':eol) ;; end of line/string
	 ((#\\) ;; either \b \B or \AtomEscape
	  (consume-char!)
	  (let ((c (peek-char)))
	     (js-char-case c
		;; Remaining Assertions \b and \B
		((#\b) (consume-char!)
		       ':wbdry)
		((#\B) (consume-char!)
		       ':not-wbdry)
		(else
		 (back!)
		 (quantified-atom)))))
	 (else
	  (quantified-atom)))))

(define (quantified-atom)
   (let* ((at (atom))
	  (c (peek-char)))
      (js-char-case c
	 ((#\* #\+ #\? #\{)
	  (receive (min max)
	     (quantifier)
	     (if (cc=? (peek-char) #\?)
		 (begin
		    ;; non greedy
		    (consume-char!)
		    `(:quantified #f ,min ,max ,at))
		 ;; greedy
		 `(:quantified #t ,min ,max ,at))))
	 (else at))))

(define (quantifier)
   (let ((c (consume-char!)))
      (js-char-case c
	 ((#\*) (values 0 #f))
	 ((#\?) (values 0 1))
	 ((#\+) (values 1 #f))
	 (else ;; {
	  (let* ((n1 (read-number))
		 (n2 n1))
	     (when (cc=? (peek-char) #\,)
		(consume-char!)
		(if (c-numeric? (peek-char))
		    (begin
		       (set! n2 (read-number))
		       (unless (<=fx n1 n2)
			  (return #f)))
		    (set! n2 #f)))
	     (if (cc=? (consume-char!) #\})
		 (values n1 n2)
		 (return #f)))))))
	  
(define (atom)
   (let ((c (peek-char)))
      (js-char-case c
	 ((#\.) (consume-char!)
		':any)
	 ((#\\) (atom-escape))
	 ((#\() (cluster))
	 ((#\[) (character-class))
	 (else  (consume-char!)
		c))))

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
	 ((c-numeric? c)
	  (decimal-escape))
	 ;; CharacterClassEscape
	 ;; 15.10.2.12
	 ((cc-any-of? c "dDsSwW")
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
	 ((cc-any-of? c "tnvfr")
	  (control-escape))
	 ;; c ControlLetter
	 ((cc=? c #\c)
	  ;; must be followed by a-zA-Z
	  (consume-char!)
	  (control-letter))
	 ((or (cc=? c #\x)
	      (cc=? c #\u))
	  (consume-char!)
	  (hex-escape (if (cc=? c #\x) 2 4)))
	 ;; IdentityEscape
	 (else
	  ;; TODO: get chars of IdentifierPart.
	  ;;       for now just accept it.
	  (consume-char!)))))
   
(define (back-reference)
   ;; back-reference
   (let ((start (current-pos)))
      (let ((n (read-number)))
	 `(:backref ,n))))

(define (decimal-escape)
   (let ((c (peek-char)))
      (if (cc=? c #\0) ;; must not be followed by digit.
	  (begin
	     ;; null-char
	     (consume-char!)
	     (if (c-numeric? (peek-char))
		 (return #f)
		 `(char ,(char->js-char #\null))))
	  (back-reference))))

;; we know already that (peek-char) is one of "dDsSwW".
(define (character-class-escape)
   (let ((c (consume-char!)))
      (cond
	 ((cc=? c #\d) ':digit)
	 ((cc=? c #\D) ':not-digit)
	 ((cc=? c #\s) ':space)
	 ((cc=? c #\S) ':not-space)
	 ((cc=? c #\w) ':word)
	 ((cc=? c #\W) ':not-word))))

;; we know already, that (peek-char) is one of "tnvfr".
(define (control-escape)
   (let ((c (consume-char!)))
      (cond
	 ((cc=? c #\t) (char->js-char (string-ref "\t" 0)))
	 ((cc=? c #\n) (char->js-char (string-ref "\n" 0)))
	 ((cc=? c #\v) (char->js-char (string-ref "\v" 0)))
	 ((cc=? c #\f) (char->js-char (string-ref "\f" 0)))
	 ((cc=? c #\r) (char->js-char (string-ref "\r" 0))))))

(define (control-letter)
   (if (not (c-alphabetic? (peek-char)))
       (return #f)
       (let* ((c (consume-char!))
	      (n (js-char->integer c))
	      (rem (modulo c 32)))
	  (if (zero? rem)
	      #\null
	      (integer->js-char rem)))))

(define (hex-escape nb-hexs)
   (let ((start-pos (current-pos))
	 (end-pos (+fx nb-hexs (current-pos))))
      ;; verify und consume
      (let loop ((i (current-pos)))
	 (when (<fx i end-pos)
	    (if (not (c-hex? (consume-char!)))
		(return #f)
		(loop (+fx i 1)))))
      (let ((hex-str (js-substring str start-pos end-pos)))
	 (integer->js-char (js-string->integer hex-str 16)))))


(define (cluster)
   (consume-char!) ;; the (
   (let* ((c1 (consume-char!))  ;; maybe ?
	  (c2 (consume-char!))) ;; maybe :, = or !
      (cond
	 ((or (not c1)
	      (not c2)
	      (not (cc=? c1 #\?))
	      (not (cc-any-of? c2 ":=!")))
	  (back!)
	  (back!)
	  (let* ((d (disjunction))
		 (c (consume-char!)))
	     (if (not (cc=? c #\)))
		 (return #f)
		 `(:cluster ,d))))
	 ((cc=? c2 #\:)
	  (let* ((d (disjunction))
		 (c (consume-char!)))
	     (if (not (cc=? c #\)))
		 (return #f)
		 d)))
	 ((cc=? c2 #\=)
	  (let* ((d (disjunction))
		 (c (consume-char!)))
	     (if (not (cc=? c #\)))
		 (return #f)
		 `(:pos-lookahead-cluster ,d))))
	 ((cc=? c2 #\!)
	  (let* ((d (disjunction))
		 (c (consume-char!)))
	     (if (not (cc=? c #\)))
		 (return #f)
		 `(:neg-lookahead-cluster ,d)))))))

(define (character-class)
   (consume-char!) ;; the [
   (let ((invert? (cc=? (peek-char) #\^)))
      (when invert?
	 (consume-char!))
      ;; first just get elements
      ;; we will build ranges afterwards
      (let ((chars (let loop ((rev-res '()))
		      (let ((c (consume-char!)))
			 (js-char-case c
			    ((#f)  (return #f))
			    ((#\]) (reverse! rev-res)) ;; leave the ']' consumed
			    ((#\\) (loop (cons (class-escape) rev-res)))
			    (else  (loop (cons c rev-res))))))))
	 ;; now build the ranges.
	 (let loop ((res chars))
	    (unless (or (null? res)
			(null? (cdr res))
			(null? (cddr res)))
	       (match-case res
		  (((and (? js-char?) ?c-from) #\- (and (? js-char?) ?c-to) ???-)
		   (let ((c-from-n (js-char->integer c-from))
			 (c-to-n (js-char->integer c-to)))
		      (unless (<=fx c-from-n c-to-n)
			 (return #f))
		      (set-car! res `(:char-range ,c-from-n ,c-to-n))
		      (set-cdr! res (cdddr res)))))
	       (loop (cdr res))))
	 ;; chars is now a list of chars and ranges.
	 (if invert?
	     `(:neg-char (:one-of-chars ,@chars))
	     `(:one-of-chars ,@chars)))))

(define (class-escape)
   (let ((c (peek-char)))
      (cond
	 ((not c)                   (return #f))
	 ;; inside classes decimal-escapes must not be back-refs.
	 ;; -> only choice left is "\0"
	 ((c-numeric? c)            (let ((d (decimal-escape)))
				       (unless (and (pair? d)
						    (eq? (car d) 'char))
					  (return #f))
				       d))
	 ((cc=? c #\b)              `(char ,(integer->char 8))) ;; backspace
	 ((cc-any-of? c "dDsSwW")   (character-class-escape))
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
