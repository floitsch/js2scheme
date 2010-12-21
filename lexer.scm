(module lexer
   (library utf)
   (export *JS-grammar*
	   *care-future-reserved*
	   *Reg-exp-grammar*
	   (reserved-word?::bool symbol::symbol)
	   (inline token-type token)
	   (inline token-val token)
	   (inline token-pos token)))

(define (lexer-supports-unicode?) #t)

(define (the-coord input-port)
   (list 'at (input-port-name input-port) (input-port-position input-port)))

(define *care-future-reserved* #t)

(define *keyword-list*
   '("as"
     "break"
     "case"
     "catch"
     "class"
     "const"
     "continue"
     "default"
     "delete"
     "do"
     "else"
     "extends"
     "false"
     "finally"
     "for"
     "function"
     "if"
     "import"
     "in"
     "instanceof"
     "is"
     "namespace"
     "new"
     "null"
     "package"
     "private"
     "public"
     "return"
     "super"
     "switch"
     "this"
     "throw"
     "true"
     "try"
     "typeof"
     "use"
     "var"
     "void"
     "while"
     "with"))

(define *future-reserved-list*
   '("abstract"
     "debugger"
     "enum"
     "export"
     "goto"
     "implements"
     "interface"
     "native"
     "protected"
     "synchronized"
     "throws"
     "transient"
     "volatile"))

(for-each (lambda (word)
	     (putprop! (string->symbol word) 'reserved #t))
	  *keyword-list*)
(for-each (lambda (word)
	     (putprop! (string->symbol word) 'future-reserved #t))
	  *future-reserved-list*)

(define (reserved-word? symbol)
   (or (getprop symbol 'reserved)
       (and *care-future-reserved*
	    (getprop symbol 'future-reserved))))

(define-macro (token type value)
   `(econs ,type ,value (the-coord input-port)))
(define-inline (token-type token) (car token))
(define-inline (token-val token) (cdr token))
(define-inline (token-pos token) (and (epair? token) (cer token)))

(define (unescape-unicode-escape-sequences str)
   (define (surrogate-1st? n) (=fx (bit-and n #xFC00) #xD800))
   (define (surrogate-2nd? n) (=fx (bit-and n #xFC00) #xFC00))
   (define (surrogate? n) (or (surrogate-1st? n) (surrogate-2nd? n)))

   (let ((i (string-contains str "\\u")))
      (if (not i)
	  str
	  (let ((first-part (substring str 0 i))
		(u-part (string->integer (substring str (+fx i 2) (+fx i 6))
					 16))
		(last-part (substring str (+fx i 6) (string-length str))))
	     (cond
		((not (surrogate? u-part))
		 (let* ((buffer (make-string 4))
			(len (utf8-string-set! buffer 0 u-part)))
		    (unescape-unicode-escape-sequences
		     (string-append first-part
				    (string-shrink! buffer len)
				    last-part))))
		((and (surrogate-1st? u-part)
		      (>=fx (string-length str) (+fx i 12))
		      (char=? #\\ (string-ref str (+fx i 6)))
		      (char=? #\u (string-ref str (+fx i 7)))
		      (let* ((sub-str (substring str (+fx i 8) (+fx i 12)))
			     (u-part2 (string->integer sub-str 16)))
			 (and (surrogate-2nd? u-part2)
			      u-part2)))
		 =>
		 (lambda (u-part2)
		    (let ((ucs2-buffer (make-ucs2-string 2))
			  (u1 (integer->ucs2-ur u-part))
			  (u2 (integer->ucs2-ur u-part2)))
		       (ucs2-string-set! ucs2-buffer 0 u1)
		       (ucs2-string-set! ucs2-buffer 1 u2)
		       (unescape-unicode-escape-sequences
			(string-append first-part
				       (utf16-string->utf8-string ucs2-buffer)
				       last-part)))))
		(else #f))))))
   
(define *JS-grammar*
   (utf8-regular-grammar
	 ((source-char (out))
	  (blank_no_lt  (or (in #x9 #xB #xC #x20 #xA0) Zs))
	  (lt           (in  #a013 #\Newline #x2028 #x2029))
	  (not-lt       (out #a013 #\Newline #x2028 #x2029))
	  (not-lt-no-single-quote-no-bs (out #a013 #\Newline #x2028 #x2029 #\' #\\))
	  (not-lt-no-double-quote-no-bs (out #a013 #\Newline #x2028 #x2029 #\" #\\))
	  (blank        (or blank_no_lt lt))
	  (nonzero-digit   (in ("19")))
	  (unicode-letter (or Lu Ll Lt Lm Lo Nl))
	  (unicode-combining-mark (or Mn Mc))
	  (unicode-digit Nd)
	  (unicode-connector-punctuation Pc)
	  (hex-digit (or (in ("09")) (in ("af")) (in ("AF"))))
	  (unicode-escape-sequence (: #\u hex-digit hex-digit
					  hex-digit hex-digit))
	  (id_start (or unicode-letter #\$ #\_
			(: #\\ unicode-escape-sequence)))
	  (id_part (or id_start unicode-combining-mark unicode-digit
		       unicode-connector-punctuation
		       (: #\\ unicode-escape-sequence))))


      ((+ blank_no_lt)
       (ignore))

      ((: (* blank_no_lt) lt (* blank))
       (token 'NEW_LINE #\newline))
      
      ;; LineComment
      ((:"//" (* not-lt))
       (ignore))

      ;; multi-line comment on one line
      ((: "/*" (* (or (out #\*) (: (+ #\*) (out #\/ #\*)))) (+ #\*) "/")
       (ignore))

      ;; multi-line comment with LineTerminators (several lines)
      ((: "/*"
	  (* (or lt
		 (out #\*)
		 (: (+ #\*) (out #\/ #\*))))
	  (+ #\*) "/")
       (token 'NEW_LINE #\newline))

      ;; unfinished multi-line comment (added for repl)
      ((eof (: "/*"
	       (* (or lt
		      (out #\*)
		      (: (+ #\*) (out #\/ #\*))))))
       (token 'ERROR 'eof))

      ((or
	;; integer constant
	#\0
	(: nonzero-digit (* digit))
	(: #\0 (in #\x #\X) (+ xdigit))
	;; floating-point constant
	(: (+ digit)
	   (: (in #\e #\E) (? (in #\- #\+)) (+ digit)))
	(: (or (: (+ digit) #\. (* digit)) (: #\. (+ digit)))
	   (? (: (in #\e #\E) (? (in #\- #\+)) (+ digit)))))
       (token 'NUMBER (the-string)))

      (#\{   (token 'LBRACE #\{))
      (#\}   (token 'RBRACE #\}))
      (#\(   (token 'LPAREN #\())
      (#\)   (token 'RPAREN #\)))
      (#\[   (token 'LBRACKET #\[))
      (#\]   (token 'RBRACKET #\]))
      (#\.   (token 'DOT #\.))
      (#\;   (token 'SEMICOLON #\;))
      (#\,   (token 'COMMA #\,))
      (#\|   (token 'BIT_OR #\|))
      ("||"  (token 'OR "||"))
      ("|="  (token 'BIT_OR= "|="))
      ((or #\< #\> "<=" ">=" "==" "!=" "===" "!==" #\+ #\- #\* #\% "++" "--"
	   "<<" ">>" ">>>" #\& #\^ #\! #\~ "&&" #\: #\= "+=" "-="  
	   "*=" "%=" "<<=" ">>=" ">>>=" "&=" "^=" "/=" #\/ #\?)
       (token (the-symbol) (the-string)))

      ((: #\" (* (or not-lt-no-double-quote-no-bs (: #\\ source-char))) #\")
       (token 'STRING (the-string)))
      ((: #\' (* (or not-lt-no-single-quote-no-bs (: #\\ source-char))) #\')
       (token 'STRING (the-string)))

      ;; Identifiers and Keywords
      ((: id_start (* id_part))
       (let* ((symbol (the-symbol)))
	  (cond
	     ((getprop symbol 'reserved)
	      (token symbol symbol))
	     ((and *care-future-reserved*
		   (getprop symbol 'future-reserved))
	      (token symbol symbol))
	     (else
	      (let* ((str (the-string))
		     (unescaped-str (unescape-unicode-escape-sequences str)))
		 (cond
		    ((not unescaped-str)
		     (token 'ERROR #f))
		    ((string=? str unescaped-str)
		     (token 'ID symbol))
		    (else
		     ;; try to read it again.
		     (let ((tkn (read/rp *JS-grammar*
					 (open-input-string unescaped-str)))
			   (unescaped-sym (string->symbol unescaped-str)))
			(cond
			   ((and (eq? 'ID (car tkn))
				 (eq? unescaped-sym (cdr tkn)))
			    (token 'ID unescaped-sym))
			   ((eq? 'ERROR (car tkn))
			    (token 'ERROR (cdr tkn)))
			   (else
			    (token 'ERROR #f)))))))))))
      ;; error
      (else
       (let ((c (the-failure)))
	  (if (eof-object? c)
	      (cons 'EOF c)
	      (token 'ERROR c))))))

(define *Reg-exp-grammar*
   (utf8-regular-grammar
	 ((lt           (in #a013 #\Newline #x2028 #x2029))
	  (not-lt (out #a013 #\Newline #x2028 #x2029))
	  (not-/-bs-lt (out #a013 #\Newline #x2028 #x2029 #\/ #\\))
	  (unicode-letter (or Lu Ll Lt Lm Lo Nl))
	  (unicode-combining-mark (or Mn Mc))
	  (unicode-digit Nd)
	  (unicode-connector-punctuation Pc)
	  (hex-digit (or (in ("09")) (in ("af")) (in ("AF"))))
	  (unicode-escape-sequence (: #\u hex-digit hex-digit
					  hex-digit hex-digit))
	  (id_start (or unicode-letter #\$ #\_
			(: #\\ unicode-escape-sequence)))
	  (id_part (or id_start unicode-combining-mark unicode-digit
		       unicode-connector-punctuation
		       (: #\\ unicode-escape-sequence))))
      ((: (* (or not-/-bs-lt (: #\\ not-lt))) #\/ (* id_part))
       (let ((str (the-string)))
	  (let loop ((i 0))
	     (cond
		((char=? #\\ (string-ref str i)) (loop (+fx i 2)))
		((not (char=? #\/ (string-ref str i))) (loop (+fx i 1)))
		((not (string-contains str "\\u" i))
		 (token 'REG-EXP str))
		(else
		 (let* ((reg-part (substring str 0 (+fx i 1)))
			(flags-part (substring str (+fx i 1)
					       (string-length str)))
			(unescaped (unescape-unicode-escape-sequences
				    flags-part)))
		    (if (not unescaped)
			(token 'ERROR 'unicode-error)
			(token 'REG-EXP
			       (string-append reg-part unescaped)))))))))
      (else
       (let ((c (the-failure)))
	  (if (eof-object? c)
	      (token 'EOF c)
	      (token 'ERROR c))))))
