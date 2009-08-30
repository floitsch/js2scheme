(module lexer
   (library utf)
   (export *JS-grammar*
	   *care-future-reserved*
	   *Reg-exp-grammar*
	   (reserved-word?::bool symbol::symbol)))

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

(define *JS-grammar*
   (utf8-regular-grammar
	 ((source-char (out))
	  (blank_no_lt  (or (in #x9 #xB #xC #x20 #xA0) Zs))
	  (lt           (in #a013 #\Newline #x2028 #x2029))
	  (not-lt-no-single-quote (out #a013 #\Newline #x2028 #x2029 #\'))
	  (not-lt-no-double-quote (out #a013 #\Newline #x2028 #x2029 #\"))
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
      ((:"//" (* source-char))
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

      ;; TODO: verify if this is really the correct syntax
      ((or
	;; integer constant
	#\0
	(: nonzero-digit (* digit))
	(: (uncase "0x") (+ xdigit))
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

      ((: #\" (* (or not-lt-no-double-quote (: #\\ source-char))) #\")
       (token 'STRING (the-string)))
      ((: #\' (* (or not-lt-no-single-quote (: #\\ source-char))) #\')
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
	      (token 'ID symbol)))))
      
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
       (cons 'Reg-exp (the-string)))
      (else
       (let ((c (the-failure)))
	  (if (eof-object? c)
	      (cons 'EOF c)
	      (cons 'ERROR c))))))
