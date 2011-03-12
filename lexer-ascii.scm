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

(module lexer
   (export *JS-grammar*
	   *care-future-reserved*
	   *Reg-exp-grammar*
	   (reserved-word?::bool symbol::symbol)
	   (inline token-type token)
	   (inline token-val token)
	   (inline token-pos token)))

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

(define *JS-grammar*
   (regular-grammar
	 ;; TODO: are a010 and a013 really correct?
	 ((blank        (in #\Space #\Tab #a010 #a012 #a013 #\Newline))
	  (blank_no_lt  (in #\Space #\Tab #a012))
	  (lt           (in #a013 #\Newline))
	  (nonzero-digit   (in ("19")))
	  (id_start     (or alpha #\$ #\_))
	  (id_part      (or alnum #\$ #\_))) ;; TODO: not spec-conform!


      ((+ blank_no_lt)
       (ignore))

      ((: (* blank_no_lt) lt (* blank))
       (token 'NEW_LINE #\newline))
      
      ;; LineComment
      ((:"//" (* all))
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

      ;; TODO: probably not spec-conform
      ((: #\" (* (or (out #\" #\\ #\Newline) (: #\\ all))) #\")
       (token 'STRING (the-string)))
      ((: #\' (* (or (out #\' #\\ #\Newline) (: #\\ all))) #\')
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
      
      ;; TODO: add regular expressions

      ;; error
      (else
       (let ((c (the-failure)))
	  (if (eof-object? c)
	      (cons 'EOF c)
	      (token 'ERROR c))))))

(define *Reg-exp-grammar*
   (regular-grammar
	 ;; TODO: see TODOs for JS-grammar
	 ((lt           (in #a013 #\Newline))
	  (id_part      (or alnum #\$ #\_)))
      ((: (* (or (out #\/ #\\ lt) (: #\\ (out lt)))) #\/ (* id_part))
       (token 'REG-EXP (the-string)))
      (else
       (let ((c (the-failure)))
	  (if (eof-object? c)
	      (cons 'EOF c)
	      (cons 'ERROR c))))))
