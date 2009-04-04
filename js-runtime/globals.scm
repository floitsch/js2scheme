(module jsre-globals
   (use jsre-base-object
	jsre-natives ;; undefined
	jsre-primitives
	jsre-Error
	jsre-Object
	jsre-Date
	jsre-Function
	jsre-String
	jsre-Number
	jsre-Bool
	jsre-conversion
	jsre-scope-object
	jsre-global-object
	)
   (export jsg-NaN
	   jsg-Infinity
	   jsg-undefined
	   jsg-print
	   jsg-scmprint
	   jsg-eval
	   (js-eval-orig::procedure)
	   jsg-isNaN
	   jsg-isFinite
	   jsg-parseInt
	   jsg-parseFloat
	   jsg-decodeURI
	   jsg-decodeURIComponent
	   jsg-encodeURI
	   jsg-encodeURIComponent
	   (globals-init)
	   ))

(define jsg-NaN #unspecified)
(define jsg-Infinity #unspecified)
(define jsg-undefined #unspecified)

(define *js-eval-orig* (lambda () 'to-be-replaced))
(define (js-eval-orig) *js-eval-orig*)

(define (globals-init)
   (set! jsg-NaN                            ;; 15.1.1.1
	 (create-special-global "NaN"
				(get-Attributes dont-enum dont-delete)
				+nan.0))

   (set! jsg-Infinity                       ;; 15.1.1.2
	 (create-special-global "Infinity"
				(get-Attributes dont-enum dont-delete)
				+inf.0))

   (set! jsg-undefined                      ;; 15.1.1.3
	 (create-special-global "undefined"
				(get-Attributes dont-enum dont-delete)
				(js-undefined)))

   (for-each (lambda (f) (f)) *delayed-assigs*)
   (set! *delayed-assigs* #unspecified)
   (set! *js-eval-orig* (global-read jsg-eval)))

(define-macro (define-runtime-globals . L)
   (let* ((ids (map (lambda (def)
		       (if (pair? (cadr def))
			   (caadr def)
			   (cadr def)))
		    L))
	  (exported-ids (map (lambda (id) (symbol-append 'jsg- id)) ids))
	  (vals (map (lambda (def id)
			(if (pair? (cadr def))
			    `(js-fun #f #f #f ,(symbol->string id)
				     ,(cdr (cadr def))
				     ,@(cddr def))
			    (caddr def)))
			L ids)))
      `(begin
	  ,@(map (lambda (exported-id id val)
		    `(begin
			(define ,exported-id #unspecified)
			(set! *delayed-assigs*
			      (cons (lambda ()
				       (set! ,exported-id
					     (create-runtime-global
					      ,(symbol->string id)
					      ,val)))
				    *delayed-assigs*))))
		 exported-ids
		 ids
		 vals))))

(define *delayed-assigs* '())

(define-runtime-globals
   (define (print to-print)
      (print (any->string to-print)))

   (define (scmprint to-print)
      (write-circle to-print)
      (print))

   (define (eval prog)              ;; 15.1.2.1
      (eval-error prog))

   (define (isNaN number)           ;; 15.1.2.4
      (nanfl? (any->number number)))

   (define (isFinite number)        ;; 15.1.2.5
      (let ((n (any->number number)))
	 (finitefl? n)))

   (define (parseInt string radix)  ;; 15.1.2.2
      (define (parseIntR s i R::bint sign::bint)
	 (let ((str-len (string-length s)))
	    (let loop ((i i)
		       (res 0)
		       (found-char #f))
	       (if (>= i str-len)
		   (if found-char
		       (* 1.0 (* res sign)) ;; (* 1.0) -> flonum
		       +nan.0)
		   (let* ((c (string-ref s i))
			  (ci (char->integer (string-ref s i)))
			  (cv (cond
				 ((and (char>=? c #\0)
				       (char<=? c #\9))
				  (-fx ci (char->integer #\0)))
				 ((and (char>=? c #\a)
				       (char<=? c #\z))
				  (+fx 10 (-fx ci (char->integer #\a))))
				 ((and (char>=? c #\A)
				       (char<=? c #\Z))
				  (+fx 10 (-fx ci (char->integer #\A))))
				 (else
				  99)))) ;; some big value
		      (cond
			 ((<fx cv R)
			  (loop (+ i 1)
				(+ (* R res) cv)
				#t))
			 ((and (not found-char)
			       (white-space? c))
			  (loop (+ i 1)
				res
				found-char))
			 (else ;; loop with i == str-len. -> finishes
			  (loop str-len
				res
				found-char))))))))
      
      (define (white-space? c)
	 ;; TODO: not spec-conform
	 (char-whitespace? c))

      (define (first-non-white s)
	 (let ((len (string-length s)))
	    (let loop ((i 0))
	       (cond
		  ((>=fx i len) i)
		  ((char-whitespace? (string-ref s i))
		   (loop (+fx i 1)))
		  (else i)))))

      (define (read-sign s pos)
	 (cond
	    ((>=fx pos (string-length s))
	     (values 1 pos))
	    ((char=? #\- (string-ref s pos))
	     (values -1 (+fx pos 1)))
	    ((char=? #\+ (string-ref s pos))
	     (values 1 (+fx pos 1)))
	    (else
	     (values 1 pos))))

      (define (read-0x s Rfx pos)
	 (cond
	    ((and (or (=fx Rfx 0)
		      (=fx Rfx 16))
		  (or (substring-at? s "0x" pos)
		      (substring-at? s "0X" pos)))
	     (values 16 (+fx pos 2)))
	    ((=fx Rfx 0)
	     (values 10 pos))
	    (else
	     (values Rfx pos))))

      (let* ((s (any->string string))
	     (tmp-R (any->int32 radix)))
	 (if (not (or (=fl tmp-R 0.0)
		      (and (>=fl tmp-R 2.0)
			   (<=fl tmp-R 36.0))))
	     +nan.0
	     (let ((pos (first-non-white s))
		   (Rfx (flonum->fixnum tmp-R)))
		(receive (sign pos)
		   (read-sign s pos)
		   (receive (R pos)
		      (read-0x s Rfx pos)
		      (parseIntR s pos R sign)))))))
   
   (define (parseFloat string) ;; 15.1.2.3
      (define decimal-char? char-numeric?)
      ;; TODO not spec-conform (whitespace)
      (define whitespace? char-whitespace?)
      
      (define (substring->real str str-len from to)
	 (cond
	    ((=fx from to) +nan.0)
	    ((and (=fx from 0)
		  (=fx to str-len))
	     (string->real str))
	    (else
	     (string->real (substring str from to)))))
      
      (define (read-ws str i str-len)
	 (cond
	    ((>= i str-len)
	     +nan.0)
	    ((whitespace? (string-ref str i))
	     (read-ws str (+fx i 1) str-len))
	    (else
	     (read-str-decimal-literal str i str-len))))
      
      (define (read-str-decimal-literal str i str-len)
	 ;; get rid of infinity.
	 (cond
	    ((>=fx i str-len) +nan.0)
	    ((substring-at? str "Infinity" i) +inf.0)
	    ((substring-at? str "+Infinity" i) +inf.0)
	    ((substring-at? str "-Infinity" i) -inf.0)
	    ((or (char=? #\- (string-ref str i))
		 (char=? #\+ (string-ref str i)))
	     (read-unsigned str (+fx i 1) str-len i))
	    (else
	     (read-unsigned str i str-len i))))
      
      (define (read-unsigned str i str-len start-pos)
	 (let loop ((i i)
		    (need-digit? #t)
		    (encountered-dot? #f))
	    (cond
	       ((and (>=fx i str-len)
		     need-digit?)
		+nan.0)
	       ((>=fx i str-len)
		(substring->real str str-len start-pos str-len))
	       ((decimal-char? (string-ref str i))
		(loop (+fx i 1)
		      #f
		      encountered-dot?))
	       ((char=? #\. (string-ref str i))
		(cond
		   ((and encountered-dot?
			 need-digit?)
		    +nan.0)
		   (encountered-dot?
		    (substring->real str str-len start-pos i))
		   (else
		    (loop (+fx i 1) need-digit? #t))))
	       ((or (char=? #\e (string-ref str i))
		    (char=? #\E (string-ref str i)))
		(if need-digit?
		    +nan.0
		    (read-exponent-integer str i str-len start-pos)))
	       (need-digit?
		+nan.0)
	       (else
		(substring->real str str-len start-pos i)))))
      
      (define (read-exponent-integer str i str-len start-pos)
	 (let loop ((i (+fx i 1)) ;; skip 'e'
		    (end-pos i)  ;; last-valid end-pos
		    (need-digit? #t)
		    (sign-is-allowed? #t))
	    (cond
	       ((>=fx i str-len)
		(substring->real str str-len start-pos end-pos))
	       ((decimal-char? (string-ref str i))
		(loop (+fx i 1)
		      (+fx i 1)
		      #f
		      #f))
	       ((and sign-is-allowed?
		     (or (char=? #\- (string-ref str i))
			 (char=? #\+ (string-ref str i))))
		(loop (+fx i 1)
		      end-pos
		      #t
		      #f))
	       (else
		(substring->real str str-len start-pos end-pos)))))
      
      (let ((str (any->string string)))
	 (read-ws str 0 (string-length str))))
   
   (define (decodeURI encodedURI)                     ;; 15.1.3.1
      (let ((str (any->string encodedURI)))
	 (uri-decode str (lambda (c)
			    (case c
			       ((#\; #\/ #\? #\: #\@ #\& #\= #\+ #\$ #\,)
				#t)
			       ((#\#) #t)
			       (else #f))))))
   
   (define (decodeURIComponent encodedURIComponent)   ;; 15.1.3.2
      (let ((str (any->string encodedURIComponent)))
	 (uri-decode str (lambda (c)
			    #f))))
   
   (define (encodeURI uri)                            ;; 15.1.3.3
      (let ((str (any->string uri)))
	 (uri-encode str
		     (lambda (c)
			(and (<fx c 128)
			     (case (integer->char c)
				((#\; #\/ #\? #\: #\@ #\& #\= #\+ #\$ #\,)
				 #t)
				((#\#)
				 #t)
				((#\a #\b #\c #\d #\e #\f #\g #\h #\i #\j
				      #\k #\l #\m #\n #\o #\p #\q #\r #\s
				      #\t #\u #\v #\w #\x #\y #\z)
				 #t)
				((#\A #\B #\C #\D #\E #\F #\G #\H #\I #\J
				      #\K #\L #\M #\N #\O #\P #\Q #\R #\S
				      #\T #\U #\V #\W #\X #\Y #\Z)
				 #t)
				((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
				 #t)
				((#\- #\_ #\. #\! #\~ #\* #\' #\( #\))
				 #t)
				(else #f)))))))
   
   (define (encodeURIComponent uriComponent)          ;; 15.1.3.4
      (let ((str (any->string uriComponent)))
	 (uri-encode str
		     (lambda (c)
			(and (<fx c 128)
			     (case (integer->char c)
				((#\a #\b #\c #\d #\e #\f #\g #\h #\i #\j
				      #\k #\l #\m #\n #\o #\p #\q #\r #\s
				      #\t #\u #\v #\w #\x #\y #\z)
				 #t)
				((#\A #\B #\C #\D #\E #\F #\G #\H #\I #\J
				      #\K #\L #\M #\N #\O #\P #\Q #\R #\S
				      #\T #\U #\V #\W #\X #\Y #\Z)
				 #t)
				((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
				 #t)
				((#\- #\_ #\. #\! #\~ #\* #\' #\( #\))
				 #t)
				(else #f)))))))
   )

;; 15.1.3 URI functions
(define-inline (uri-encode str in-unescaped-set?)
   (define (unicode-char-at str i)
      (char->integer (string-ref str i)))
   (define (display-uc-char c)
      (display (integer->char c)))

   (define (display-escaped-byte b)
      (define (hex-digit->char d)
	 (cond
	    ((<fx d 10)
	     (integer->char (+fx d (char->integer #\0))))
	    (else
	     (integer->char (+fx d (-fx (char->integer #\A) 10))))))
      (display* #\%
		(hex-digit->char (bit-ursh b 4))
		(hex-digit->char (bit-and b #x0F))))

   (let ((str-len (string-length str)))
      ;; TODO: uri-encode. do not create new string, if there is nothing to encode
      (with-output-to-string
	 (lambda ()
	    (let loop ((i 0))
	       (when (<fx i str-len)
		  (let ((c (unicode-char-at str i)))
		     (cond
			((not c) ;; can't happen yet
			 (uri-error "invalid unicode character"))
			;; the following would have been written completely
			;; different (and imho nicer) in C.
			((in-unescaped-set? c)
			 (display-uc-char c))
			((<=fx c #x7F)
			 (display-escaped-byte c))
			((<=fx c #x7FF)
			 (let ((b1 (+fx #xC0 (bit-ursh c 6)))
			       (b2 (+fx #x80 (bit-and c #x3F))))
			    (display-escaped-byte b1)
			    (display-escaped-byte b2)))
			((<=fx c #xFFFF)
			 (let ((b1 (+fx #xE0 (bit-ursh c 12)))
			       (b2 (+fx #x80
					(bit-and (bit-ursh c 6) #x3F)))
			       (b3 (+fx #x80 (bit-and c #x3F))))
			    (display-escaped-byte b1)
			    (display-escaped-byte b2)
			    (display-escaped-byte b3)))
			(else
			 (let ((b1 (+fx #xF0 (bit-ursh c 18)))
			       (b2 (+fx #x80
					(bit-and (bit-ursh c 12) #x3F)))
			       (b3 (+fx #x80
					(bit-and (bit-ursh c 6) #x3F)))
			       (b4 (+fx #x80 (bit-and c #x3F))))
			    (display-escaped-byte b1)
			    (display-escaped-byte b2)
			    (display-escaped-byte b3)
			    (display-escaped-byte b4)))))
		  (loop (+fx i 1))))))))

(define-inline (uri-decode str in-reserved-set?)
   ;; the str is supposed to be encoded. Therefore all chars should be ASCII.
   ;; we will therefore simply use string-ref to traverse the string.

   (define (display-uc-char c)
      (display (integer->char c)))

   (define (read-escaped-byte str i str-len)
      (define (hex-char->integer c)
	 (let ((v (char->integer c))
	       (v_0 (char->integer #\0))
	       (v_a (char->integer #\a))
	       (v_A (char->integer #\A)))
	    (cond
	       ((and (>=fx v v_0)
		     (<=fx v (char->integer #\9)))
		(-fx v v_0))
	       ((and (>=fx v v_A)
		     (<=fx v (char->integer #\F)))
		(-fx v (-fx v_A 10)))
	       ((and (>=fx v v_a)
		     (<=fx v (char->integer #\f)))
		(-fx v (-fx v_a 10)))
	       (else #f))))

      ;; (string-ref str i) == #\%
      (and (<fx (+fx i 2) str-len)
	   (let ((d1 (hex-char->integer (string-ref str (+fx i 1))))
		 (d2 (hex-char->integer (string-ref str (+fx i 2)))))
	      (and d1 d2
		   (+fx (bit-lsh d1 4) d2)))))

   ;; removes the leading 10 bits
   (define (read-non-head-escaped-byte str i str-len)
      (let ((b (read-escaped-byte str i str-len)))
	 (when (or (not b)
		   (not (=fx #x80 (bit-and #xC0 b)))) ;; starts with '10'
	    (uri-error "bad URI escape sequence 1"))
	 (bit-and #x3F b)))

   (define (read-escaped-sequence str i str-len)
      (let ((b1 (read-escaped-byte str i str-len)))
	 (when (not b1)
	    (uri-error "bad URI escape sequence 2"))
	 (cond
	    ((zerofx? (bit-and b1 #x80))
	     (values b1 (+fx i 3)))
	    ((=fx #xC0 (bit-and #xE0 b1)) ;; 2 bytes
	     (let ((b2 (read-non-head-escaped-byte str (+fx i 3) str-len)))
		(values (+fx (bit-lsh (bit-and #x1F b1) 6)
			     b2)
			(+fx i 6))))
	    ((=fx #xE0 (bit-and #xF0 b1)) ;; 3 bytes
	     (let* ((b2 (read-non-head-escaped-byte str (+fx i 3) str-len))
		    (b3 (read-non-head-escaped-byte str (+fx i 6) str-len)))
		(values (+fx (bit-lsh (bit-and #x0F b1) 12)
			     (+fx (bit-lsh b2 6)
				  b3))
			(+fx i 9))))
	    ((=fx #xF0 (bit-and #xF8 b1)) ;; 4bytes
	     (let* ((b2 (read-non-head-escaped-byte str (+fx i 3) str-len))
		    (b3 (read-non-head-escaped-byte str (+fx i 6) str-len))
		    (b4 (read-non-head-escaped-byte str (+fx i 9) str-len)))
		(values (+fx (bit-lsh (bit-and #x0F b1) 18)
			     (+fx (bit-lsh b2 12)
				  (+fx (bit-lsh b3 6)
				       b4)))
			(+fx i 12))))
	    (else
	     (uri-error "bad URI escape sequence 3")))))
		 
   (let ((str-len (string-length str)))
      ;; TODO: uri-decode. do not create new string, if there is nothing to decode
      (with-output-to-string
	 (lambda ()
	    (let loop ((i 0))
	       (when (<fx i str-len)
		  (let ((c (string-ref str i)))
		     (if (not (char=? c #\%))
			 (begin
			    (display c)
			    (loop (+fx i 1)))
			 (receive (decoded-uc-char end-pos)
			    (read-escaped-sequence str i str-len)
			    (when (>fx decoded-uc-char #x10FFFF)
			       (uri-error "bad URI escape sequence 4"))
			    (if (in-reserved-set? decoded-uc-char)
				(display (substring str i end-pos))
				(display-uc-char decoded-uc-char))
			    (loop end-pos))))))))))
