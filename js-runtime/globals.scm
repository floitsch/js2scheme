(module jsre-globals
   (use jsre-base-object
	jsre-base-string
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
	 (create-special-global (STR "NaN")
				(get-Attributes dont-enum dont-delete)
				+nan.0))

   (set! jsg-Infinity                       ;; 15.1.1.2
	 (create-special-global (STR "Infinity")
				(get-Attributes dont-enum dont-delete)
				+inf.0))

   (set! jsg-undefined                      ;; 15.1.1.3
	 (create-special-global (STR "undefined")
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
			    `(js-fun #f #f #f (STR ,(symbol->string id))
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
					      (STR ,(symbol->string id))
					      ,val)))
				    *delayed-assigs*))))
		 exported-ids
		 ids
		 vals))))

(define *delayed-assigs* '())

(define-runtime-globals
   (define (print to-print)
      (print (js-string->utf8 (any->js-string to-print))))

   (define (scmprint to-print)
      (write-circle to-print)
      (print))

   (define (eval prog)              ;; 15.1.2.1
      ;; real eval's are short-cut in scm-out.
      (eval-error))

   (define (isNaN number)           ;; 15.1.2.4
      (nanfl? (any->number number)))

   (define (isFinite number)        ;; 15.1.2.5
      (let ((n (any->number number)))
	 (finitefl? n)))

   (define (parseInt string radix)  ;; 15.1.2.2
      (define (parseIntR s i R::bint sign::bint)
	 (let ((str-len (js-string-length s)))
	    (let loop ((i i)
		       (res 0)
		       (found-char #f))
	       (if (>= i str-len)
		   (if found-char
		       (* 1.0 (* res sign)) ;; (* 1.0) -> flonum
		       +nan.0)
		   (let* ((c (js-string-ref s i))
			  (ci (js-char->integer (js-string-ref s i)))
			  (cv (cond
				 ((and (js-char>=char? c #\0)
				       (js-char<=char? c #\9))
				  (-fx ci (char->integer #\0)))
				 ((and (js-char>=char? c #\a)
				       (js-char<=char? c #\z))
				  (+fx 10 (-fx ci (char->integer #\a))))
				 ((and (js-char>=char? c #\A)
				       (js-char<=char? c #\Z))
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
	 (js-char-whitespace? c))

      (define (first-non-white s)
	 (let ((len (js-string-length s)))
	    (let loop ((i 0))
	       (cond
		  ((>=fx i len) i)
		  ((js-char-whitespace? (js-string-ref s i))
		   (loop (+fx i 1)))
		  (else i)))))

      (define (read-sign s pos)
	 (cond
	    ((>=fx pos (js-string-length s))
	     (values 1 pos))
	    ((char=js-char? #\- (js-string-ref s pos))
	     (values -1 (+fx pos 1)))
	    ((char=js-char? #\+ (js-string-ref s pos))
	     (values 1 (+fx pos 1)))
	    (else
	     (values 1 pos))))

      (define (read-0x s Rfx pos)
	 (cond
	    ((and (or (=fx Rfx 0)
		      (=fx Rfx 16))
		  (or (js-substring-at_utf8? s "0x" pos)
		      (js-substring-at_utf8? s "0X" pos)))
	     (values 16 (+fx pos 2)))
	    ((=fx Rfx 0)
	     (values 10 pos))
	    (else
	     (values Rfx pos))))

      (let* ((s (any->js-string string))
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
      (define decimal-char? js-char-numeric?)
      (define whitespace? js-char-whitespace?)
      
      (define (substring->real str str-len from to)
	 (cond
	    ((=fx from to) +nan.0)
	    ((and (=fx from 0)
		  (=fx to str-len))
	     (js-string->real str))
	    (else
	     (js-string->real (js-substring str from to)))))
      
      (define (read-ws str i str-len)
	 (cond
	    ((>= i str-len)
	     +nan.0)
	    ((whitespace? (js-string-ref str i))
	     (read-ws str (+fx i 1) str-len))
	    (else
	     (read-str-decimal-literal str i str-len))))
      
      (define (read-str-decimal-literal str i str-len)
	 ;; get rid of infinity.
	 (cond
	    ((>=fx i str-len) +nan.0)
	    ((js-substring-at_utf8? str "Infinity" i) +inf.0)
	    ((js-substring-at_utf8? str "+Infinity" i) +inf.0)
	    ((js-substring-at_utf8? str "-Infinity" i) -inf.0)
	    ((or (char=js-char? #\- (js-string-ref str i))
		 (char=js-char? #\+ (js-string-ref str i)))
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
	       ((decimal-char? (js-string-ref str i))
		(loop (+fx i 1)
		      #f
		      encountered-dot?))
	       ((char=js-char? #\. (js-string-ref str i))
		(cond
		   ((and encountered-dot?
			 need-digit?)
		    +nan.0)
		   (encountered-dot?
		    (substring->real str str-len start-pos i))
		   (else
		    (loop (+fx i 1) need-digit? #t))))
	       ((or (char=js-char? #\e (js-string-ref str i))
		    (char=js-char? #\E (js-string-ref str i)))
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
	       ((decimal-char? (js-string-ref str i))
		(loop (+fx i 1)
		      (+fx i 1)
		      #f
		      #f))
	       ((and sign-is-allowed?
		     (or (char=js-char? #\- (js-string-ref str i))
			 (char=js-char? #\+ (js-string-ref str i))))
		(loop (+fx i 1)
		      end-pos
		      #t
		      #f))
	       (else
		(substring->real str str-len start-pos end-pos)))))
      
      (let ((str (any->js-string string)))
	 (read-ws str 0 (js-string-length str))))
   
   (define (decodeURI encodedURI)                     ;; 15.1.3.1
      (let ((str (any->js-string encodedURI)))
	 (uri-decode str (lambda (c)
			    (case c
			       ((#\; #\/ #\? #\: #\@ #\& #\= #\+ #\$ #\,)
				#t)
			       ((#\#) #t)
			       (else #f))))))
   
   (define (decodeURIComponent encodedURIComponent)   ;; 15.1.3.2
      (let ((str (any->js-string encodedURIComponent)))
	 (uri-decode str (lambda (c)
			    #f))))
   
   (define (encodeURI uri)                            ;; 15.1.3.3
      (let ((str (any->js-string uri)))
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
      (let ((str (any->js-string uriComponent)))
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
(define-inline (uri-encode str::Js-Base-String in-unescaped-set?)
   (define (escaped-byte-push! buf b)
      (define (hex-digit->integer d)
	 (cond
	    ((<fx d 10)
	     (+fx d (char->integer #\0)))
	    (else
	     (+fx d (-fx (char->integer #\A) 10)))))
      (js-string-buffer-uc-push! buf (char->integer #\%))
      (js-string-buffer-uc-push! buf (hex-digit->integer (bit-ursh b 4)))
      (js-string-buffer-uc-push! buf (hex-digit->integer (bit-and b #x0F))))

   (define (needs-escaping?/compute-escaped-size str)
      (let ((it (js-string-uc-iterator str)))
	 (with-handler
	    (lambda (e)
	       (uri-error (STR "invalid unicode character")))
	    ;; we assume that ascii-chars only take one js-string-char.
	    ;; this should be a reasonable assumptions. but still CARE.
	    (let loop ((needs-escaping? #f)
		       (size 0))
	       (let ((c (it)))
		  (cond
		     ((not c) (values needs-escaping? size))
		     ((in-unescaped-set? c) needs-escaping? (+fx size 1))
		     ;; otherwise it needs to be escaped:
		     ;; each byte is written as %xy
		     ((<=fx c #x7F) (loop #t (+fx size 3)))
		     ((<=fx c #x7FF) (loop #t (+fx size 6)))
		     ((<=fx c #xFFFF) (loop #t (+fx size 9)))
		     (else (loop #t (+fx size 12)))))))))

   (receive (needs-escaping? escaped-size)
      (needs-escaping?/compute-escaped-size str)
      (if (not needs-escaping?)
	  str ;; nothing to do.
	  (let ((res-buf (open-js-string-buffer escaped-size))
		(it (js-string-uc-iterator str)))
	     (let loop ()
		(let ((c (it)))
		   (cond
		      ((not c)
		       (close-js-string-buffer res-buf))
		      ;; the following would have been written completely
		      ;; different (and imho nicer) in C.
		      ((in-unescaped-set? c)
		       (js-string-buffer-uc-push! res-buf c)
		       (loop))
		      ((<=fx c #x7F)
		       (escaped-byte-push! res-buf c)
		       (loop))
		      ((<=fx c #x7FF)
		       (let ((b1 (+fx #xC0 (bit-ursh c 6)))
			     (b2 (+fx #x80 (bit-and c #x3F))))
			  (escaped-byte-push! res-buf b1)
			  (escaped-byte-push! res-buf b2)
			  (loop)))
		      ((<=fx c #xFFFF)
		       (let ((b1 (+fx #xE0 (bit-ursh c 12)))
			     (b2 (+fx #x80
				      (bit-and (bit-ursh c 6) #x3F)))
			     (b3 (+fx #x80 (bit-and c #x3F))))
			  (escaped-byte-push! res-buf b1)
			  (escaped-byte-push! res-buf b2)
			  (escaped-byte-push! res-buf b3)
			  (loop)))
		      (else
		       (let ((b1 (+fx #xF0 (bit-ursh c 18)))
			     (b2 (+fx #x80
				      (bit-and (bit-ursh c 12) #x3F)))
			     (b3 (+fx #x80
				      (bit-and (bit-ursh c 6) #x3F)))
			     (b4 (+fx #x80 (bit-and c #x3F))))
			  (escaped-byte-push! res-buf b1)
			  (escaped-byte-push! res-buf b2)
			  (escaped-byte-push! res-buf b3)
			  (escaped-byte-push! res-buf b4)
			  (loop))))))))))

(define-inline (uri-decode str::Js-Base-String in-reserved-set?)
   ;; the str is supposed to be encoded. Therefore all chars should be ASCII.
   ;; we will therefore simply use js-string-ref to traverse the string.

   (define-macro (+fx+ x . L)
      (if (null? L)
	  x
	  `(+fx ,x (+fx+ ,@L))))

   (define (read-escaped-byte str i str-len)
      (define (hex-char->integer c)
	 (let ((v (js-char->integer c))
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
	   (let ((d1 (hex-char->integer (js-string-ref str (+fx i 1))))
		 (d2 (hex-char->integer (js-string-ref str (+fx i 2)))))
	      (and d1 d2
		   (+fx (bit-lsh d1 4) d2)))))

   ;; removes the leading '10' bits
   (define (read-non-head-escaped-byte str i str-len)
      (let ((b (read-escaped-byte str i str-len)))
	 (when (or (not b)
		   (not (=fx #x80 (bit-and #xC0 b)))) ;; starts with '10'
	    (uri-error (STR "bad URI escape sequence")))
	 (bit-and #x3F b)))

   (define (read-escaped-sequence str i str-len)
      (let ((b1 (read-escaped-byte str i str-len)))
	 (when (not b1)
	    (uri-error (STR "bad URI escape sequence")))
	 (cond
	    ((zerofx? (bit-and b1 #x80))
	     (values b1 (+fx i 3)))
	    ((=fx #xC0 (bit-and #xE0 b1)) ;; 2 bytes
	     (let* ((b2 (read-non-head-escaped-byte str (+fx i 3) str-len))
		    (decoded-uc-char (+fx (bit-lsh (bit-and #x1F b1) 6)
					  b2)))
		(when (>fx decoded-uc-char #x10FFFF)
		   (uri-error (STR "bad URI escape sequence")))
		(values decoded-uc-char (+fx i 6))))
	    ((=fx #xE0 (bit-and #xF0 b1)) ;; 3 bytes
	     (let* ((b2 (read-non-head-escaped-byte str (+fx i 3) str-len))
		    (b3 (read-non-head-escaped-byte str (+fx i 6) str-len))
		    (decoded-uc-char (+fx+ (bit-lsh (bit-and #x0F b1) 12)
					   (bit-lsh b2 6)
					   b3)))
		(when (>fx decoded-uc-char #x10FFFF)
		   (uri-error (STR "bad URI escape sequence")))
		(values decoded-uc-char (+fx i 9))))
	    ((=fx #xF0 (bit-and #xF8 b1)) ;; 4bytes
	     (let* ((b2 (read-non-head-escaped-byte str (+fx i 3) str-len))
		    (b3 (read-non-head-escaped-byte str (+fx i 6) str-len))
		    (b4 (read-non-head-escaped-byte str (+fx i 9) str-len))
		    (decoded-uc-char (+fx+ (bit-lsh (bit-and #x0F b1) 18)
					   (bit-lsh b2 12)
					   (bit-lsh b3 6)
					   b4)))
		(when (>fx decoded-uc-char #x10FFFF)
		   (uri-error (STR "bad URI escape sequence")))
		(values decoded-uc-char (+fx i 12))))
	    (else
	     (uri-error (STR "bad URI escape sequence"))))))

   (define (needs-decoding?/decoded-size str)
      ;; we use a simple js-string-ref. for ascii-chars that should be ok.
      ;; but still: CARE
      (let ((str-len (js-string-length str)))
	 (let loop ((i 0)
		    (needs-decoding? #f)
		    (decoded-size 0))
	    (if (>=fx i str-len)
		(values needs-decoding? decoded-size)
		(let ((c (js-string-ref str i)))
		   (cond
		      ((char=js-char? #\% c)
		       (receive (decoded-uc-char end-pos)
			  (read-escaped-sequence str i str-len)
			  (if (in-reserved-set? decoded-uc-char)
			      ;; needs to stay encoded
			      (loop end-pos
				    needs-decoding?
				    (+fx decoded-size (-fx end-pos i)))
			      (loop end-pos
				    #t
				    (+fx decoded-size
					 (js-string-uc-char-size
					  decoded-uc-char))))))
		      (else (loop (+fx i 1)
				  needs-decoding?
				  (+fx decoded-size 1)))))))))

   (define (verbatim-push! buf str from to)
      (unless (>=fx from to)
	 (js-string-buffer-verbatim-push! buf (js-string-ref str from))
	 (verbatim-push! buf str (+fx from 1) to)))

   (receive (needs-decoding? target-len)
      (needs-decoding?/decoded-size str)
      (if (not needs-decoding?)
	  str
	  (let ((str-len (js-string-length str))
		(buf (open-js-string-buffer target-len)))
	     (let loop ((i 0))
		(cond
		   ((>=fx i str-len)
		    (close-js-string-buffer buf))
		   ((not (char=js-char? #\% (js-string-ref str i)))
		    (js-string-buffer-verbatim-push! buf (js-string-ref str i))
		    (loop (+fx i 1)))
		   (else
		    (receive (decoded-uc-char end-pos)
		       (read-escaped-sequence str i str-len)
		       (if (in-reserved-set? decoded-uc-char)
			   (verbatim-push! buf str i end-pos)
			   (js-string-buffer-uc-push! buf decoded-uc-char))
		       (loop end-pos)))))))))
