(module utf
   (export
    (utf8-string->utf16-string::ucs2string s::bstring)
    (utf16-string->utf8-string::bstring s::ucs2string))
   (main my-main))

(define (utf8-fold s init-val fun)
   (define (utf-8-error)
      (error "utf8-fold"
	     "bad UTF-8 string"
	     s))

   (define (string-int str i)
      (char->integer (string-ref str i)))

   (let ((str-len (string-length s)))
      (define (surrogate-char i)
	 (and (<fx i str-len)
	      ;; char is of form '10xxxxxx'
	      (=fx (bit-and (char->integer (string-ref s i))
			    #xC0)
		   #x80)))

      (let loop ((i 0)
		 (v init-val))
	 (if (>=fx i str-len)
	     v
	     (let ((cv (string-int s i)))
		(cond
		   ((=fx (bit-and cv #xF8) #xF0)
		    ;; encoded as 4 chars
		    (unless (and (surrogate-char (+fx i 1))
				 (surrogate-char (+fx i 2))
				 (surrogate-char (+fx i 3)))
		       (utf-8-error))
		    (let* ((cv1 (string-int s (+fx i 1)))
			   (cv2 (string-int s (+fx i 2)))
			   (cv3 (string-int s (+fx i 3)))
			   ;; first 3 bits
			   (p0 (bit-and cv #x7))
			   ;; next 6
			   (p1 (+fx (bit-lsh p0 6)
				    (bit-and cv1 #x3F)))
			   ;; next 6
			   (p2 (+fx (bit-lsh p1 6)
				    (bit-and cv2 #x3F)))
			   ;; and the last 6 ones
			   (p3 (+fx (bit-lsh p2 6)
				    (bit-and cv3 #x3F))))
		       (loop (+fx i 4)
			     (fun v p3))))
		   ((=fx (bit-and cv #xF0) #xE0)
		    ;; encoded a 3 chars
		    (unless (and (surrogate-char (+fx i 1))
				 (surrogate-char (+fx i 2)))
		       (utf-8-error))
		    (let* ((cv1 (string-int s (+fx i 1)))
			   (cv2 (string-int s (+fx i 2)))
			   ;; first 4 bits
			   (p0 (bit-and cv #x0F))
			   ;; next 6
			   (p1 (+fx (bit-lsh p0 6)
				    (bit-and cv1 #x3F)))
			   ;; final 6 bits
			   (p2 (+fx (bit-lsh p1 6)
				    (bit-and cv2 #x3F))))
		       (loop (+fx i 3)
			     (fun v p2))))
		   ((=fx (bit-and cv #xE0) #xC0)
		    ;; encoded as 2 chars
		    (unless (surrogate-char (+fx i 1))
		       (utf-8-error))
		    (let* ((cv1 (string-int s (+fx i 1)))
			   ;; first 5 bits
			   (p0 (bit-and cv #x1F))
			   ;; and the second part
			   (p1 (+fx (bit-lsh p0 6)
				    (bit-and cv1 #x3F))))
		       (loop (+fx i 2)
			     (fun v p1))))
		   ((=fx (bit-and cv #x80) 0)
		    ;; encoded as 1 char
		    (loop (+fx i 1)
			  (fun v cv)))
		   (else
		    (error "utf8->utf16"
			   "bad utf-8 string"
			   s))))))))

(define (utf16-fold s::ucs2string init-val fun)
   (define (utf16-error)
      (error "utf16-fold"
	     "bad UTF-16 string"
	     s))

   (let loop ((i 0)
	      (v init-val))
      (cond
	 ((>=fx i (ucs2-string-length s))
	  v)
	 ((or (<fx (ucs2->integer (ucs2-string-ref s i)) #xD800)
	      (>=fx (ucs2->integer (ucs2-string-ref s i)) #xE000))
	  (loop (+fx i 1)
		(fun v (ucs2->integer (ucs2-string-ref s i)))))
	 (else
	  (unless (>fx (ucs2-string-length s) (+fx i 1))
	     (utf16-error))
	  (let* ((ucs1 (ucs2->integer (ucs2-string-ref s i)))
		 (ucs2 (ucs2->integer (ucs2-string-ref s (+fx i 1)))))
	     (unless (and (>=fx ucs1 #xD800) (<=fx ucs1 #xDBFF)
			  (>=fx ucs2 #xDC00) (<=fx ucs2 #xDFFF))
		(utf16-error))
	     (loop (+fx i 2)
		   (fun v
			(+fx #x10000
			     ;; take 10 bits from each char
			     (+fx (bit-lsh (bit-and ucs1 #x3FF) 10)
				  (bit-and ucs2 #x3FF))))))))))
	     
   
(define (utf8-string->utf16-string::ucs2string s::bstring)
   (define (count-utf16-code-points s)
      (utf8-fold s
		 0
		 (lambda (count::bint uc-char::bint)
		    (if (>=fx uc-char #x10000)
			(+fx count 2)
			(+fx count 1)))))

   (let* ((nb-ucs-chars (count-utf16-code-points s))
	  (ucs-str (make-ucs2-string nb-ucs-chars)))
      (utf8-fold s
		 0
		 (lambda (i uc-char)
		    (if (>fx uc-char #x10000)
			(let* ((sub (-fx uc-char #x10000))
			       ;; first 10 bits
			       (p1 (bit-rsh (bit-and sub #xFFC00) 10))
			       ;; second 10 bits
			       (p2 (bit-and sub #x3FF))
			       (ucs1 (integer->ucs2 (+fx #xD800 p1)))
			       (ucs2 (integer->ucs2 (+fx #xDC00 p2))))
			   (ucs2-string-set! ucs-str i ucs1)
			   (ucs2-string-set! ucs-str (+fx i 1) ucs2)
			   (+fx i 2))
			(begin
			   (ucs2-string-set! ucs-str i (integer->ucs2 uc-char))
			   (+fx i 1)))))
      ucs-str))

(define (utf16-string->utf8-string s)
   (define (count-utf8-code-points s)
      (utf16-fold s
		  0
		  (lambda (count::bint uc-char::bint)
		     (cond
			((>=fx uc-char #x10000)
			 (+fx count 4))
			((>=fx uc-char #x800)
			 (+fx count 3))
			((>=fx uc-char #x80)
			 (+fx count 2))
			(else
			 (+fx count 1))))))

   (let* ((nb-chars (count-utf8-code-points s))
	  (str (make-string nb-chars)))
      (utf16-fold s
		  0
		  (lambda (i uc-char)
		     (cond
			((>=fx uc-char #x10000)
			 (let* (;; take the last 6 bits
				(c3 (+fx #x80 (bit-and uc-char #x3F)))
				(uc-3 (bit-rsh uc-char 6))
				;; take the next 6 bits
				(c2 (+fx #x80 (bit-and uc-3 #x3F)))
				(uc-2 (bit-rsh uc-char 6))
				;; another 6
				(c1 (+fx #x80 (bit-and uc-2 #x3F)))
				(uc-1 (bit-rsh uc-char 6))
				;; and the final 3 bits
				(c0 (+fx #xF0 (bit-and uc-1 #x07))))
			    (string-set! str i (integer->char c0))
			    (string-set! str (+fx i 1) (integer->char c1))
			    (string-set! str (+fx i 2) (integer->char c2))
			    (string-set! str (+fx i 3) (integer->char c3))
			    (+fx i 4)))
			((>=fx uc-char #x800)
			 (let* (;; take the last 6 bits
				(c2 (+fx #x80 (bit-and uc-char #x3F)))
				(uc-2 (bit-rsh uc-char 6))
				;; take the next 6 bits
				(c1 (+fx #x80 (bit-and uc-2 #x3F)))
				(uc-1 (bit-rsh uc-char 6))
				;; and the final 4 bits
				(c0 (+fx #xE0 (bit-and uc-1 #x0F))))
			    (string-set! str i (integer->char c0))
			    (string-set! str (+fx i 1) (integer->char c1))
			    (string-set! str (+fx i 2) (integer->char c2))
			    (+fx i 3)))
			((>=fx uc-char #x80)
			 (let* (;; take the last 6 bits
				(c1 (+fx #x80 (bit-and uc-char #x3F)))
				(uc-1 (bit-rsh uc-char 6))
				;; and the final 5 bits
				(c0 (+fx #xC0 (bit-and uc-1 #x1F))))
			    (string-set! str i (integer->char c0))
			    (string-set! str (+fx i 1) (integer->char c1))
			    (+fx i 2)))
			(else
			 (string-set! str i (integer->char uc-char))
			 (+fx i 1)))))
      str))

(define *mode* 'utf8->utf16)

(define (read-utf8-buffer::bstring)
   (let ((buffer (make-string 1024)))
      (string-shrink! buffer (read-chars! buffer 1024))))

(define (read-utf16-buffer::ucs2string)
   (define BOM0 (integer->char #xFE))
   (define BOM1 (integer->char #xFF))

   (let* ((buffer (make-string 2048))
	  (str-len (read-chars! buffer 2048)))
      (when (not (even? str-len))
	 (error 'utf
		"utf16-string must have even length"
		(string-length buffer)))
      (let* ((BOM? (and (>=fx str-len 2)
			(or (and (char=? (string-ref buffer 0) BOM0)
				 (char=? (string-ref buffer 1) BOM1))
			    (and (char=? (string-ref buffer 0) BOM1)
				 (char=? (string-ref buffer 1) BOM0)))))
	     (bigendian (if BOM?
			    (char=? (string-ref buffer 0) BOM0)
			    #t)) ;; assume bigendian
	     (res (make-ucs2-string (if BOM?
					(-fx (/fx str-len 2) 1)
					(/fx str-len 2)))))
	 (let loop ((i (if BOM? 2 0))
		    (j 0))
	    (if (>=fx i str-len)
		res
		(let* ((c0 (string-ref buffer i))
		       (c1 (string-ref buffer (+fx i 1)))
		       (v0 (char->integer c0))
		       (v1 (char->integer c1))
		       (sum (+fx (*fx v0 256) v1))
		       (uc (integer->ucs2 sum)))
		   (ucs2-string-set! res j uc)
		   (loop (+fx i 2) (+fx j 1))))))))

(define (write-utf8-buffer s)
   (let loop ((i 0))
      (unless (>=fx i (string-length s))
	 (write-char (string-ref s i))
	 (loop (+fx i 1)))))

(define (write-utf16-buffer s)
   (write-char (integer->char #xFE))
   (write-char (integer->char #xFF))
   (let loop ((i 0))
      (unless (>=fx i (ucs2-string-length s))
	 (let* ((c (ucs2-string-ref s i))
		(v (ucs2->integer c))
		(v0 (/fx v 256))
		(v1 (remainderfx v 256)))
	    (write-char (integer->char v0))
	    (write-char (integer->char v1))
	    (loop (+fx i 1))))))

(define (my-main args)
   (when (not (null? (cdr args)))
      (set! *mode* 'utf16->utf8))

   (if (eq? *mode* 'utf8->utf16)
       (let ((s (read-utf8-buffer)))
	  (write-utf16-buffer (utf8-string->utf16-string s)))
       (let ((s (read-utf16-buffer)))
	  (write-utf8-buffer (utf16-string->utf8-string s)))))
