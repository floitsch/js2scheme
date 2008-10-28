(module jsre-globals
   (include "macros.sch")
   (import jsre-object
	   jsre-natives ;; undefined, null, ...
	   jsre-primitives
	   jsre-Error
	   jsre-Object
	   jsre-Date
	   jsre-Function
	   jsre-String
	   jsre-Number
	   jsre-Bool
	   jsre-conversion
	   jsre-global-object
	   jsre-scope-object
	   jsre-globals-tmp
	   )
   (export jsg-NaN
	   jsg-Infinity
	   jsg-undefined
	   jsg-print
	   jsg-scmprint
	   jsg-eval
	   jsg-isNaN
	   jsg-isFinite
	   jsg-parseInt
	   ))

(define jsg-NaN (NaN))
(globals-tmp-add!
 (lambda ()
    (global-special-add! 'NaN              ;; 15.1.1.1
			 jsg-NaN
			 (get-Attributes dont-enum dont-delete))))

(define jsg-Infinity (+infinity))
(globals-tmp-add!
 (lambda ()
    (global-special-add! 'Infinity         ;; 15.1.1.2
			 jsg-Infinity
			 (get-Attributes dont-enum dont-delete))))
(define jsg-undefined (js-undefined))
(globals-tmp-add!
 (lambda ()
    (global-special-add! 'undefined        ;; 15.1.1.3
			 jsg-undefined
			 (get-Attributes dont-enum dont-delete))))

(define-runtime-globals
   (define (print to-print)
      (print (any->string to-print)))
   (define (scmprint to-print)
      (write-circle to-print)
      (print))
   (define (eval prog)              ;; 15.1.2.1
      (eval-error prog))
   (define (isNaN number)           ;; 15.1.2.4
      (NaN? (any->number number)))
   (define (isFinite number)        ;; 15.1.2.5
      (let ((n (any->number number)))
	 (not (or (NaN? n)
		  (+infinity? n)
		  (-infinity? n)))))
   (define (parseInt string radix)  ;; 15.1.2.2
      (define (white-space? c)
	 ;; TODO: not spec-conform
	 (char-whitespace? c))

      (define (parseIntR s i R sign)
	 (let ((str-len (string-length s)))
	    (let loop ((i i)
		       (res 0.0)
		       (found-char #f))
	       (if (>= i str-len)
		   (if found-char
		       (*fl res sign)
		       (NaN))
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
      
      (let* ((s (any->string string))
	     (sign (if (or (string-null? s)
			   (not (char=? #\- (string-ref s 0))))
		       1.0
		       -1.0))
	     (i (cond
		   ((string-null? s) 0)
		   ((or (char=? #\- (string-ref s 0))
			(char=? #\+ (string-ref s 0)))
		    1)
		   (else 0)))
	     (tmp-R (any->int32 radix)))
	 (if (or (<fl tmp-R 2.0)
		 (>fl tmp-R 36.0))
	     (NaN)
	     (let* ((fix-R (flonum->fixnum tmp-R))
		    (R (cond
			  ((and (zero? fix-R)
				(or (substring-at? s "0x" i)
				    (substring-at? s "0X" i)))
			   ;; i will be increased by two at call to parseIntR.
			   16)
			  ((zero? fix-R)
			   10)
			  (else
			   fix-R))))
		(cond
		   ((<fx R 2)
		    (NaN))
		   ((>fx R 36)
		    (NaN))
		   (else
		    (parseIntR s
			       (if (and (=fx R 16)
					(=fx fix-R 0))
				   (+fx i 2)
				   i)
			       R
			       sign))))))))
