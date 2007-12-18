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
(global-special-add! 'NaN
		     jsg-NaN
		     (dont-enum-dont-delete-attributes))
(define jsg-Infinity (+infinity))
(global-special-add! 'Infinity
		     jsg-Infinity
		     (dont-enum-dont-delete-attributes))
(define jsg-undefined (js-undefined))
(global-special-add! 'undefined
		     jsg-undefined
		     (dont-enum-dont-delete-attributes))

(define-runtime-globals
   (define (print to-print)
      (print (any->string to-print)))
   (define (scmprint to-print)
      (write-circle to-print)
      (print))
   (define (eval prog)
      (eval-error prog))
   (define (isNaN number)
      (NaN? (any->number number)))
   (define (isFinite number)
      (let ((n (any->number number)))
	 (not (or (NaN? n)
		  (+infinity? n)
		  (-infinity? n)))))
   (define (parseInt string radix)
      (let* ((s (any->string string))
	     (str-len (string-length s))
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
	     (tmp-R (any->int32 radix))
	     (R (cond
		   ((and (zero? tmp-R)
			 (or (substring-at? s "0x" i)
			     (substring-at? s "0X" i)))
		    ;; i will be increased by two at init of loop.
		    16)
		   ((zero? tmp-R)
		    10)
		   (else
		    tmp-R))))
	 (cond
	    ((<fx R 2)
	     (NaN))
	    ((>fx R 36)
	     (NaN))
	    (else
	     (let loop ((i (if (and (=fx R 16)
				    (=fx tmp-R 0))
			       (+fx i 2)
			       i))
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
		       (if (<fx cv R)
			   (loop (+ i 1)
				 (+ (* R res) cv)
				 #t)
			   (loop str-len ;; let the first case do its job.
				 res
				 found-char))))))))))
