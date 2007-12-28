(module jsre-Array
   (include "macros.sch")
   (import jsre-object
	   jsre-Object
	   jsre-Date
	   jsre-Function
	   jsre-String
	   jsre-Number
	   jsre-Bool
	   jsre-natives
	   jsre-primitives
	   jsre-Error
	   jsre-conversion
	   jsre-global-object
	   jsre-scope-object
	   jsre-globals-tmp
	   )
   (export
    *js-Array* ;; can be modified by user -> can't be ::procedure
    (class Js-Array::Js-Object
       length::bint) ;; TODO: bint is too small.
    (Array-init)
    (js-array-literal length::bint els::pair-nil)))

(define *js-Array* (tmp-js-object))
(define *js-Array-prototype* (tmp-js-object))

;; extracts requested indices from object o and prototypes (if requested).
;; works, as indices can't be Ref-elements...
;; DANGEROUS though. (what if I change something?)
;; limit must be < max-int (otherwise llong->fixnum will be bad.
(define (extract-index-els-in-range o ht start end go-into-prototypes?)
   (define (key->index key)
      (let ((str-len (string-length key)))
	 (let loop ((i 0)
		    (res #l0))
	    (cond
	       ((>= res end)
		#f)
	       ((and (>= i str-len)
		     (>= res start))
		;; TODO: correct?
		(llong->fixnum res))
	       ((>= i str-len)
		#f)
	       (else
		(let ((cv (-fx (char->integer (string-ref key i))
			       (char->integer #\0))))
		   (if (and (>= cv 0)
			    (< cv 10))
		       (loop (+fx i 1)
			     (+ (* res 10) cv))
		       #f)))))))

   (unless (js-null? o)
      (with-access::Js-Object o (props proto)
	 (hashtable-for-each props
			     (lambda (key entry)
				(let ((index (key->index key)))
				   (when (and index
					      (not (hashtable-get ht index)))
				      (with-access::Property-entry entry (val)
				      (hashtable-put! ht index
						      (cons key val)))))))
	 (when go-into-prototypes?
	    (extract-index-els-in-range proto ht start end go-into-prototypes?)))))
   
(define-method (js-property-one-level-contains? o::Js-Array prop::bstring)
   (if (string=? prop "length")
       #t
       (call-next-method)))
(define-method (js-property-is-enumerable? o::Js-Array prop::bstring)
   (if (string=? prop "length")
       #f
       (call-next-method)))
(define-method (js-property-contains o::Js-Array prop::bstring)
   (if (string=? prop "length")
       (exact->inexact (Js-Array-length o))
       (call-next-method)))

(define-method (add-enumerables o::Js-Array enumerables-ht shadowed-ht)
   (hashtable-put! shadowed-ht "length" #t)
   (call-next-method))

(define-method (js-property-generic-set! o::Js-Array prop::bstring
					 new-val attributes)
   (define (property-index prop)
      ;; TODO
      (let ((index (any->uint32 prop)))
	 (and (string=? (integer->string index) prop)
	      index)))

   (with-access::Js-Array o (length props)
      (if (string=? prop "length")
	  (let ((nb-int (any->integer new-val))
		(nb-uint32 (any->uint32 new-val)))
	     (if (= nb-int nb-uint32) ;; TODO: really not optimal
		 (begin
		    (when (< nb-uint32 length)
		       (let ((ht (make-hashtable)))
			  (extract-index-els-in-range o ht nb-uint32 length #f)
			  (hashtable-for-each
			   ht
			   (lambda (index key/val)
			      (hashtable-remove! props (car key/val))))))
		    (set! length nb-uint32))
		 (range-error new-val)))
	  (let ((index (property-index prop)))
	     (when (and index
			(>= index length))
		;; TODO: verify that new length is not too big.
		(set! length (+ index 1)))
	     (call-next-method)))))

(define-method (js-object->string::bstring o::Js-Array)
   "Array")

(define (Array-init)
   (set! *js-Array* (Array-lambda))
   (register-function-object! *js-Array*
			      (Array-new)
			      Array-construct
			      (js-function-prototype) ;; 15.4.3
			      1                       ;; 15.4.3
			      "TODO [native]")
   (globals-tmp-add! (lambda () (global-runtime-add! 'Array *js-Array*)))
   (let ((array-object (procedure-object *js-Array*))
	 (prototype (instantiate::Js-Array            ;; 15.4.4
		       (props (make-props-hashtable))
		       (proto (js-object-prototype))
		       (length 0))))
      (set! *js-Array-prototype* prototype)
      (js-property-generic-set! array-object            ;; 15.4.3.1
				"prototype"
				prototype
				(prototype-attributes))
      (js-property-generic-set! prototype               ;; 15.4.4.2
			       "toString"
			       (toString)
			       (built-in-attributes))
      (js-property-generic-set! prototype               ;; 15.4.4.3
			       "toLocaleString"
			       (toLocaleString)
			       (built-in-attributes))
      (js-property-generic-set! prototype               ;; 15.4.4.4
			       "concat"
			       (concat)
			       (built-in-attributes))
      (js-property-generic-set! prototype               ;; 15.4.4.5
			       "join"
			       (join)
			       (built-in-attributes))
       (js-property-generic-set! prototype               ;; 15.4.4.6
 			       "pop"
 			       (pop)
 			       (built-in-attributes))
       (js-property-generic-set! prototype               ;; 15.4.4.7
 			       "push"
 			       (push)
 			       (built-in-attributes))
       (js-property-generic-set! prototype               ;; 15.4.4.8
 			       "reverse"
 			       (reverse)
 			       (built-in-attributes))
       (js-property-generic-set! prototype               ;; 15.4.4.9
 			       "shift"
 			       (shift)
 			       (built-in-attributes))
       (js-property-generic-set! prototype               ;; 15.4.4.10
 			       "slice"
 			       (slice)
 			       (built-in-attributes))
       (js-property-generic-set! prototype               ;; 15.4.4.11
 			       "sort"
 			       (array-sort)
 			       (built-in-attributes))
       (js-property-generic-set! prototype               ;; 15.4.4.12
 			       "splice"
 			       (splice)
 			       (built-in-attributes))
       (js-property-generic-set! prototype               ;; 15.4.4.13
 			       "unshift"
 			       (unshift)
 			       (built-in-attributes))))


(define (fill-Array a nb-args get-arg)
   ;; 15.4.2.1 && 15.4.2.2
   (if (and (= nb-args 1)
	    (real? (get-arg 0)))
       (let ((len (get-arg 0)))
	  (let ((int-len (any->uint32 len)))
	     (if (= len int-len)
		 (js-property-safe-set! a "length" len)
		 (range-error len))))
       (let loop ((i 0))
	  (when (< i nb-args)
	     (js-property-safe-set! a (integer->string i) (get-arg i))
	     (loop (+ i 1)))))
   a)

(define (Array-lambda)    ;; 15.4.1
   (js-fun-lambda
    #f
    #f
    (nb-args get-arg)
    ()
    (let ((a (instantiate::Js-Array
		(props (make-props-hashtable))
		(proto *js-Array-prototype*)
		(length 0))))
       (fill-Array a nb-args get-arg))))


(define (Array-new)
   (js-fun-lambda
    this
    #f
    (nb-args get-arg)
    ()
    (fill-Array this nb-args get-arg)))
   
(define (Array-construct::Js-Array f-o::Js-Function)   
   (instantiate::Js-Array
      (props (make-props-hashtable))
      (proto *js-Array-prototype*)
      (length 0)))

(define (js-array-literal length els)
   (let ((a (js-new *js-Array*)))
      (js-property-safe-set! a "length" length)
      (for-each (lambda (el)
		   (let ((index (car el))
			 (val (cadr el)))
		      (js-property-safe-set! a
					     (integer->string index)
					     val)))
		els)
      a))

(define (join-array a sep el->string)
   ;; 15.4.4.3 && 15.4.4.5
   (define (join->string el)
      (if (or (js-undefined? el)
	      (js-null? el))
	  ""
	  (el->string el)))
   
   (let ((len (any->uint32 (js-property-safe-get a "length")))
	 (sep-str (if (js-undefined? sep)
		      ","
		      (any->string sep))))
      (if (zero? len)
	  ""
	  (let loop ((res (join->string (js-property-safe-get a "0")))
		     (i 1))
	     (if (=fx i len)
		 res
		 (let* ((el (js-property-safe-get a (integer->string i)))
			(str (join->string el)))
		    (loop (string-append res sep-str str)
			  (+ 1 i))))))))
   
(define (toString)
   ;; 15.4.4.2
   (js-fun this #f #f "Array.toString"
	   ()
	   (if (not (Js-Array? this))
	       (type-error "Array-toString applied to" this)
	       (join-array this (js-undefined) any->string))))

(define (toLocaleString)
   ;; 15.4.4.3
   (js-fun this #f #f "Array.toLocaleString"
	   ()
	   (if (not (Js-Array? this))
	       (type-error "Array-toLocaleString applied to" this)
	       (join-array this
			   "," ;; locale-specific way of separating elements
			   (lambda (el)
			      (js-method-call el "toLocaleString"))))))

(define (arrays-concat nb-arrays get-array)
   ;; 15.4.4.4
   (define (add-els new-a offset a)
      (let* ((len (Js-Array-length a))
	     (ht (make-hashtable)))
	 (extract-index-els-in-range a ht 0 len #t)
	 (hashtable-for-each
	  ht
	  (lambda (index key/val)
	     (js-property-generic-set! new-a
				       ;; TODO not protected against overflow
				       (integer->string (+fx offset index))
				       (cdr key/val)
				       #f)))))
   
   (let ((new-a (js-new *js-Array*)))
      (let loop ((array-counter 0)
		 (new-length 0))
	 (if (=fx array-counter nb-arrays)
	     (begin
		(Js-Array-length-set! new-a new-length)
		 new-a)
	     (let ((a (get-array array-counter)))
		(if (Js-Array? a)
		    (begin
		       (add-els new-a new-length a)
		       (loop (+ array-counter 1)
			     (+ new-length (Js-Array-length a))))
		    (let ((str (any->string a)))
		       (js-property-safe-set! new-a
					      (integer->string new-length)
					      str)
		       (loop (+ array-counter 1)
			     (+ new-length 1)))))))))

(define (concat)
   ;; 15.4.4.4
   (js-fun this #f (nb-args get-arg) "Array.concat"
	   (first-arg) ;; so the length is 1 (end of 15.4.4.4)
	   (arrays-concat (+ nb-args 1)
			  (lambda (i)
			     (if (zero? i)
				 this
				 (get-arg (- i 1)))))))

(define (join)
   ;; 15.4.4.5
   (js-fun this #f #f "Array.join"
	   (sep)
	   (join-array this sep any->string)))

(define (pop)
   ;; 15.4.4.6
   (js-fun this #f #f "Array.pop"
	   ()
	   (if (Js-Array? this)
	       (let ((len (Js-Array-length this)))
		  (if (zero? len)
		      (js-undefined)
		      (let* ((len-str (integer->string len))
			     (res (js-property-safe-get this len-str)))
			 (js-property-safe-delete! this len-str)
			 (Js-Array-length-set! this (- len 1))
			 res)))
	       (let ((len (any->uint32 (js-property-safe-get this "length"))))
		  (if (zero? len)
		      (js-property-safe-set! this "length" 0.0)
		      (let* ((len-str (integer->string len))
			     (res (js-property-safe-get this len-str)))
			 (js-property-safe-delete! this len-str)
			 (js-property-safe-set! this
						"length"
						(exact->inexact (-fx len 1)))
			 res))))))

(define (push)
   ;; 15.4.4.7
   (js-fun this #f (nb-args get-arg) "Array.push"
	   (first) ;; length == 1
	   (let ((len (any->uint32 (js-property-safe-get this "length"))))
	      (let loop ((i 0)
			 (len len))
		 (if (>= i nb-args)
		     (let ((inexact-len (exact->inexact len)))
			(js-property-safe-set! this "length" inexact-len)
			inexact-len)
		     (begin
			(js-property-safe-set! this
					       (integer->string len)
					       (get-arg i))
			(loop (+fx i 1)
			      (+fx len 1))))))))

(define (reverse)
   ;; 15.4.4.8
   (js-fun this #f #f "Array.reverse"
	   ()
	   (let* ((len (any->uint32 (js-property-safe-get this "length")))
		  (h-len (/fx len 2))
		  (ht (make-hashtable)))
	      (extract-index-els-in-range this ht 0 len #t)
	      (let ((indices (hashtable-key-list ht)))
		 (let loop ((indices indices))
		    (unless (null? indices)
		       (let* ((index (car indices))
			      (other (- len index 1))
			      (key/val1 (hashtable-get ht index))
			      (key/val2 (hashtable-get ht other)))
			  (cond
			     ((and key/val1 key/val2)
			      (js-property-generic-set! this
							(car key/val1)
							(cdr key/val2)
							#f)
			      (js-property-generic-set! this
							(car key/val2)
							(cdr key/val1)
							#f)
			      ;; remove 2nd entry so it won't be added again.
			      (hashtable-remove! ht (car key/val2)))
			     (key/val1
			      (let ((other-key (integer->string other)))
				 (js-property-generic-set! this
							   other-key
							   (cdr key/val1)
							   #f)
				 (js-property-safe-delete! this
							   (car key/val1))))
			     (else
			      'do-nothing))
			  (loop (cdr indices)))))
		 this))))

;; shift to the left or right by given nb starting at given number.
;; if by is negative shift is to the right
(define (shift-from-by a from by)
   (let* ((len (any->uint32 (js-property-safe-get a "length")))
	  (shift? (positive? by))
	  ;; when shifting (to the left) we might have to erase elements left
	  ;; of 'from'. lower-target limits these elements.
	  (lower-target (if shift?
			    (-fx from by)
			    from))
	  (ht (make-hashtable)))
      (extract-index-els-in-range a ht lower-target len #t)
      (hashtable-for-each
       ht
       (lambda (index key/val)
	  (cond
	     ((< index lower-target)
	      'do-nothing)
	     ((< index from)
	      ;; can only happen during shift to left
	      ;; element that might be erased, but that won't be copied
	      (unless (hashtable-get ht (+fx index by))
		 (js-property-safe-delete! a (car key/val))))
	     (else
	      (let ((shifted-index-str (integer->string (-fx index by))))
		 ;; first copy current value
		 (js-property-generic-set! a shifted-index-str (cdr key/val) #f)
		 ;; then delete this one (if necessary)
		 (if shift? ;; to the left
		     (when (or (>=fx index (-fx len by)) ;; right end
			       (not (hashtable-get ht (+fx index by))))
			(js-property-safe-delete! a (car key/val)))
		     ;; when right-shifting do not delete left area
		     ;; remember: 'by' is negative
		     (when (and (>=fx index (-fx from by)) ;; not left end
				(not (hashtable-get ht (+fx index by))))
			(js-property-safe-delete! a (car key/val)))))))))))

(define (shift)
   ;; 15.4.4.9
   (js-fun
    this #f #f "Array.shift"
    ()
    (let ((len (any->uint32 (js-property-safe-get this "length"))))
       (if (zero? len)
	   (begin
	      (js-property-safe-set! this "length" 0.0)
	      (js-undefined))
	   (let ((res (js-property-safe-get this "0")))
	      (shift-from-by this 1 1)
	      (js-property-safe-set! this "length"
				     (exact->inexact (-fx len 1)))
	      res)))))

(define (slice)
   ;; 15.4.4.10
      (js-fun
       this #f #f "Array.slice"
       (start-any end-any)
       (let* ((new-a (js-new *js-Array*))
	      (len (any->uint32 (js-property-safe-get this "length")))
	      (start-int (any->integer start-any))
	      (start (if (< start-int 0)
			 (max 0 (+fx len start-int))
			 (min start-int len)))
	      (end-int (if (js-undefined? end-any)
			   len
			   (any->integer end-any)))
	      (end (if (< end-int 0)
		       (max 0 (+fx len end-int))
		       (min end-int len)))
	      (ht (make-hashtable)))
	  (extract-index-els-in-range this ht start end #t) ;; end is handled here
	  (hashtable-for-each
	   ht
	   (lambda (index key/val)
	      (js-property-generic-set! new-a
					(integer->string (-fx index start))
					(cdr key/val)
					#f)))
	  (js-property-safe-set! new-a
				 "length"
				 (exact->inexact (-fx end start)))
	  new-a)))

(define (array-sort)
   ;; 15.4.4.10
   (js-fun
    this #f #f "Array.sort"
    (compare-fn)
    (let* ((len (any->uint32 (js-property-safe-get this "length")))
	   (ht (make-hashtable)))
       (extract-index-els-in-range this ht 0 len #t)
	  ;; start by deleting all old values.
       (hashtable-for-each
	ht
	(lambda (index key/val)
	   (js-property-safe-delete! this
				     (car key/val))))
       (let* ((no-comp-fun? (js-undefined? compare-fn))
	      (els (map cdr (hashtable->list ht)))
	      (els-strs (if no-comp-fun?
			    (map (lambda (el)
				    (if (js-undefined? el)
					(cons #f el)
					(cons (any->string el) el)))
				 els)
			    els))
	      (comp (if no-comp-fun?
			(lambda (x y)
			   (cond
			      ((and (car x) (car y))
			       (string<? (car x) (car y)))
			      ((car x) #t)
			      ((car y) #f)
			      (else #f)))
			(lambda (x y)
			   (cond
			      ((and (js-undefined? x)
				    (js-undefined? y))
			       #f)
			      ((js-undefined? x)
			       #f)
			      ((js-undefined? y)
			       #t)
			      (else
			       (let ((tmp (js-call compare-fn #f x y)))
				  (if (and (real? tmp) (<fl tmp 0.0))
				      #t ;; less
				      #f)))))))
	      (sorted (sort comp els-strs)))
	  (let loop ((i 0)
		     (sorted sorted))
	     (unless (null? sorted)
		(js-property-generic-set! this
					  (integer->string i)
					  (if no-comp-fun?
					      (cdr (car sorted))
					      (car sorted))
					  #f)
		(loop (+fx i 1)
		      (cdr sorted)))))
       this)))

(define (splice)
   ;; 15.4.4.12
   (js-fun
    this #f (nb-args get-arg) "Array.splice"
    (start-any delete-count-any)
    (let* ((new-a (js-new *js-Array*))
	   (len (any->uint32 (js-property-safe-get this "length")))
	   (start-int (any->integer start-any))
	   (start (if (< start-int 0)
		      (max 0 (+fx len start-int))
		      (min start-int len)))
	   (delete-count-int (max (any->integer delete-count-any)
				  0))
	   (end (min delete-count-int
		     (-fx len start)))
	   (nb-inserted (-fx nb-args 2)))
       (js-property-safe-set! new-a "length" (exact->inexact delete-count-int))
       (if (=fx delete-count-int nb-inserted)
	   ;; best case...
	   (let loop ((k 0)
		      (i start))
	      (if (>= i end)
		  new-a
		  (let* ((k-str (integer->string k))
			 (i-str (integer->string i))
			 (old-val (js-property-contains this i-str))
			 (new-val (get-arg (+fx k 2))))
		     (when old-val
			(js-property-generic-set! new-a k-str old-val #f))
		     (js-property-safe-set! this i-str new-val))))
	   (begin
	      ;; copy cut-out elements into new array
	      (let ((ht (make-hashtable)))
		 (extract-index-els-in-range this ht start end #t)
		 (hashtable-for-each
		  ht
		  (lambda (index key/val)
		     (when (>= index start)
			(js-property-generic-set!
			 new-a
			 (integer->string (-fx index start))
			 (cdr key/val)
			 #f)))))
	      ;; (un)shift elements
	      (let ((diff (-fx delete-count-int (-fx nb-args 2))))
		 (shift-from-by this end diff)
		 ;; copy arguments into array
		 (let loop ((i 0))
		    (unless (>= i (-fx nb-args 2))
		       (js-property-safe-set! this
					      (integer->string (+fx start i))
					      (get-arg (+fx i 2)))
		       (loop (+fx i 1))))
		 (js-property-safe-set! this "length"
					(exact->inexact (-fx len diff)))
		 new-a))))))

(define (unshift)
   ;; 15.4.4.13
   (js-fun
    this #f (nb-args get-arg) "Array.unshift"
    (first) ;; len 1
    (let* ((len (any->uint32 (js-property-safe-get this "length")))
	   (new-len (exact->inexact (+fx len nb-args))))
       (unless (zero? len)
	  (shift-from-by this 0 (- nb-args))) ;; unshift requires negative 'by'
       (let loop ((i 0))
	  (unless (< i nb-args)
	     (js-property-safe-set! this (integer->string i) (get-arg i))
	     (loop (+fx i 1))))
       ;; TODO handle length-overflow?
       (js-property-safe-set! this "length" new-len)
       new-len)))
