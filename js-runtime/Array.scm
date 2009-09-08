(module jsre-Array
   (import jsre-base-object
	   jsre-ht-object
	   jsre-property-entry
	   jsre-base-string)
   (use jsre-undefined
	jsre-Object
	jsre-Date
	jsre-Function
	jsre-String
	jsre-Number
	jsre-Bool
	jsre-Error
	jsre-conversion
	jsre-global-object
	jsre-scope-object
	)
   (export
    *jsg-Array*
    (final-class NatO-Array::Js-HT-Object
       length::double) ;; TODO: really not optimal :(
    (Array-init)
    (js-array-literal length::bint els::pair-nil)
    (scm-list->js-array::NatO-Array els::pair-nil)
    (orig-jsg-Array?::bool)
    (empty-js-Array::NatO-Array)))

;; TODO: Array is really not optimal: number operations are bad, and
;;       the array implementation based on hashtables is slow.

(define *jsg-Array* #unspecified)
(define *js-Array-orig* (lambda () (js-null))) ;; going to be replaced soon.
(define *js-Array-prototype* (js-null))

;; extracts requested indices from object o and prototypes (if requested).
;; limit must be < max-int (otherwise llong->fixnum will be bad.
(define (extract-index-els-in-range o ht start::llong end::llong
				    go-into-prototypes?)
   (define (key->index key)
      (let ((str-len (js-string-length key)))
	 (let loop ((i 0)
		    (res #l0))
	    (cond
	       ((>=llong res end)
		#f)
	       ((and (>=fx i str-len)
		     (>=llong res start))
		res)
	       ((>=fx i str-len)
		#f)
	       (else
		(let ((cv (-fx (js-char->integer (js-string-ref key i))
			       (char->integer #\0))))
		   (if (and (>= cv 0)
			    (< cv 10))
		       (loop (+fx i 1)
			     (+ (*llong res #l10) cv))
		       #f)))))))

   (js-property-for-each
    o
    (lambda (key val read-only? deletable? enumerable?)
       (when enumerable?
	  (let ((index (key->index key)))
	     (when (and index
			(not (hashtable-get ht index)))
		(hashtable-put! ht index
				(cons key val))))))))
   
(define-method (js-property-one-level-contains? o::NatO-Array prop)
   ;; length-attribute is dontEnum dontDelete (15.4.5.2)
   (if (js-string=? prop (STR "length"))
       #t
       (call-next-method)))
(define-method (js-property-is-enumerable? o::NatO-Array prop)
   ;; length-attribute is dontEnum dontDelete (15.4.5.2)
   (if (js-string=? prop (STR "length"))
       #f
       (call-next-method)))
(define-method (js-property-contains o::NatO-Array prop)
   (if (js-string=? prop (STR "length"))
       (NatO-Array-length o)
       (call-next-method)))

(define-method (js-property-one-level-for-each o::NatO-Array p::procedure)
   ;; length-attribute is dontEnum dontDelete (15.4.5.2)
   (with-access::NatO-Array o (length)
      (p (STR "length") length #f #f #f)
      (call-next-method)))
   
(define *array-index-limit* 4294967295.0) ;; #xffffffff))
(define-method (js-property-generic-set! o::NatO-Array prop new-val attributes)
   (define (property-index prop)
      (let ((index (any->uint32 prop)))
	 (and (js-string=? (any->js-string index) prop)
	      (<fl index *array-index-limit*)
	      index)))

   (with-access::NatO-Array o (length props)
      (if (js-string=? prop (STR "length"))
	  (let ((nb-uint32 (any->uint32 new-val)))
	     (if (=fl new-val nb-uint32)
		 (begin
		    (when (<fl nb-uint32 length)
		       (let ((ht (make-hashtable)))
			  (extract-index-els-in-range o ht
						      (flonum->llong nb-uint32)
						      (flonum->llong length)
						      #f)
			  (hashtable-for-each
			   ht
			   (lambda (index key/val)
			      (hashtable-remove! props (car key/val))))))
		    (set! length nb-uint32))
		 (range-error (STR "invalid array-index") new-val)))
	  (let ((index (property-index prop)))
	     (when (and index
			(>=fl index length))
		(set! length (+fl index 1.0)))
	     (call-next-method)))))

(define-method (js-property-safe-delete! o::NatO-Array prop)
   ;; length-attribute is dontEnum dontDelete (15.4.5.2)
   (if (js-string=? prop (STR "length"))
       #f
       (call-next-method)))

(define-method (js-class-name o::NatO-Array)
   (STR "Array"))

(define (Array-init)
   (set! *js-Array-orig* (Array-lambda))
   (set! *jsg-Array* (create-runtime-global (STR "Array") *js-Array-orig*))
   (let* ((text-repr (STR "function(v) { /*native Array*/ throw 'native'; }"))
	  (array-object (create-function-object *js-Array-orig*
						(Array-new)
						Array-construct
						text-repr))
	  (prototype (instantiate::NatO-Array            ;; 15.4.4
		       (props (make-props-hashtable))
		       (proto (natO-object-prototype))
		       (length 0.0))))

      (set! *js-Array-prototype* prototype)
      
      (js-property-generic-set! array-object            ;; 15.4.3
				(STR "length")
				1.0
				(length-attributes))
      (js-property-generic-set! array-object            ;; 15.4.3.1
				(STR "prototype")
				prototype
				(get-Attributes dont-enum
						dont-delete read-only))
      
      (js-property-generic-set! prototype               ;; 15.4.4.1
				(STR "constructor")
				*js-Array-orig*
				(constructor-attributes))
      (js-property-generic-set! prototype               ;; 15.4.4.2
				(STR "toString")
				(toString)
				(built-in-attributes))
      (js-property-generic-set! prototype               ;; 15.4.4.3
				(STR "toLocaleString")
				(toLocaleString)
				(built-in-attributes))
      (js-property-generic-set! prototype               ;; 15.4.4.4
				(STR "concat")
				(concat)
				(built-in-attributes))
      (js-property-generic-set! prototype               ;; 15.4.4.5
				(STR "join")
				(join)
				(built-in-attributes))
      (js-property-generic-set! prototype               ;; 15.4.4.6
				(STR "pop")
				(pop)
				(built-in-attributes))
      (js-property-generic-set! prototype               ;; 15.4.4.7
				(STR "push")
				(push)
				(built-in-attributes))
      (js-property-generic-set! prototype               ;; 15.4.4.8
				(STR "reverse")
				(reverse)
				(built-in-attributes))
      (js-property-generic-set! prototype               ;; 15.4.4.9
				(STR "shift")
				(shift)
				(built-in-attributes))
      (js-property-generic-set! prototype               ;; 15.4.4.10
				(STR "slice")
				(slice)
				(built-in-attributes))
      (js-property-generic-set! prototype               ;; 15.4.4.11
				(STR "sort")
				(array-sort)
				(built-in-attributes))
      (js-property-generic-set! prototype               ;; 15.4.4.12
				(STR "splice")
				(splice)
				(built-in-attributes))
      (js-property-generic-set! prototype               ;; 15.4.4.13
				(STR "unshift")
				(unshift)
				(built-in-attributes))))


(define (fill-Array a nb-args get-arg)
   ;; 15.4.2.1 && 15.4.2.2
   ;; CARE: we do not allow to pass more than bint elements to fill-array
   ;;       this is due to nb-args being a bint.
   (if (and (= nb-args 1)
	    (flonum? (get-arg 0)))
       (let ((len (get-arg 0)))
	  (let ((int-len (any->uint32 len)))
	     (if (=fl len int-len)
		 (js-property-set! a (STR "length") len)
		 (begin
		    (tprint int-len " " len)
		    (range-error (STR "invalid array-length") len)))))
       (let loop ((i 0))
	  (when (< i nb-args)
	     (js-property-set! a (integer->js-string i) (get-arg i))
	     (loop (+fx i 1)))))
   a)

(define (Array-lambda)    ;; 15.4.1
   (js-fun-lambda
    #f
    #f
    (nb-args get-arg)
    ()
    (let ((a (instantiate::NatO-Array
		(props (make-props-hashtable))
		(proto *js-Array-prototype*)
		(length 0.0))))
       (fill-Array a nb-args get-arg))))


(define (Array-new)
   (js-fun-lambda
    this
    #f
    (nb-args get-arg)
    ()
    (fill-Array this nb-args get-arg)))
   
(define (Array-construct::NatO-Array f-o)   
   (instantiate::NatO-Array
      (props (make-props-hashtable))
      (proto *js-Array-prototype*)
      (length 0.0)))

(define (js-array-literal length els)
   ;; HACK: we only allow bint elements. (length is of type bint)
   (let ((a (js-new (global-read *jsg-Array*))))
      (js-property-set! a (STR "length") (fixnum->flonum length))
      (for-each (lambda (el)
		   (let ((index (car el))
			 (val (cadr el)))
		      (js-property-set! a
					(integer->js-string index)
					val)))
		els)
      a))

(define (orig-jsg-Array?)
   (eq? (global-typeof-read *jsg-Array*)
	*js-Array-orig*))

(define (empty-js-Array)
   (Array-construct #unspecified))

(define (scm-list->js-array els)
   (let ((a (empty-js-Array)))
      (let loop ((els els)
		 (i 0))
	 (if (null? els)
	     a
	     (begin
		(js-property-set! a
				  (integer->js-string i)
				  (car els))
		(loop (cdr els)
		      (+fx i 1)))))))

(define (join-array a sep el->string)
   ;; 15.4.4.3 && 15.4.4.5
   (define (join->string el)
      (if (or (js-undefined? el)
	      (js-null? el))
	  (STR "")
	  (el->string el)))

   (let* ((len (any->uint32 (js-property-get a (STR "length"))))
	  (llen (flonum->llong len))
	  (sep-str (if (js-undefined? sep)
		       (STR ",")
		       (any->js-string sep))))
      (cond
	 ((=fl len 0.0)
	  (STR ""))
	 ((js-string-null? sep-str)
	  ;; sparse huge arrays are feasible and should not be done in
	  ;; O(length) but in O(nb-items)
	  (let ((ht (make-hashtable)))
	     (extract-index-els-in-range a ht #l0 llen #t)
	     (let* ((l (hashtable-map ht list))
		    (sorted-l (sort (lambda (a b) (<llong (car a) (car b)))
				    l))
		    (els (map! (lambda (entry)
				  (let ((key/val (cadr entry)))
				     (join->string (cdr key/val))))
			       sorted-l)))
		(apply js-string-append sorted-l))))
	 (else
	  (let loop ((res (join->string (js-property-get a (STR "0"))))
		     (i #l1))
	     (if (=llong i llen)
		 res
		 (let* ((el (js-property-get a (llong->js-string i)))
			(str (join->string el)))
		    (loop (js-string-append res sep-str str)
			  (+llong i #l1)))))))))
   
(define (toString)
   ;; 15.4.4.2
   (js-fun this #f #f (STR "Array.prototype.toString")
	   ()
	   (if (not (NatO-Array? this))
	       (type-error (STR "Array-toString applied to") this)
	       (join-array this (js-undefined) any->js-string))))

(define (toLocaleString)
   ;; 15.4.4.3
   (js-fun this #f #f (STR "Array.prototype.toLocaleString")
	   ()
	   (if (not (NatO-Array? this))
	       (type-error (STR "Array-toLocaleString applied to") this)
	       (join-array this
			   "," ;; locale-specific way of separating elements
			   (lambda (el)
			      (js-method-call el (STR "toLocaleString")))))))

(define (arrays-concat nb-arrays get-array)
   ;; 15.4.4.4
   (define (add-els new-a offset a)
      (let* ((len (NatO-Array-length a))
	     (ht (make-hashtable)))
	 (extract-index-els-in-range a ht #l0 (flonum->llong len) #t)
	 (hashtable-for-each
	  ht
	  (lambda (index key/val)
	     (js-property-generic-set! new-a
				       (llong->js-string (+llong offset index))
				       (cdr key/val)
				       #f)))))
   
   (let ((new-a (js-new (global-read *jsg-Array*))))
      (let loop ((array-counter 0)
		 (new-length #l0))
	 (if (=fx array-counter nb-arrays)
	     (begin
		(NatO-Array-length-set! new-a (llong->flonum new-length))
		 new-a)
	     (let ((a (get-array array-counter)))
		(if (NatO-Array? a)
		    (begin
		       (add-els new-a new-length a)
		       (loop (+ array-counter 1)
			     (+llong new-length
				     (flonum->llong (NatO-Array-length a)))))
		    (let ((str (any->js-string a)))
		       (js-property-set! new-a
					 (llong->js-string new-length)
					 str)
		       (loop (+fx array-counter 1)
			     (+llong new-length #l1)))))))))

(define (concat)
   ;; 15.4.4.4
   (js-fun this #f (nb-args get-arg) (STR "Array.prototype.concat")
	   (first-arg) ;; so the length is 1 (end of 15.4.4.4)
	   (arrays-concat (+ nb-args 1)
			  (lambda (i)
			     (if (zero? i)
				 this
				 (get-arg (- i 1)))))))

(define (join)
   ;; 15.4.4.5
   (js-fun this #f #f (STR "Array.prototype.join")
	   (sep)
	   (join-array this sep any->js-string)))

(define (pop)
   ;; 15.4.4.6
   (js-fun this #f #f (STR "Array.prototype.pop")
	   ()
	   (let ((len (any->uint32 (js-property-get this (STR "length")))))
	      (if (=fl len 0.0)
		  (begin
		     (js-property-set! this (STR "length") 0.0)
		     (js-undefined))
		  (let* ((len-str (llong->js-string (flonum->llong len)))
			 (res (js-property-get this len-str)))
		     (js-property-safe-delete! this len-str)
		     (js-property-set! this
				       (STR "length")
				       (-fl len 1.0))
		     res)))))

(define (push)
   ;; 15.4.4.7
   (js-fun this #f (nb-args get-arg) (STR "Array.prototype.push")
	   (first) ;; length == 1
	   (let ((len (any->uint32 (js-property-get this (STR "length")))))
	      (let loop ((i 0)
			 (llen (flonum->llong len)))
		 (if (>=fx i nb-args)
		     (let ((flolen (llong->flonum llen)))
			(js-property-set! this (STR "length") flolen)
			flolen)
		     (begin
			(js-property-set! this
					  (llong->js-string llen)
					  (get-arg i))
			(loop (+fx i 1)
			      (+llong llen #l1))))))))

(define (reverse)
   ;; 15.4.4.8
   (js-fun this #f #f (STR "Array.prototype.reverse")
	   ()
	   (let* ((len (any->uint32 (js-property-get this (STR "length"))))
		  (llen (flonum->llong len))
		  (ht (make-hashtable)))
	      (extract-index-els-in-range this ht #l0 llen #t)
	      (let ((indices (hashtable-key-list ht)))
		 (let loop ((indices indices))
		    (unless (null? indices)
		       (let* ((index (car indices))
			      (other (- llen index #l1))
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
			      (let ((other-key (llong->js-string other)))
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
(define (shift-from-by a from::llong by::llong)
   (let* ((len (any->uint32 (js-property-get a (STR "length"))))
	  (llen (flonum->llong len))
	  (left-shift? (positivellong? by)) ;; left-shift
	  ;; when shifting (to the left) we might have to erase elements left
	  ;; of 'from'. lower-target limits these elements.
	  (lower-target (if left-shift?
			    (-llong from by)
			    from))
	  (ht (make-hashtable)))
      (extract-index-els-in-range a ht lower-target llen #t)
      (hashtable-for-each
       ht
       (lambda (index key/val)
	  (cond
	     ((<llong index lower-target)
	      'do-nothing)
	     ((<llong index from)
	      ;; can only happen during shift to left
	      ;; element that might be erased, but that won't be copied
	      (unless (hashtable-get ht (+llong index by))
		 (js-property-safe-delete! a (car key/val))))
	     (else
	      (let ((shifted-index-str (llong->js-string (-llong index by))))
		 ;; first copy current value
		 (js-property-generic-set! a shifted-index-str (cdr key/val) #f)
		 ;; then delete this one (if necessary)
		 (if left-shift?
		     (when (or (>=llong index (-llong llen by)) ;; right end
			       (not (hashtable-get ht (+llong index by))))
			(js-property-safe-delete! a (car key/val)))
		     ;; when right-shifting do not delete left area
		     ;; remember: 'by' is negative
		     (when (and (>=llong index (-llong from by)) ;;not left end
				(not (hashtable-get ht (+llong index by))))
			(js-property-safe-delete! a (car key/val)))))))))))

(define (shift)
   ;; 15.4.4.9
   (js-fun
    this #f #f (STR "Array.prototype.shift")
    ()
    (let ((len (any->uint32 (js-property-get this (STR "length")))))
       (if (=fl len 0.0)
	   (begin
	      (js-property-set! this (STR "length") 0.0)
	      (js-undefined))
	   (let ((res (js-property-get this (STR "0"))))
	      (shift-from-by this #l1 #l1)
	      (js-property-set! this (STR "length") (=fl len 1.0))
	      res)))))

(define (slice)
   ;; 15.4.4.10
      (js-fun
       this #f #f (STR "Array.prototype.slice")
       (start-any end-any)
       (let* ((new-a (js-new (global-read *jsg-Array*)))
	      (len (any->uint32 (js-property-get this (STR "length"))))
	      (start-int (any->integer start-any))
	      (start (if (<fl start-int 0.0)
			 (max-2fl 0.0 (+fl len start-int))
			 (min-2fl start-int len)))
	      (lstart (flonum->llong start))
	      (end-int (if (js-undefined? end-any)
			   len
			   (any->integer end-any)))
	      (end (if (<fl end-int 0.0)
		       (max-2fl 0.0 (+fl len end-int))
		       (min-2fl end-int len)))
	      (ht (make-hashtable)))
	  (extract-index-els-in-range this ht
				      lstart
				      (flonum->llong end) ;;end is handled here
				      #t)
	  (hashtable-for-each
	   ht
	   (lambda (index key/val)
	      (js-property-generic-set! new-a
					(llong->js-string (-llong index lstart))
					(cdr key/val)
					#f)))
	  (js-property-set! new-a
			    (STR "length")
			    (-fl end start))
	  new-a)))

(define (array-sort)
   ;; 15.4.4.10
   (js-fun
    this #f #f (STR "Array.prototype.sort")
    (compare-fn)
    (let* ((len (any->uint32 (js-property-get this (STR "length"))))
	   (ht (make-hashtable)))
       (extract-index-els-in-range this ht #l0 (flonum->llong len) #t)
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
					(cons (any->js-string el) el)))
				 els)
			    els))
	      (comp (if no-comp-fun?
			(lambda (x y)
			   (cond
			      ((and (car x) (car y))
			       (js-string<? (car x) (car y)))
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
				  (if (and (flonum? tmp) (<fl tmp 0.0))
				      #t ;; less
				      #f)))))))
	      (sorted (sort comp els-strs)))
	  (let loop ((i #l0)
		     (sorted sorted))
	     (unless (null? sorted)
		(js-property-generic-set! this
					  (llong->js-string i)
					  (if no-comp-fun?
					      (cdr (car sorted))
					      (car sorted))
					  #f)
		(loop (+llong i #l1)
		      (cdr sorted)))))
       this)))

(define (splice)
   ;; 15.4.4.12
   (js-fun
    this #f (nb-args get-arg) (STR "Array.prototype.splice")
    (start-any delete-count-any)
    (let* ((new-a (js-new (global-read *jsg-Array*)))
	   (len (any->uint32 (js-property-get this (STR "length"))))
	   (start-int (any->integer start-any))
	   (start (if (<fl start-int 0.0)
		      (max-2fl 0.0 (+fl len start-int))
		      (min-2fl start-int len)))
	   (lstart (flonum->llong start))
	   (delete-count-int (maxfl (any->integer delete-count-any)
				    0.0))
	   (end (min-2fl delete-count-int
			 (-fl len start)))
	   (lend (flonum->llong end))
	   (nb-inserted (-fx nb-args 2)))
       (js-property-set! new-a (STR "length") delete-count-int)
       (if (= delete-count-int nb-inserted)
	   ;; best case...
	   (let loop ((k 0)
		      (i lstart))
	      (if (>=llong i lend)
		  new-a
		  (let* ((k-str (integer->js-string k))
			 (i-str (llong->js-string i))
			 (old-val (js-property-contains this i-str))
			 (new-val (get-arg (+fx k 2))))
		     (when old-val
			(js-property-generic-set! new-a k-str old-val #f))
		     (js-property-set! this i-str new-val)
		     (loop (+fx k 1)
			   (+llong i #l1)))))
	   (begin
	      ;; copy cut-out elements into new array
	      (let ((ht (make-hashtable)))
		 (extract-index-els-in-range this ht lstart lend #t)
		 (hashtable-for-each
		  ht
		  (lambda (index key/val)
		     (when (>=llong index start)
			(js-property-generic-set!
			 new-a
			 (llong->js-string (-llong index start))
			 (cdr key/val)
			 #f)))))
	      ;; (un)shift elements
	      (let ((diff (- (flonum->llong delete-count-int)
			     (-fx nb-args 2))))
		 (shift-from-by this lend diff)
		 ;; copy arguments into array
		 (let loop ((i 0))
		    (unless (>= i (-fx nb-args 2))
		       (js-property-set! this
					 (llong->js-string (+ lstart i))
					 (get-arg (+fx i 2)))
		       (loop (+fx i 1))))
		 (js-property-set! this (STR "length")
				   (-fl len (llong->flonum diff)))
		 new-a))))))

(define (unshift)
   ;; 15.4.4.13
   (js-fun
    this #f (nb-args get-arg) (STR "Array.prototype.unshift")
    (first) ;; len 1
    (let* ((len (any->uint32 (js-property-get this (STR "length"))))
	   (new-len (+ len nb-args)))
       (unless (=fl len 0.0)
	  ;; remember: unshift requires negative 'by'
	  (shift-from-by this #l0 (fixnum->llong (- nb-args))))
       (let loop ((i 0))
	  (unless (< i nb-args)
	     (js-property-set! this (integer->js-string i) (get-arg i))
	     (loop (+fx i 1))))
       (js-property-set! this (STR "length") new-len)
       new-len)))
