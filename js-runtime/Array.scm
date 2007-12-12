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
    (js-array-literal length::int els::pair-nil)))

(define *js-Array* (tmp-js-object))
(define *js-Array-prototype* (tmp-js-object))

(define-method (js-property-contains o::Js-Array prop::bstring)
   (if (string=? prop "length")
       (exact->inexact (Js-Array-length o))
       (call-next-method)))

(define-method (js-property-generic-set! o::Js-Array prop::bstring
					 new-val attributes)
   (define (property-index prop)
      ;; TODO
      (let ((index (any->uint32 prop)))
	 (and (string=? (integer->string index) prop)
	      index)))

   (with-access::Js-Array o (length)
      (if (string=? prop "length")
	  (let ((nb-int (any->integer new-val))
		(nb-uint32 (any->uint32 new-val)))
	     (if (= nb-int nb-uint32) ;; TODO: really not optimal
		 (set! length nb-uint32)
		 (range-error new-val)))
	  (let ((index (property-index prop)))
	     (if (and index
		      (<= length index))
		 (set! length (+ index 1)))
	     (call-next-method)))))

(define-method (js-property-update! o::Js-Array prop::bstring new-val)
   (with-access::Js-Array o (length)
      (if (string=? prop "length")
	  (let ((nb-int (any->integer new-val))
		(nb-uint32 (any->uint32 new-val)))
	     (if (= nb-int nb-uint32) ;; TODO: really not optimal
		 (set! length nb-uint32)
		 (range-error new-val)))
	  (call-next-method))))

(define-method (js-object->string::bstring o::Js-Array)
   "Array")

(define (Array-init)
   (set! *js-Array* (Array-lambda))
   (register-function-object! *js-Array*
			      (Array-new)
			      Array-construct
			      (js-function-prototype) ;; TODO: what's the proto?
			      1
			      "TODO [native]")
   (globals-tmp-add! (lambda () (global-runtime-add! 'Array
						     *js-Array*)))
   (let ((array-object (procedure-object *js-Array*))
	 (prototype (instantiate::Js-Array
		       (props (make-props-hashtable))
		       (proto (js-object-prototype))
		       (length 0))))
      (set! *js-Array-prototype* prototype)
      (js-property-generic-set! array-object
				"prototype"
				prototype
				(prototype-attributes))
      (js-property-generic-set! prototype
			       "toString"
			       (toString)
			       (built-in-attributes))
      (js-property-generic-set! prototype
			       "concat"
			       (concat)
			       (built-in-attributes))
      (js-property-generic-set! prototype
			       "join"
			       (join)
			       (built-in-attributes))))

(define (fill-Array a nb-args get-arg)
    ;; TODO: touches numbers
    (if (and (= nb-args 1)
	     (number? (get-arg 0)))
	(let ((len (get-arg 0)))
	   (if (exact? len)
	       (js-property-safe-set! a "length" len)
	       (let ((int-len (any->uint32 len)))
		  (if (= len int-len)
		      (js-property-safe-set! a "length" int-len)
		      (range-error len)))))
	(let loop ((i 0))
	   (when (< i nb-args)
	      (js-property-safe-set! a (integer->string i) (get-arg i))
	      (loop (+ i 1)))))
    a)

(define (Array-lambda)
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
		      (js-property-safe-set! a index val)))
		els)
      a))

(define (join-array a sep)
   (define (join->string el)
      (if (or (js-undefined? el)
	      (js-null? el))
	  ""
	  (any->string el)))
   
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
   (js-fun this #f #f ()
	   (if (not (Js-Array? this))
	       (type-error (with-output-to-string
			      (lambda () (display-circle this))))
	       (join-array this (js-undefined)))))

(define (join)
   (js-fun this #f #f (sep) (join-array this sep)))

(define (arrays-concat nb-arrays get-array)
   (define (add-els new-a offset a)
      (let ((len (Js-Array-length a)))
	 (let loop ((i 0)
		    (j offset))
	    (unless (=fx i len)
	       (let ((prop (js-property-contains a (integer->string i))))
		  (when prop
		     (js-property-generic-set! new-a
					       (integer->string offset)
					       prop
					       #f))
		  (loop (+ i 1) (+ j 1)))))))
   
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
   (js-fun this #f (nb-args get-arg) (first-arg)
	   (arrays-concat (+ nb-args 1)
			  (lambda (i)
			     (if (zero? i)
				 this
				 (get-arg (- i 1)))))))
