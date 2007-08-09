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
	   jsre-exceptions
	   jsre-conversion
	   jsre-global-object
	   jsre-globals-tmp
	   )
   (export
    *js-Array* ;; can be modified by user -> can't be ::Js-Object
    *js-Array-prototype*::Js-Object
    (class Js-Array::Js-Object
       length::bint) ;; TODO: bint is too small.
    (Array-init)
    (js-array-literal length::int els::pair-nil)))

(define *js-Array* (tmp-js-object))
(define *js-Array-prototype* (tmp-js-object))

(define-method (js-property-one-level-contains o::Js-Array prop::bstring)
   (if (string=? prop "length")
       (exact->inexact (Js-Array-length o))
       (call-next-method)))

(define-method (js-property-contains o::Js-Array prop::bstring)
   (if (string=? prop "length")
       (Js-Array-length o)
       (call-next-method)))

(define-method (js-property-generic-set! o::Js-Array prop::bstring new-val)
   (define (property-index prop)
      ;; TODO
      #f)

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

(define-method (js-object->string::bstring o::Js-Array)
   ;; TODO
   "Array"
   )

(define (Array-init)
   ;; TODO not yet correct
;    (set! *js-Array-prototype* (instantiate::Js-Object
; 				 (props (make-props-hashtable))
; 				 (proto (js-object-prototype))))
;    (set! *js-Array* (instantiate::Js-Function
; 			(props (make-props-hashtable))
; 			(proto *js-Array-prototype*)
; 			(new Array-new)
; 			(construct (lambda () 'ignored))
; 			(text-repr "TODO [native]")))

   (set! *js-Array* Array-lambda)
   (register-function-object! Array-lambda
			      Array-new
			      Array-construct
			      (js-function-prototype) ;; TODO: what's the proto?
			      1
			      "TODO [native]")
   (globals-tmp-add! (lambda () (global-add! 'Array *js-Array*)))
   ;; TODO: add other properties (like prototype...) ?
   )

(define (fill-Array a nb-args get-arg)
    ;; TODO: touches numbers
    (if (and (= nb-args 1)
	     (number? (get-arg 0)))
	(let ((len (get-arg 0)))
	   (if (exact? len)
	       (js-property-set! a "length" len)
	       (let ((int-len (any->uint32 len)))
		  (if (= len int-len)
		      (js-property-set! a "length" int-len)
		      (range-error len)))))
	(let loop ((i 0))
	   (when (< i nb-args)
	      (js-property-set! a i (get-arg i))
	      (loop (+ i 1)))))
    a)

(define Array-lambda
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


(define Array-new
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
      (js-property-set! a "length" length)
      (for-each (lambda (el)
		   (let ((index (car el))
			 (val (cadr el)))
		      (js-property-set! a index val)))
		els)
      a))
