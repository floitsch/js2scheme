(module jsre-Array
   (include "macros.sch")
   (import jsre-object
	   jsre-Object ;; recursive dependency :(
	   jsre-natives
	   )
   (export
    (class Js-Array::Js-Object
       length::bint))) ;; TODO: bint is too small.

(define-method (js-property-one-level-contains o::Js-Array prop::bstring)
   (if (string=? prop "length")
       (Js-Array-length o)
       (call-next-method)))

(define-method (js-property-contains o::Js-Array prop::bstring)
   (if (string=? prop "length")
       (Js-Array-length o)
       (call-next-method)))

(define-method (js-property-generic-set! o::Js-Array prop::bstring new-val)
   (with-access::Js-Array o (length)
      (if (string=? prop "length")
	  (let ((nb-int (any->integer new-val))
		(nb-uint32 (any->uint32 new-val)))
	     (if (=fl nb-int nb-uint32) ;; TODO: really not optimal
		 (set! length nb-uint32)
		 (range-error new-val)))
	  (let ((index (property-index prop)))
	     (if (and index
		      (<= length index))
		 (set! length (+ index 1)))
	     (call-next-method)))))

(define-method (js-object->primitive o::Js-Object)
   )
(define-method (js-object->string::bstring o::Js-Object)
   )

(define (js-function-prototype)
   (or *js-Function-prototype*
       (let ((proto (instantiate::Js-Object
		       (props (make-props-hashtable))
		       (proto (js-object-prototype))
		       (fun error-fun)
		       (new error-fun))))
	  (set! *js-Function-prototype* proto)
	  proto)))

(define (Object-init)
   (set! *js-Object* (instantiate::Js-Object
			(props (make-props-hashtable))
			(proto (js-function-prototype))
			(fun Object-fun)
			(new Object-new))))
