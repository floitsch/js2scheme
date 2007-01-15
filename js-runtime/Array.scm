(module jsre-Array
   (include "macros.sch")
   (import jsre-object
	   jsre-Object
	   jsre-Function
	   jsre-String
	   jsre-Number
	   jsre-Bool
	   jsre-natives
	   jsre-primitives
	   jsre-exceptions
	   jsre-conversion
	   )
   (export
    *js-Array* ;; can be modified by user -> can't be ::Js-Object
    *js-Array-prototype*::Js-Object
    (class Js-Array::Js-Object
       length::bint) ;; TODO: bint is too small.
    (Array-init)))

(define *js-Array* (tmp-js-object))
(define *js-Array-prototype* (tmp-js-object))

(define-method (js-property-one-level-contains o::Js-Array prop::bstring)
   (if (string=? prop "length")
       (Js-Array-length o)
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

(define-method (js-object->primitive o::Js-Object hint::symbol)
   ;; TODO
   'todo
   )
(define-method (js-object->string::bstring o::Js-Object)
   ;; TODO
   "some-string"
   )

(define (Array-init)
   ;; TODO not yet correct
   (set! *js-Array-prototype* (instantiate::Js-Object
				 (props (make-props-hashtable))
				 (proto (js-object-prototype))))
   (set! *js-Array* (instantiate::Js-Function
			(props (make-props-hashtable))
			(proto *js-Array-prototype*)
			(new Array-new)
			(construct (lambda () 'ignored))
			(text-repr "TODO [native]"))))

(define (Array-fun)
   ;; TODO
   'TODO
   )

(define (Array-new)
   ;; TODO
   'TODO
   )
