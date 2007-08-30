(module jsre-primitives
   (include "macros.sch")
   (import jsre-object
	   jsre-natives ;; undefined, null, ...
	   jsre-exceptions
	   jsre-Object
	   jsre-Date
	   jsre-Function
	   jsre-String
	   jsre-Number
	   jsre-Bool
	   jsre-conversion)
   (export *js-global-this*::Js-Object
	   (inline primitive? v)
	   (inline js-property-get o prop)
	   (inline js-property-set! o prop new-val)
	   (inline +infinity::double)
	   (inline -infinity::double)
	   (inline NaN::double)
	   *+infinity*
	   *-infinity*
	   *NaN*
	   (inline +infinity?::bool v)
	   (inline -infinity?::bool v)
	   (inline NaN?::bool v)
	   (tmp-js-object)))

(define-inline (primitive? v)
   (not (Js-Object? v)))

(define *tmp-object* #f)
(define (tmp-js-object)
   (co-instantiate ((tmp (instantiate::Js-Object
			    (props (make-props-hashtable))
			    (proto tmp))))
      (set! *tmp-object* tmp)
      tmp))

(define *js-global-this* (tmp-js-object))

(define *+infinity* (/fl 1.0 0.0))
(define-inline (+infinity) *+infinity*)
(define-inline (+infinity? v) (eq? v *+infinity*))
(define *-infinity* (/fl -1.0 0.0))
(define-inline (-infinity) *-infinity*)
(define-inline (-infinity? v) (eq? v *-infinity*))
(define *NaN* (/fl 0.0 0.0))
(define-inline (NaN) *NaN*)
(define-inline (NaN? v) (eq? *NaN* v))

(define-inline (js-property-get o prop)
   ;; non-generic. but js-property-contains is.
   (define (js-property-safe-get o::Js-Object prop::bstring)
      ;(write-circle o)(print)
      ;(write-circle prop)(print)
      (let ((res (js-property-contains o prop)))
	 ;(write-circle res)(print)
	 (if res
	     (unmangle-false res)
	     *js-Undefined*)))

   (let ((o-typed (any->object o))
	 (prop-typed (any->string prop)))
      (js-property-safe-get o-typed prop-typed)))

(define-inline (js-property-set! o prop new-val)
   (let ((o-typed (any->object o))
	 (prop-typed (any->string prop)))
      (js-property-safe-set! o-typed prop-typed new-val)))
