(module jsre-primitives
   (include "macros.sch")
   (import jsre-object
	   jsre-natives ;; undefined, null, ...
	   jsre-exceptions
	   jsre-Object
	   jsre-Function
	   jsre-String
	   jsre-Number
	   jsre-Bool
	   jsre-conversion)
   (export *js-global-this*::Js-Object
	   (inline js-property-get o prop)
	   (inline js-property-set! o prop new-val)
	   *+infinity* ;; TODO type it
	   *-infinity* ;; TODO type it
	   *NaN*
	   (tmp-js-object)))

(define *tmp-object* #f)
(define (tmp-js-object)
   (co-instantiate ((tmp (instantiate::Js-Object
			    (props (make-props-hashtable))
			    (proto tmp))))
      (set! *tmp-object* tmp)
      tmp))

(define *js-global-this* (tmp-js-object))

(define *+infinity* (/fl 1.0 0.0))
(define *-infinity* (/fl -1.0 0.0))
(define *NaN* 0.0) ;; TODO

(define-inline (js-property-get o prop)
   ;; non-generic. but js-property-contains is.
   (define (js-property-safe-get o::Js-Object prop::bstring)
      (let ((res (js-property-contains o prop)))
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
