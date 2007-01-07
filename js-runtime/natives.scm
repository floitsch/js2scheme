(module jsre-natives
   (import jsre-object
	   jsre-exceptions)
   (export
    *js-Undefined*::Js-Object
    *js-Null*::Js-Object
    (class Js-Undefined::Js-Object)
    (class Js-Null::Js-Object)))

(define *js-Undefined* (undefined-primitive))
(define *js-Null* (null-primitive))

(define (undefined-primitive)
   (co-instantiate ((undefined (instantiate::Js-Undefined
				  (props (make-props-hashtable))
				  (proto undefined)
				  (fun (error-fun "no exec"))
				  (new (error-fun "no new")))))
      undefined))

(define (null-primitive)
   (co-instantiate ((null (instantiate::Js-Null
			     (props (make-props-hashtable))
			     (proto null)
			     (fun (error-fun "no exec"))
			     (new (error-fun "no new")))))
      null))

(define-method (js-property-one-level-contains o::Js-Undefined prop::bstring)
   #f)
(define-method (js-property-contains o::Js-Undefined prop::bstring)
   #f)
(define-method (js-property-generic-set! o::Js-Undefined prop::bstring new-val)
   (type-error "undefined"))
(define-method (js-property-direct-set! o::Js-Undefined
					prop::bstring
					new-entry::Property-entry)
   (type-error "undefined"))
(define-method (js-object->primitive o::Js-Undefined hint::symbol)
   *js-Undefined*)
(define-method (js-object->string::bstring o::Js-Undefined)
   "undefined")

(define-method (js-property-one-level-contains o::Js-Null prop::bstring)
   #f)
(define-method (js-property-contains o::Js-Null prop::bstring)
   #f)
(define-method (js-property-generic-set! o::Js-Null prop::bstring new-val)
   (type-error "null"))
(define-method (js-property-direct-set! o::Js-Null
					prop::bstring
					new-entry::Property-entry)
   (type-error "null"))
(define-method (js-object->primitive o::Js-Null hint::symbol)
   *js-Null*)
(define-method (js-object->string::bstring o::Js-Null)
   "null")
