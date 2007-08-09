(module jsre-natives
   (import jsre-object
	   jsre-exceptions
	   jsre-globals-tmp
	   jsre-global-object)
   (export
    (class Js-Undefined::Js-Object)
    (class Js-Null::Js-Object)
    (class Js-Undeclared::Js-Object)
    *js-Undefined*::Js-Undefined
    *js-Undefined-var* ;; changeable var
    *js-Null*::Js-Null
    *js-Undeclared*::Js-Undeclared
    (inline check-undeclared v id)
    ))
;   (eval (class Js-Undefined)))

(define *js-Undefined* (undefined-primitive))
(define *js-Null* (null-primitive))
(define *js-Undeclared* (undeclared-primitive))

;; the global 'undefined' variable
(define *js-Undefined-var* *js-Undefined*)
(globals-tmp-add! (lambda () (global-add! 'undefined *js-Undefined-var*)))

(define-inline (check-undeclared v id)
   (if (eq? v *js-Undeclared*)
       (undeclared-error id)
       v))

(define (undefined-primitive)
   (co-instantiate ((undefined (instantiate::Js-Undefined
				  (props (make-props-hashtable))
				  (proto undefined))))
      undefined))

(define (null-primitive)
   (co-instantiate ((null (instantiate::Js-Null
			     (props (make-props-hashtable))
			     (proto null))))
      null))

(define (undeclared-primitive)
   (co-instantiate ((undeclared (instantiate::Js-Undeclared
			     (props (make-props-hashtable))
			     (proto undeclared))))
      undeclared))

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
(define-method (js-object->string::bstring o::Js-Null)
   "null")


(define-method (js-property-one-level-contains o::Js-Undeclared prop::bstring)
   #f)
(define-method (js-property-contains o::Js-Undeclared prop::bstring)
   #f)
(define-method (js-property-generic-set! o::Js-Undefined prop::bstring new-val)
   (undeclared-error #f))
(define-method (js-property-direct-set! o::Js-Undeclared
					prop::bstring
					new-entry::Property-entry)
   (undeclared-error #f))
(define-method (js-object->string::bstring o::Js-Undeclared)
   (undeclared-error #f))

