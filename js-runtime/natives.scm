(module jsre-natives
   (import jsre-object
	   jsre-exceptions
	   jsre-globals-tmp
	   jsre-global-object
	   jsre-scope-object)
   (export
    (class Js-Undefined::Js-Object)
    (class Js-Null::Js-Object)
    (class Js-Undeclared::Js-Object)
    *js-Undefined*::Js-Undefined
    *js-Null*::Js-Null
    *js-Undeclared*::Js-Undeclared
    
    (inline js-undefined)
    (inline js-undefined? v)
    (inline js-null)
    (inline js-null? v)
    (inline js-undeclared)
    (inline js-undeclared? v)))
;   (eval (class Js-Undefined)))

(define *js-Undefined* (undefined-primitive))
(define-inline (js-undefined) *js-Undefined*)
(define-inline (js-undefined? v) (eq? v *js-Undefined*))
(define *js-Null* (null-primitive))
(define-inline (js-null) *js-Null*)
(define-inline (js-null? v) (eq? v *js-Null*))
(define *js-Undeclared* (undeclared-primitive))
(define-inline (js-undeclared) *js-Undeclared*)
(define-inline (js-undeclared? v) (eq? v *js-Undeclared*))

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

(define-method (js-property-contains o::Js-Undefined prop::bstring)
   #f) ;; when null is the prototype, then it should simply return #f
(define-method (js-property-generic-set! o::Js-Undefined prop::bstring
					 new-val attributes)
   (type-error "undefined"))
(define-method (js-property-update! o::Js-Undefined prop::bstring new-val)
   #f)
(define-method (js-property-safe-delete! o::Js-Undefined prop::bstring)
   #t) ;; property is not in Object -> return true
(define-method (js-object->string::bstring o::Js-Undefined)
   "undefined")


(define-method (js-property-contains o::Js-Null prop::bstring)
   #f) ;; when null is the prototype, then it should simply return #f
(define-method (js-property-generic-set! o::Js-Null prop::bstring
					 new-val attributes)
   (type-error "null"))
(define-method (js-property-update! o::Js-Null prop::bstring new-val)
   #f)
(define-method (js-property-safe-delete! o::Js-Null prop::bstring)
   #t) ;; property is not in Object -> return true
(define-method (js-object->string::bstring o::Js-Null)
   "null")


(define-method (js-property-contains o::Js-Undeclared prop::bstring)
   (undeclared-error #f))
(define-method (js-property-generic-set! o::Js-Undefined prop::bstring
					 new-val attributes)
   (undeclared-error #f))
(define-method (js-property-update! o::Js-Undeclared prop::bstring new-val)
   #f)
(define-method (js-property-safe-delete! o::Js-Undeclared prop::bstring)
   (undeclared-error #f))
(define-method (js-object->string::bstring o::Js-Undeclared)
   (undeclared-error #f))

