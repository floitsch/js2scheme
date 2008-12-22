(module jsre-natives
   (import jsre-object)
   (use jsre-Error)
   (export
    (class Js-Undefined::Js-Object)
    (class Js-Null::Js-Object)
    *js-Undefined*::Js-Undefined
    *js-Null*::Js-Null
    
    (inline js-undefined::Js-Object)
    (inline js-undefined? v)
    (inline js-null::Js-Object)
    (inline js-null? v)))
;   (eval (class Js-Undefined)))

(define *js-Undefined* (undefined-primitive))
(define-inline (js-undefined) *js-Undefined*)
(define-inline (js-undefined? v) (eq? v *js-Undefined*))
(define *js-Null* (null-primitive))
(define-inline (js-null) *js-Null*)
(define-inline (js-null? v) (eq? v *js-Null*))

(define (undefined-primitive)
   (co-instantiate ((undefined (instantiate::Js-Undefined
				  (props #f)
				  (proto undefined))))
      undefined))

(define (null-primitive)
   (co-instantiate ((null (instantiate::Js-Null
			     (props #f)
			     (proto null))))
      null))

(define-method (js-property-one-level-contains? o::Js-Undefined prop::bstring)
   #f)
(define-method (js-property-is-enumerable? o::Js-Undefined prop::bstring)
   #f)
(define-method (add-enumerables o::Js-Undefined enumerables-ht shadowed-ht
				go-into-prototypes?::bool)
   'do-nothing)
(define-method (js-property-contains o::Js-Undefined prop::bstring)
   #f) ;; when null is the prototype, then it should simply return #f
(define-method (js-property-generic-set! o::Js-Undefined prop::bstring
					 new-val attributes)
   (type-error "can't set property of undefined" "undefined"))
(define-method (js-property-safe-delete! o::Js-Undefined prop::bstring)
   #t) ;; property is not in Object -> return true
(define-method (js-class-name::bstring o::Js-Undefined)
   "undefined")


(define-method (js-property-one-level-contains? o::Js-Null prop::bstring)
   #f)
(define-method (js-property-is-enumerable? o::Js-Null prop::bstring)
   #f)
(define-method (add-enumerables o::Js-Null enumerables-ht shadowed-ht
				go-into-prototypes?::bool)
   'do-nothing)
(define-method (js-property-contains o::Js-Null prop::bstring)
   #f) ;; when null is the prototype, then it should simply return #f
(define-method (js-property-generic-set! o::Js-Null prop::bstring
					 new-val attributes)
   (type-error "can't set property of null" "null"))
(define-method (js-property-safe-delete! o::Js-Null prop::bstring)
   #t) ;; property is not in Object -> return true
(define-method (js-class-name::bstring o::Js-Null)
   "null")
