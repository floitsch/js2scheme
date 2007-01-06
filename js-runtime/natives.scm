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
