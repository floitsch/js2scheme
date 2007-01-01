(module jsre-Function
   (include "macros.sch")
   (import jsre-object
	   jsre-Object ;; recursive dependency :(
	   jsre-natives
	   )
   (export
    (class Js-Function::Js-Object
       text-repr::bstring)
    (js-function-prototype)))

(define-method (js-object->primitive o::Js-Function)
   (Js-Object-fun o))

(define-method (js-object->string::bstring o::Js-Function)
   (Js-Function-text-repr o))

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
