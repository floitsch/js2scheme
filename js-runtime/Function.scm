(module jsre-Function
   (include "macros.sch")
   (import jsre-object
	   jsre-Object ;; recursive dependency :(
	   jsre-natives
	   jsre-exceptions
	   jsre-primitives
	   )
   (export
    (class Js-Function::Js-Object
       text-repr::bstring)
    (js-function-prototype)
    (Function-init)))

(define-method (js-object->primitive o::Js-Function hint::symbol)
   (Js-Object-fun o))

(define-method (js-object->string::bstring o::Js-Function)
   (Js-Function-text-repr o))

(define *function-prototype-initialized?* #f)

(define (js-function-prototype)
   (if (not *function-prototype-initialized?*)
       (let ((proto (instantiate::Js-Object
		       (props (make-props-hashtable))
		       (proto (js-object-prototype))
		       (fun error-fun)
		       (new error-fun))))
	  (set! *js-Function-prototype* proto)
	  (set! *function-prototype-initialized?* #t)))
   *js-Function-prototype*)

(define (Function-init)
   (set! *js-Function* (instantiate::Js-Function
			  (props (make-props-hashtable))
			  (proto (js-function-prototype))
			  (fun Function-fun)
			  (new Function-new)
			  (text-repr "TODO [native]"))))

(define (Function-fun)
   ;; TODO
   'TODO
   )

(define (Function-new)
   ;; TODO
   'TODO
   )
