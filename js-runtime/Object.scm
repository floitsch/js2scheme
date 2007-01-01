(module jsre-Object
   (include "macros.sch")
   (import jsre-object
	   jsre-Function ;; recursive dependency :(
	   jsre-natives
	   )
   (export *js-Object* ;; will be exported from runtime -> can't be ::Js-Object
	   *js-Object-prototype*::Js-Object
	   (js-object-prototype)))

(define Object-fun
   ())
(define Object-new
   ())

(define (js-object-prototype)
   (or *js-Object-prototype*
       (let ((proto (instantiate::Js-Object
		       (props (make-props-hashtable))
		       (proto *js-Null*)
		       (fun error-fun)
		       (new error-fun))))
	  (set! *js-Object-prototype* proto)
	  proto)))

(define (Object-init)
   (set! *js-Object* (instantiate::Js-Object
			(props (make-props-hashtable))
			(proto (js-function-prototype))
			(fun Object-fun)
			(new Object-new))))
