(module jsre-Object
   (include "macros.sch")
   (import jsre-object
	   jsre-Function ;; recursive dependency :(
	   jsre-natives
	   jsre-exceptions
	   jsre-primitives
	   )
   (export (js-object-prototype)
	   (Object-init)))

(define *object-prototype-initialized?* #f)
(define (js-object-prototype)
   (if (not *object-prototype-initialized?*)
       (let ((proto (instantiate::Js-Object
		       (props (make-props-hashtable))
		       (proto *js-Null*)
		       (fun error-fun)
		       (new error-fun))))
	  (set! *js-Object-prototype* proto)
	  (set! *object-prototype-initialized?* #t)))
   *js-Object-prototype*)

(define (Object-init)
   (set! *js-Object* (instantiate::Js-Object
			(props (make-props-hashtable))
			(proto (js-function-prototype))
			(fun Object-fun)
			(new Object-new))))

(define (Object-fun)
   ;; TODO
   'TODO
   )
(define (Object-new)
   ;; TODO
   'TODO
   )
