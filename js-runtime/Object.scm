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
		       (fun (error-fun "can't be invoked"))
		       (new (error-fun "can't be instantiated")))))
	  (set! *js-Object-prototype* proto)
	  (set! *object-prototype-initialized?* #t)))
   *js-Object-prototype*)

(define (Object-init)
   (set! *js-Object* Object-lambda)
   (register-function-object! Object-lambda
			      Object-new
			      (js-function-prototype)
			      1 ;; TODO
			      "TODO [native]")
   ;; TODO: add other attributes?
   )

(define (Object-lambda)
   ;; TODO
   'TODO
   )
(define (Object-new . L)
   ;; TODO
   (instantiate::Js-Object
      (props (make-props-hashtable))
      (proto *js-Object-prototype*)
      (fun (error-fun "can't be invoked"))
      (new (error-fun "can't be instantiated"))))
