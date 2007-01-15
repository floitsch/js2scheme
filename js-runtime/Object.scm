(module jsre-Object
   (include "macros.sch")
   (import jsre-object
	   jsre-Function ;; recursive dependency :(
	   jsre-String
	   jsre-Number
	   jsre-Bool
	   jsre-natives
	   jsre-exceptions
	   jsre-primitives
	   jsre-conversion
	   )
   (export *js-Object* ;; can be modified by user -> can't be ::Js-Object
	   *js-Object-prototype*::Js-Object
	   (js-object-prototype)
	   (Object-init)))

(define *js-Object* (tmp-js-object))
(define *js-Object-prototype* (tmp-js-object))

(define *object-prototype-initialized?* #f)
(define (js-object-prototype)
   (if (not *object-prototype-initialized?*)
       (let ((proto (instantiate::Js-Object
		       (props (make-props-hashtable))
		       (proto *js-Null*))))
	  (set! *js-Object-prototype* proto)
	  (set! *object-prototype-initialized?* #t)))
   *js-Object-prototype*)

(define (Object-init)
   (set! *js-Object* Object-lambda)
   (register-function-object! Object-lambda
			      Object-new
			      Object-construct
			      (js-function-prototype)
			      1 ;; TODO
			      "TODO [native]")
   ;; TODO: add other attributes?
   )

(define (Object-lambda)
   'TODO
   )
(define (Object-new this f . L)
   'TODO)

(define (Object-construct c . L)
   ;; TODO
   (create-empty-object-lambda c))
