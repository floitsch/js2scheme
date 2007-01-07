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
    (inline register-function-object!
	    js-lambda
	    new
	    prototype-object
	    length
	    text-repr)
    (Function-init)
    *constructor-attributes*
    *prototype-attributes*
    *length-attributes*))

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
		       (fun (error-fun "can't be invoked"))
		       (new (error-fun "can't be instantiated")))))
	  (set! *js-Function-prototype* proto)
	  (set! *function-prototype-initialized?* #t)))
   *js-Function-prototype*)

(define (Function-init)
   (set! *js-Function* Function-lambda)
   (register-function-object! Function-lambda
			      Function-new
			      (js-function-prototype)
			      1 ;; TODO
			      "TODO [native]")
   ;; TODO: add other attributes?
   )

;; TODO: correct attributes
(define *constructor-attributes* (instantiate::Attributes
				  (read-only #f)
				  (deletable #f)
				  (enumerable #f)))
				  
;; TODO: correct attributes
(define *prototype-attributes* (instantiate::Attributes
				  (read-only #f)
				  (deletable #f)
				  (enumerable #f)))
				  
;; TODO: correct attributes
(define *length-attributes* (instantiate::Attributes
			       (read-only #f)
			       (deletable #f)
			       (enumerable #f)))
				  
(define-inline (register-function-object! js-lambda
					  new
					  prototype-object
					  length
					  text-repr)
   (let ((fun-obj (instantiate::Js-Function
		     (props (make-props-hashtable))
		     (proto *js-Function-prototype*)
		     (fun js-lambda)
		     (new new)
		     (text-repr text-repr))))
      (hashtable-put! *js-function-objects-ht* js-lambda fun-obj)
      (js-property-direct-set! prototype-object
			       "constructor"
			       (instantiate::Property-entry
				  (val js-lambda)
				  (attr *constructor-attributes*)))
      (js-property-direct-set! fun-obj
			       "length"
			       (instantiate::Property-entry
				  (val length)
				  (attr *length-attributes*)))
      (js-property-direct-set! fun-obj
			       "prototype"
			       (instantiate::Property-entry
				  (val prototype-object)
				  (attr *prototype-attributes*)))))

(define (Function-lambda)
   ;; TODO
   'TODO
   )

(define (Function-new)
   ;; TODO
   'TODO
   )
