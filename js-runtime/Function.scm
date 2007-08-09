(module jsre-Function
   (include "macros.sch")
   (import jsre-object
	   jsre-Object ;; recursive dependency :(
	   jsre-Date
	   jsre-String
	   jsre-Number
	   jsre-Bool
	   jsre-natives
	   jsre-exceptions
	   jsre-primitives
	   jsre-conversion
	   )
   (export
    *js-Function* ;; can be modified by user -> can't be ::Js-Object
    *js-Function-prototype*::Js-Object
    (class Js-Function::Js-Object
       new::procedure       ;; when called as a function. by default raises an error.
       construct::procedure ;; when called as constructor. Usually same as 'fun'.
       text-repr::bstring)
    (js-function-prototype)
    (register-function-object!
	    js-lambda::procedure
	    new
	    construct
	    prototype-object
	    length
	    text-repr)
    (procedure-object::Js-Object p::procedure)
    (Function-init)
    (inline create-empty-object-lambda::Js-Object f-o::Js-Function)
    *constructor-attributes*
    *prototype-attributes*
    *length-attributes*))

(define *js-Function* (tmp-js-object))
(define *js-Function-prototype* (tmp-js-object))

(define-method (js-object->string::bstring o::Js-Function)
   (Js-Function-text-repr o))

(define *function-prototype-initialized?* #f)

(define (js-function-prototype)
   (if (not *function-prototype-initialized?*)
       (let ((proto (instantiate::Js-Object
		       (props (make-props-hashtable))
		       (proto (js-object-prototype)))))
	  (set! *js-Function-prototype* proto)
	  (set! *function-prototype-initialized?* #t)))
   *js-Function-prototype*)

(define (Function-init)
   (set! *js-Function* Function-lambda)
   (register-function-object! Function-lambda
			      Function-new
			      (lambda () 'ignored)
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

(define-inline (create-empty-object-lambda::Js-Object f-o::Js-Function)
   (let ((proto (let ((prototype (js-object (js-property-get f-o "prototype"))))
		   (or prototype
		       *js-Object-prototype*))))
      (instantiate::Js-Object
	 (props (make-props-hashtable))
	 (proto proto))))

(define *js-function-objects-ht* (make-hashtable #unspecified #unspecified eq?))

(define (register-function-object! js-lambda
				   new
				   construct
				   prototype-object
				   length
				   text-repr)
   (let ((fun-obj (instantiate::Js-Function
		     (props (make-props-hashtable))
		     (proto *js-Function-prototype*)
		     (new new)
		     (construct construct)
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

(define (procedure-object::Js-Object p::procedure)
   (hashtable-get *js-function-objects-ht* p))

(define (Function-lambda)
   ;; TODO
   'TODO
   )

(define (Function-new)
   ;; TODO
   'TODO
   )
