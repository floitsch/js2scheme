(module jsre-Function
   (include "macros.sch")
   (import jsre-object
	   jsre-eval
	   jsre-Object ;; recursive dependency :(
	   jsre-Date
	   jsre-String
	   jsre-Number
	   jsre-Bool
	   jsre-natives
	   jsre-Error
	   jsre-primitives
	   jsre-conversion
	   jsre-global-object
	   jsre-scope-object
	   jsre-globals-tmp
	   )
   (export
    *js-Function* ;; can be modified by user -> can't be ::procedure
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
    (inline create-empty-object-lambda::Js-Object f-o::Js-Function)))

(define *js-Function* (tmp-js-object))
(define *js-Function-prototype* (tmp-js-object))

(define-method (js-object->string::bstring o::Js-Function)
   "Function")

(define *function-prototype-initialized?* #f)

(define (js-function-prototype)
   (unless *function-prototype-initialized?*
       (let ((proto (instantiate::Js-Object
		       (props (make-props-hashtable))
		       (proto (js-object-prototype)))))
	  (set! *js-Function-prototype* proto)
	  (set! *function-prototype-initialized?* #t)))
   *js-Function-prototype*)

(define (Function-init)
   (set! *js-Function* (Function-lambda))
   (register-function-object! *js-Function*
			      *js-Function* ;; new == lambda
			      (lambda (ignored) 'ignored)
			      (js-function-prototype)
			      1 ;; TODO
			      "TODO [native]")
   (globals-tmp-add! (lambda () (global-runtime-add! 'Function *js-Function*)))
   (let ((fun-object (procedure-object *js-Function*))
	 (prototype (js-function-prototype)))
      (js-property-generic-set! fun-object
			       "prototype"
			       prototype
			       (prototype-attributes))))
				  
(define-inline (create-empty-object-lambda::Js-Object f-o::Js-Function)
   (let ((proto (or (js-object (js-property-safe-get f-o "prototype"))
		    (js-object-prototype))))
      (instantiate::Js-Object
	 (props (make-props-hashtable))
	 (proto proto))))

(define *js-function-objects-ht* (make-hashtable #unspecified #unspecified eq?))

;; ECMA 13.2
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
      (js-property-generic-set! prototype-object
			       "constructor"
			       js-lambda
			       (constructor-attributes))
      (js-property-generic-set! fun-obj
			       "length"
			       (if (exact? length)
				   (exact->inexact length)
				   length)
			       (length-attributes))
      
      (js-property-generic-set! fun-obj
			       "prototype"
			       prototype-object
			       ;; ECMA 15.3.5.2
			       (dont-delete-attributes))))

(define (procedure-object::Js-Object p::procedure)
   (hashtable-get *js-function-objects-ht* p))

(define (Function-lambda)
   (js-fun
    #f #f ;; don't need 'this' and 'this-callee'
    (nb-args get-arg)
    ()
    (let loop ((i 0)
	       (args "")
	       (body ""))
       (cond
	  ((=fx i nb-args)
	   (js-Function-eval (string-append "(function (" args ") {"
					    body
					    "})")))
	  ((=fx i (- nb-args 1))
	   (loop (+ i 1) args (any->string (get-arg i))))
	  ((=fx i 0)
	   (loop (+ i 1) (any->string (get-arg i)) body))
	  (else
	   (loop (+ i 1)
		 (string-append args "," (any->string (get-arg i)))
		 body))))))
