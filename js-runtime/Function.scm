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
	   jsre-global-object
	   jsre-globals-tmp
	   )
   (export
    *js-Function* ;; can be modified by user -> can't be ::procedure
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
   (set! *js-Function* (Function-lambda))
   (register-function-object! *js-Function*
			      *js-Function* ;; new == lambda
			      (lambda (ignored) 'ignored)
			      (js-function-prototype)
			      1 ;; TODO
			      "TODO [native]")
   (globals-tmp-add! (lambda () (global-add! 'Function *js-Function*)))
   (let ((fun-object (procedure-object *js-Function*)))
      (js-property-direct-set! fun-object
			       "prototype"
			       (instantiate::Property-entry
				  (val (js-function-prototype))
				  (attr (prototype-attribute)))))
   ;; TODO: add other attributes
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
   (let ((proto (or (js-object (js-property-safe-get f-o "prototype"))
		    *js-Object-prototype*)))
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
   (js-fun
    #f #f ;; don't need 'this' and 'this-callee'
    (nb-args get-arg)
    ()
    (print "HERE")
    (let loop ((i 0)
	       (args "")
	       (body ""))
       (cond
	  ((=fx i nb-args)
	   (with-handler
	      (lambda (e) (syntax-error e))
	      (let* ((fun (string-append "(function (" args ") {"
					 body
					 "})"))
		     (scm-prog (js2scheme (open-input-string fun))))
		 (print fun)
		 (print scm-prog)
		 ;; we can't use the eval-library function, as the scope of the
		 ;; created function is only the global this (and not all
		 ;; visible variables.
		 (eval scm-prog))))
	  ((=fx i (- nb-args 1))
	   (loop (+ i 1) args (any->string (get-arg i))))
	  ((=fx i 0)
	   (loop (+ i 1) (any->string (get-arg i)) body))
	  (else
	   (loop (+ i 1)
		 (string-append args "," (any->string (get-arg i)))
		 body))))))
