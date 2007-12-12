(module jsre-Error
   (include "macros.sch")
   (import jsre-object
	   jsre-Function
	   jsre-scope-object
	   jsre-primitives
	   jsre-natives
	   jsre-Date
	   jsre-Bool
	   jsre-String
	   jsre-Function
	   jsre-Object
	   jsre-Number
	   jsre-conversion
	   jsre-global-object
	   jsre-globals-tmp)
   (export *js-Error* ;; can be modified by user -> can't be ::Js-Object
	   *js-Eval-Error*
	   *js-Range-Error*
	   *js-Reference-Error*
	   *js-Syntax-Error*
	   *js-Type-Error*
	   *js-URI-Error*
	   (Error-init)
	   (class Js-Error::Js-Object)
	   (range-error val)
	   (type-error val)
	   (undeclared-error id)
	   (syntax-error msg)
	   (eval-error msg)
	   (delete-error msg)))

(define *js-Error* #unspecified)
(define *js-Error-orig* #unspecified)

(define *js-Eval-Error* #unspecified)
(define *js-Eval-Error-orig* #unspecified)

(define *js-Range-Error* #unspecified)
(define *js-Range-Error-orig* #unspecified)

(define *js-Reference-Error* #unspecified)
(define *js-Reference-Error-orig* #unspecified)

(define *js-Syntax-Error* #unspecified)
(define *js-Syntax-Error-orig* #unspecified)

(define *js-Type-Error* #unspecified)
(define *js-Type-Error-orig* #unspecified)

(define *js-URI-Error* #unspecified)
(define *js-URI-Error-orig* #unspecified)

(define (Error-init)
   (define (create-Error-class name)
      (let ((proc (Error-lambda)))
	 (register-function-object! proc
				    (Error-new)
				    Error-construct
				    (js-function-prototype)
				    1
				    "TODO [native]")
	 (let ((error-object (procedure-object proc))
	       (prototype (instantiate::Js-Error
			     (props (make-props-hashtable))
			     (proto (js-object-prototype)))))
	    (js-property-generic-set! error-object
				      "prototype"
				      prototype
				      (prototype-attributes))
	    (js-property-generic-set! prototype
				      "constructor"
				      *js-Error*
				      (built-in-attributes))
	    (js-property-generic-set! prototype
				      "name"
				      name
				      (built-in-attributes))
	    (js-property-generic-set! prototype
				      "message"
				      ""
				      (built-in-attributes))
	    (js-property-generic-set! prototype
				      "toString"
				      (toString)
				      (built-in-attributes))
	    proc)))
   
   (set! *js-Error* (create-Error-class "Error"))
   (set! *js-Error-orig* *js-Error*)

   (set! *js-Eval-Error* (create-Error-class "EvalError"))
   (set! *js-Eval-Error-orig* *js-Eval-Error*)

   (set! *js-Range-Error* (create-Error-class "RangeError"))
   (set! *js-Range-Error-orig* *js-Range-Error*)

   (set! *js-Reference-Error* (create-Error-class "ReferenceError"))
   (set! *js-Reference-Error-orig* *js-Reference-Error*)

   (set! *js-Syntax-Error* (create-Error-class "SyntaxError"))
   (set! *js-Syntax-Error-orig* *js-Syntax-Error*)

   (set! *js-Type-Error* (create-Error-class "TypeError"))
   (set! *js-Type-Error-orig* *js-Type-Error*)

   (set! *js-URI-Error* (create-Error-class "URIError"))
   (set! *js-URI-Error-orig* *js-URI-Error*)

   (globals-tmp-add!
    (lambda ()
       (global-runtime-add! 'Error *js-Error*)
       (global-runtime-add! 'EvalError *js-Eval-Error*)
       (global-runtime-add! 'RangeError *js-Range-Error*)
       (global-runtime-add! 'ReferenceError *js-Reference-Error*)
       (global-runtime-add! 'SyntaxError *js-Syntax-Error*)
       (global-runtime-add! 'TypeError *js-Type-Error*)
       (global-runtime-add! 'URIError *js-URI-Error*))))


(define-method (js-object->string::bstring o::Js-Error)
   "Error")

(define (Error-lambda)
   (letrec ((error-proc (js-fun-lambda
			 #f
			 #f
			 #f
			 (msg)
			 (js-new error-proc msg))))
      error-proc))

(define (Error-new)
   (js-fun-lambda
    this
    #f
    #f
    (msg)
    (unless (js-undefined? msg)
       (js-property-safe-set! this "message"
			      (any->string msg)))
    this))

(define (Error-construct::Js-Error f-o::Js-Function)
   (instantiate::Js-Error
      (props (make-props-hashtable))
      (proto (js-property-safe-get f-o "prototype"))))

(define (toString)
   (js-fun
    this #f #f ()
    (if (not (Js-Error? this))
	"ERROR"
	(string-append (any->string (js-property-safe-get this "name"))
		       ": "
		       (any->string (js-property-safe-get this "message"))))))

(define (range-error val)
   (raise (js-new *js-Range-Error-orig* val)))

(define (type-error val)
   (raise (js-new *js-Type-Error-orig* val)))

(define (undeclared-error id)
   ;; TODO: is undeclared-error really a reference-error?
   (raise (js-new *js-Reference-Error* id)))

(define (syntax-error msg)
   (raise (js-new *js-Syntax-Error* msg)))

(define (eval-error msg)
   (raise (js-new *js-Eval-Error* msg)))

(define (delete-error msg)
   (tprint *js-Type-Error-orig*)
   (raise (js-new *js-Type-Error-orig*
		  (string-append "can't delete "
				 (any->string msg)))))