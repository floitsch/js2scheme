(module jsre-Error
   (include "macros.sch")
   (use jsre-object
	jsre-Function
	jsre-Array
	jsre-Math
	jsre-Arguments
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
	jsre-global-object)
   (export *jsg-Error*
	   *jsg-Eval-Error*
	   *jsg-Range-Error*
	   *jsg-Reference-Error*
	   *jsg-Syntax-Error*
	   *jsg-Type-Error*
	   *jsg-URI-Error*
	   (Error-init)
	   (class Js-Error::Js-Object)
	   (range-error msg val)
	   (type-error msg val)
	   (undeclared-error id)
	   (syntax-error msg)
	   (eval-error msg)
	   (delete-error msg)
	   (uri-error msg)
	   (any->safe-string::bstring any)
	   (error->js-exception e)))

(define *jsg-Error* #unspecified)
(define *js-Error-orig* (lambda () 'to-be-replaced))

(define *jsg-Eval-Error* #unspecified)
(define *js-Eval-Error-orig* (lambda () 'to-be-replaced))

(define *jsg-Range-Error* #unspecified)
(define *js-Range-Error-orig* (lambda () 'to-be-replaced))

(define *jsg-Reference-Error* #unspecified)
(define *js-Reference-Error-orig* (lambda () 'to-be-replaced))

(define *jsg-Syntax-Error* #unspecified)
(define *js-Syntax-Error-orig* (lambda () 'to-be-replaced))

(define *jsg-Type-Error* #unspecified)
(define *js-Type-Error-orig* (lambda () 'to-be-replaced))

(define *jsg-URI-Error* #unspecified)
(define *js-URI-Error-orig* (lambda () 'to-be-replaced))

(define (Error-init)
   (define *error-prototype* #unspecified)
   
   (define (create-Error-class name native-error?)
      (let* ((proc (Error-lambda name))
	     (text-repr (string-append "function(msg) { /* native "
				       name
				       " */ throw 'native'; }"))
	     (error-object (create-function-object proc
						   (Error-new)
						   Error-construct
						   text-repr))
	     (prototype (instantiate::Js-Error
			   (props (make-props-hashtable))
			   ;; prototype is either object-prototype (15.11.4) or
			   ;; the Error-prototype (15.11.7.7)
			   (proto (if native-error?
				      *error-prototype*
				      (js-object-prototype))))))
	 
	 (unless native-error? (set! *error-prototype* prototype))

	 (js-property-generic-set! error-object  ;; 15.11.3 / 15.11.7 assumed
				   "length"
				   1.0
				   (length-attributes))
	 (js-property-generic-set! error-object  ;; 15.11.3.1 / 15.11.7.6
				   "prototype"
				   prototype
				   (get-Attributes dont-enum dont-delete
						   read-only))

	 (js-property-generic-set! prototype     ;; 15.11.4.1 / 15.11.7.8
				   "constructor"
				   proc
				   (constructor-attributes))

	 (js-property-generic-set! prototype     ;; 15.11.4.2 / 15.11.7.9
				   "name"
				   name
				   (built-in-attributes))
	 (js-property-generic-set! prototype     ;; 15.11.4.3 / 15.11.7.10
				   "message"
				   ""
				   (built-in-attributes))
	 (unless native-error?
	    (js-property-generic-set! prototype  ;; 15.11.4.4 
				      "toString"
				      (toString)
				      (built-in-attributes)))
	 proc))

   ;; 15.11
   (set! *js-Error-orig* (create-Error-class "Error" #f))
   (set! *jsg-Error* (create-runtime-global "Error" *js-Error-orig*))

   ;; 15.11.6.1
   (set! *js-Eval-Error-orig* (create-Error-class "EvalError" #t))
   (set! *jsg-Eval-Error* (create-runtime-global "EvalError"
						 *js-Eval-Error-orig*))

   ;; 15.11.6.2
   (set! *js-Range-Error-orig* (create-Error-class "RangeError" #t))
   (set! *jsg-Range-Error* (create-runtime-global "RangeError"
						  *js-Range-Error-orig*))

   ;; 15.11.6.3
   (set! *js-Reference-Error-orig* (create-Error-class "ReferenceError" #t))
   (set! *jsg-Reference-Error*
	 (create-runtime-global "ReferenceError"
				*js-Reference-Error-orig*))

   ;; 15.11.6.4
   (set! *js-Syntax-Error-orig* (create-Error-class "SyntaxError" #t))
   (set! *jsg-Syntax-Error*
	 (create-runtime-global "SyntaxError" *js-Syntax-Error-orig*))

   ;; 15.11.6.5
   (set! *js-Type-Error-orig* (create-Error-class "TypeError" #t))
   (set! *jsg-Type-Error* (create-runtime-global "TypeError"
						 *js-Type-Error-orig*))

   ;; 15.11.6.6
   (set! *js-URI-Error-orig* (create-Error-class "URIError" #t))
   (set! *jsg-URI-Error* (create-runtime-global "URIError"
						     *js-URI-Error-orig*)))


(define-method (js-class-name::bstring o::Js-Error)
   "Error")

(define (Error-lambda name)
   ;; 15.11.1.1 / 15.11.7.1
   (letrec ((error-proc (js-fun-lambda #f #f #f
				       (msg)
				       (if (string=? name "") ;; we need
					   ;; different lambdas with different hashes...
					   (js-new error-proc name) ;; can't happen
					   (js-new error-proc msg)))))
      error-proc))

(define (Error-new)
   ;; 15.11.2.1 / 15.11.7.4
   (js-fun-lambda this #f #f
		  (msg)
		  (unless (js-undefined? msg)
		     (js-property-set! this "message"
					    (any->safe-string msg)))
		  this))

(define (Error-construct::Js-Error f-o::Js-Function)
   (instantiate::Js-Error
      (props (make-props-hashtable))
      (proto (js-property-get f-o "prototype"))))

(define (toString)
   (js-fun this #f #f "Error.toString"
	   ()
	   (if (not (Js-Error? this))
	       "ERROR"
	       (format "~a: ~a"
		       (any->safe-string (js-property-get this "name"))
		       (any->safe-string (js-property-get this "message"))))))

(define (range-error msg val)
   (raise (js-new *js-Range-Error-orig*
		  (format "~a: ~a" msg (any->safe-string val)))))

(define (type-error msg val)
   (raise (js-new *js-Type-Error-orig*
		  (format "~a: ~a" msg (any->safe-string val)))))

(define (undeclared-error id)
   ;; TODO: is undeclared-error really a reference-error?
   (raise (js-new *js-Reference-Error-orig* id)))

(define (syntax-error msg)
   (raise (js-new *js-Syntax-Error-orig* msg)))

(define (eval-error msg)
   (raise (js-new *js-Eval-Error-orig* msg)))

(define (delete-error msg)
   (raise (js-new *js-Type-Error-orig*
		  (string-append "can't delete "
				 (any->safe-string msg)))))

(define (uri-error msg)
   (raise (js-new *js-URI-Error-orig* msg)))

(define (error->js-exception e)
   (cond
      ((&type-error? e)
       (js-new *js-Type-Error-orig*
	       (string-append (any->safe-string (&error-msg e))
			      ": "
			      (any->safe-string (&error-obj e)))))
      ((&error? e)
       (js-new *js-Error-orig*
	       (format "~a ~a\n~a:\n~a\n~a"
		       (&error-fname e)
		       (&error-location e)
		       (&error-proc e)
		       (any->safe-string (&error-msg e))
		       (any->safe-string (&error-obj e)))))
      ((&exception? e)
       (js-new *js-Error-orig*
	       "unknown exception"))
      (else e)))

(define (any->safe-string any)
   (cond
      ((or (string? any)
	   (js-null? any)
	   (js-undefined? any)
	   (boolean? any)
	   (flonum? any))
       (any->string any))
      ((Js-Arguments? any) "Arguments")
      ((Js-Array? any) "Array")
      ((Js-Bool? any) (if (Js-Bool-val any)
			  "Bool<true>"
			  "Bool<false>"))
      ((Js-Date? any) (format "Date<~a>"
			      (if (NaN? (Js-Date-t any))
				  "invalid"
				  (seconds->date
				   (flonum->elong
				    (/fl (Js-Date-t any) 1000.0))))))
      ((Js-Function? any) "Function-object")
      ((Js-Math? any) "Math")
      ((Js-Number? any) (format "Number<~a>" (Js-Number-value any)))
      ((Js-String? any) (format "String<~a>" (Js-String-str any)))
      ((Js-Error? any)
       (let ((name (js-property-get any "name"))
	     (msg (js-property-get any "message")))
	  (if (and (string? name)
		   (string? msg))
	      (string-append name ": " msg)
	      (format "Error <~a ~a>" name msg))))
      ((Js-Object? any) "Js-Object")
      (else
       (with-output-to-string (lambda ()
				 (write-circle any))))))
