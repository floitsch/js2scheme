(module jsre-Error
   (include "macros.sch")
   (import jsre-object
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
	   (type-error msg val)
	   (undeclared-error id)
	   (syntax-error msg)
	   (eval-error msg)
	   (delete-error msg)
	   (any->safe-string::bstring any)
	   (error->js-exception e)))

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
   (set! *js-Error* (create-Error-class "Error" #f))
   (set! *js-Error-orig* *js-Error*)

   ;; 15.11.6.1
   (set! *js-Eval-Error* (create-Error-class "EvalError" #t))
   (set! *js-Eval-Error-orig* *js-Eval-Error*)

   ;; 15.11.6.2
   (set! *js-Range-Error* (create-Error-class "RangeError" #t))
   (set! *js-Range-Error-orig* *js-Range-Error*)

   ;; 15.11.6.3
   (set! *js-Reference-Error* (create-Error-class "ReferenceError" #t))
   (set! *js-Reference-Error-orig* *js-Reference-Error*)

   ;; 15.11.6.4
   (set! *js-Syntax-Error* (create-Error-class "SyntaxError" #t))
   (set! *js-Syntax-Error-orig* *js-Syntax-Error*)

   ;; 15.11.6.5
   (set! *js-Type-Error* (create-Error-class "TypeError" #t))
   (set! *js-Type-Error-orig* *js-Type-Error*)

   ;; 15.11.6.6
   (set! *js-URI-Error* (create-Error-class "URIError" #t))
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
	       (string-append (any->safe-string
			       (js-property-get this "name"))
			      ": "
			      (any->safe-string
			       (js-property-get this "message"))))))

(define (range-error val)
   (raise (js-new *js-Range-Error-orig* val)))

(define (type-error msg val)
   (raise (js-new *js-Type-Error-orig*
		  (format "~a: ~a" msg (any->safe-string val)))))

(define (undeclared-error id)
   ;; TODO: is undeclared-error really a reference-error?
   (raise (js-new *js-Reference-Error* id)))

(define (syntax-error msg)
   (raise (js-new *js-Syntax-Error* msg)))

(define (eval-error msg)
   (raise (js-new *js-Eval-Error* msg)))

(define (delete-error msg)
   (raise (js-new *js-Type-Error-orig*
		  (string-append "can't delete "
				 (any->safe-string msg)))))

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
   (define (double->string::bstring v::double)
      (cond
	 ((NaN? v) "NaN")
	 ((<fl v 0.0)
	  (string-append "-" (double->string (-fl 0.0 v))))
	 ((+infinity? v) "Infinity")
	 ((=fl (floorfl v) v)
	  (llong->string (flonum->llong v)))
	 (else
	  (number->string v))))
   (cond
      ((string? any) any)
      ((eq? any *js-Null*) "null")
      ((eq? any *js-Undefined*) "undefined")
      ((boolean? any) (if any
		       "true"
		       "false"))
      ((flonum? any) (double->string any))
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
