(module jsre-Error
   (import jsre-base-object
	   jsre-base-string)
   (use jsre-Function
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
	   (range-error msg::Js-Base-String val)
	   (type-error msg::Js-Base-String val)
	   (type-procedure-error val)
	   (undeclared-error id::Js-Base-String)
	   (syntax-error msg::Js-Base-String obj)
	   (eval-error)
	   (delete-error msg::Js-Base-String)
	   (uri-error msg::Js-Base-String)
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
	     (text-repr (js-string-append (STR "function(msg) { /* native ")
					  name
					  (STR " */ throw 'native'; }")))
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
				   (STR "length")
				   1.0
				   (length-attributes))
	 (js-property-generic-set! error-object  ;; 15.11.3.1 / 15.11.7.6
				   (STR "prototype")
				   prototype
				   (get-Attributes dont-enum dont-delete
						   read-only))

	 (js-property-generic-set! prototype     ;; 15.11.4.1 / 15.11.7.8
				   (STR "constructor")
				   proc
				   (constructor-attributes))

	 (js-property-generic-set! prototype     ;; 15.11.4.2 / 15.11.7.9
				   (STR "name")
				   name
				   (built-in-attributes))
	 (js-property-generic-set! prototype     ;; 15.11.4.3 / 15.11.7.10
				   (STR "message")
				   (STR "")
				   (built-in-attributes))
	 (unless native-error?
	    (js-property-generic-set! prototype  ;; 15.11.4.4 
				      (STR "toString")
				      (toString)
				      (built-in-attributes)))
	 proc))

   ;; 15.11
   (set! *js-Error-orig* (create-Error-class (STR "Error") #f))
   (set! *jsg-Error* (create-runtime-global (STR "Error") *js-Error-orig*))

   ;; 15.11.6.1
   (set! *js-Eval-Error-orig* (create-Error-class (STR "EvalError") #t))
   (set! *jsg-Eval-Error* (create-runtime-global (STR "EvalError")
						 *js-Eval-Error-orig*))

   ;; 15.11.6.2
   (set! *js-Range-Error-orig* (create-Error-class (STR "RangeError") #t))
   (set! *jsg-Range-Error* (create-runtime-global (STR "RangeError")
						  *js-Range-Error-orig*))

   ;; 15.11.6.3
   (set! *js-Reference-Error-orig* (create-Error-class (STR "ReferenceError") #t))
   (set! *jsg-Reference-Error*
	 (create-runtime-global (STR "ReferenceError")
				*js-Reference-Error-orig*))

   ;; 15.11.6.4
   (set! *js-Syntax-Error-orig* (create-Error-class (STR "SyntaxError") #t))
   (set! *jsg-Syntax-Error*
	 (create-runtime-global (STR "SyntaxError") *js-Syntax-Error-orig*))

   ;; 15.11.6.5
   (set! *js-Type-Error-orig* (create-Error-class (STR "TypeError") #t))
   (set! *jsg-Type-Error* (create-runtime-global (STR "TypeError")
						 *js-Type-Error-orig*))

   ;; 15.11.6.6
   (set! *js-URI-Error-orig* (create-Error-class (STR "URIError") #t))
   (set! *jsg-URI-Error* (create-runtime-global (STR "URIError")
						*js-URI-Error-orig*)))


(define-method (js-class-name::bstring o::Js-Error)
   "Error")

(define (Error-lambda name)
   ;; 15.11.1.1 / 15.11.7.1
   (letrec ((error-proc (js-fun-lambda #f #f #f
				       (msg)
				       (js-new error-proc msg))))
      error-proc))

(define (Error-new)
   ;; 15.11.2.1 / 15.11.7.4
   (js-fun-lambda this #f #f
		  (msg)
		  (unless (js-undefined? msg)
		     (js-property-set! this (STR "message")
				       (any->safe-js-string msg)))
		  this))

(define (Error-construct::Js-Error f-o::Js-Function)
   (instantiate::Js-Error
      (props (make-props-hashtable))
      (proto (js-property-get f-o (STR "prototype")))))

(define (toString)
   (js-fun this #f #f (STR "Error.prototype.toString")
	   ()
	   (if (not (Js-Error? this))
	       (STR "ERROR")
	       (js-string-append
		;; (format "~a: ~a" ...)
		(any->safe-js-string (js-property-get this (STR "name")))
		(STR ": ")
		(any->safe-js-string (js-property-get this
						      (STR "message")))))))

(define (range-error msg val)
   (raise (js-new *js-Range-Error-orig*
		  ;; (format "~a: ~a" msg (any->safe-string val)))))
		  (js-string-append msg (STR ": ")
				    (any->safe-js-string val)))))

(define (type-error msg val)
   (raise (js-new *js-Type-Error-orig*
		  ;; (format "~a: ~a" msg (any->safe-string val)))))
		  (js-string-append msg (STR ": ")
				    (any->safe-js-string val)))))
;; real reason for this procedure is that otherwise the js-call/etc. would
;; contain a literal string argument, thus complicating the STR handling.
(define (type-procedure-error val)
   (type-error (STR "function expected") val))

(define (undeclared-error id)
   ;; TODO: is undeclared-error really a reference-error?
   (raise (js-new *js-Reference-Error-orig* id)))

(define (syntax-error msg obj)
   (raise (js-new *js-Syntax-Error-orig*
		  (js-string-append msg
				    (STR ": ")
				    (any->safe-js-string obj)))))

(define (eval-error)
   (raise (js-new *js-Eval-Error-orig*
		  (STR "eval function must not be copied"))))

(define (delete-error msg)
   (raise (js-new *js-Type-Error-orig*
		  (js-string-append (STR "can't delete ") msg))))

(define (uri-error msg)
   (raise (js-new *js-URI-Error-orig* msg)))

(define (error->js-exception e)
   (tprint e)
   (cond
      ((&type-error? e)
       (js-new *js-Type-Error-orig*
	       (js-string-append (any->safe-js-string (&error-msg e))
				 (STR ": ")
				 (any->safe-js-string (&error-obj e)))))
      ((&error? e)
       (js-new *js-Error-orig*
	       ;; (format "~a ~a\n~a:\n~a\n~a"
	       (js-string-append
		(utf8->js-string
		 (format "~a ~a\n~a:\n"
			 (&error-fname e)
			 (&error-location e)
			 (&error-proc e)))
		(any->safe-js-string (&error-msg e))
		(STR "\n")
		(any->safe-js-string (&error-obj e)))))
      ((&exception? e)
       (js-new *js-Error-orig*
	       (STR "unknown exception")))
      (else e)))

(define (any->safe-string any)
   (js-string->utf8 (any->safe-js-string any)))

(define (any->safe-js-string any)
   (cond
      ((string? any)
       (utf8->js-string any))
      ((js-string? any)
       any)
      ((or (js-null? any)
	   (js-undefined? any)
	   (boolean? any)
	   (flonum? any))
       (any->js-string any))
      ((Js-Arguments? any) (STR "Arguments"))
      ((Js-Array? any) (STR "Array"))
      ((Js-Bool? any) (if (Js-Bool-val any)
			  (STR "Bool<true>")
			  (STR "Bool<false>")))
      ((Js-Date? any)
       ;; (format "Date<~a>"
       (js-string-append
	(STR "Date<")
	(if (nanfl? (Js-Date-t any))
	    (STR "invalid")
	    (utf8->js-string
	     (date->string (seconds->date (flonum->elong
					   (/fl (Js-Date-t any) 1000.0))))))
	(STR ">")))
      ((Js-Function? any) (STR "Function-object"))
      ((Js-Math? any) (STR "Math"))
      ((Js-Number? any)
       ;; (format "Number<~a>" (Js-Number-value any)))
       (js-string-append (STR "Number<")
			 (real->js-string (Js-Number-value any))
			 (STR ">")))
      ((Js-String? any)
       ;; (format "String<~a>" (Js-String-str any)))
       (js-string-append (STR "String<")
			 (Js-String-str any)
			 (STR ">")))
      ((Js-Error? any)
       (let ((name (js-property-get any (STR "name")))
	     (msg (js-property-get any (STR "message"))))
	  (if (and (js-string? name)
		   (js-string? msg))
	      (js-string-append name (STR ": ") msg)
	      (utf8->js-string (format "Error <~a ~a>" name msg)))))
      ((Js-Object? any) (STR "Js-Object"))
      (else
       (utf8->js-string
	(with-output-to-string (lambda ()
				  (write-circle any)))))))
