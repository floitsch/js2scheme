(module jsre-String
   (include "macros.sch")
   (import jsre-object
	   jsre-Object
	   jsre-Date
	   jsre-Array
	   jsre-Function
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
   (export *js-String* ;; can be modified by user -> can't be ::procedure
	   *js-String-orig*::procedure
	   (class Js-String::Js-Object
	      (str::bstring read-only))
	   (String-init)))

(define *js-String* #unspecified)
(define *js-String-orig* (lambda () #f))
(define *js-String-prototype* (tmp-js-object))

(define-method (js-object->string::bstring o::Js-String)
   "String")

(define (String-init)
   (set! *js-String* (String-lambda))
   (set! *js-String-orig* *js-String*)
   (register-function-object! *js-String*
			      (String-new)
			      String-construct
			      (js-function-prototype)
			      1
			      "TODO [native]")
   (globals-tmp-add! (lambda () (global-runtime-add! 'String *js-String*)))
   (let ((string-object (procedure-object *js-String*))
	 (prototype (instantiate::Js-String
		       (props (make-props-hashtable))
		       (proto (js-object-prototype))
		       (str ""))))
      (set! *js-String-prototype* prototype)
      (js-property-generic-set! string-object
				"prototype"
				prototype
				(prototype-attributes))
      (js-property-generic-set! string-object
			        "fromCharCode"
				(fromCharCode)
				(built-in-attributes))
      (js-property-generic-set! prototype
				"toString"
				(toString)
				(built-in-attributes))
      (js-property-generic-set! prototype
				"valueOf"
				(valueOf)
				(built-in-attributes))
      (js-property-generic-set! prototype
				"split"
				(split)
				(built-in-attributes))
      (js-property-generic-set! prototype
				"replace"
				(replace)
				(built-in-attributes))))

(define (String-lambda)
   (js-fun-lambda #f #f (nb-args get-arg) (first-arg)
		  (if (zero? nb-args)
		      ""
		      (any->string first-arg))))

(define (String-new)
   (js-fun-lambda
    #f
    #f
    (nb-args get-arg)
    (value)
    (instantiate::Js-String
       (props (make-props-hashtable))
       (proto *js-String-prototype*)
       (str (if (zero? nb-args)
		""
		(any->string value))))))

(define (String-construct f-o::Js-Function)
   #f)

(define (fromCharCode)
   (js-fun this #f (nb-args get-arg) "String.fromCharCode"
	   (c0) ;; length 1
	   (list->string (map (lambda (i)
				 (integer->char
				  (any->uint16 (get-arg i))))
			      (iota nb-args)))))

(define (toString)
   (js-fun this #f #f "String.toString"
	   ()
	   (if (not (Js-String? this))
	       (type-error "String-toString applied to" this)
	       (Js-String-str this))))
	   
(define (valueOf)
   (js-fun this #f #f "String.valueOf"
	   ()
	   (if (not (Js-String? this))
	       (type-error "String-valueOf applied to" this)
	       (Js-String-str this))))

(define (split)
   (js-fun this #f #f "String.split"
	   (delimiter)
	   (let* ((splitted (string-split (Js-String-str this) delimiter))
		  (len (length splitted)))
	      (js-array-literal len (map list (iota len) splitted)))))

(define (replace)
   (js-fun this #f #f "String.replace"
	   (seachValue replaceValue) (any->string this)))
