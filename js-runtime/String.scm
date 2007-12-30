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
   (globals-tmp-add! (lambda () (global-runtime-add! 'String *js-String*)))

   (let* ((text-repr "function(v) {/* native String */ throw 'native';}")
	  (string-object (create-function-object *js-String*
						 (String-new)
						 String-construct
						 text-repr))
	  (prototype (instantiate::Js-String
			(props (make-props-hashtable))
			(proto (js-object-prototype))
			(str ""))))

      (set! *js-String-prototype* prototype)

      (js-property-generic-set! string-object
				"length"
				1.0
				(length-attributes))
      (js-property-generic-set! string-object
				"prototype"
				prototype
				(prototype-attributes))
      (js-property-generic-set! string-object
			        "fromCharCode"
				(fromCharCode)
				(built-in-attributes))

      (js-property-generic-set! prototype
				"constructor"
				*js-String*
				(constructor-attributes))
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
				(built-in-attributes))
      (js-property-generic-set! prototype
				"charCodeAt"
				(charCodeAt)
				(built-in-attributes))
      (js-property-generic-set! prototype
				"substring"
				(str-substring)
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
    (let* ((str (if (zero? nb-args)
		    ""
		    (any->string value)))
	   (str-len (string-length str))
	   (o (instantiate::Js-String
		 (props (make-props-hashtable))
		 (proto *js-String-prototype*)
		 (str str))))
       (js-property-generic-set! o "length"
				 (fixnum->flonum str-len)
				 (length-attributes))
       o)))

(define (String-construct f-o::Js-Function)
   #f)

(define (fromCharCode)
   (js-fun this #f (nb-args get-arg) "String.fromCharCode"
	   (c0) ;; length 1
	   (list->string (map (lambda (i)
				 (integer->char
				  (flonum->fixnum (any->uint16 (get-arg i)))))
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

(define (charCodeAt)
   ;; 15.5.4.5
   (js-fun this #f #f "String.charCodeAt"
	   (pos-any)
	   (let* ((str (any->string this))
		  (str-len (string-length str))
		  (pos (any->integer pos-any)))
	      (cond
		 ((<fl pos 0.0)
		  (NaN))
		 ((>= pos str-len)
		  (NaN))
		 (else
		  (fixnum->flonum
		   (char->integer
		    (string-ref str (flonum->fixnum pos)))))))))

(define (str-substring)
   ;; 15.5.4.15
   (js-fun this #f #f "String.substring"
	   (start-any end-any)
	   (let* ((str (any->string this))
		  (str-len (string-length str))
		  (start (flonum->fixnum (any->integer start-any)))
		  (end (if (js-undefined? end-any)
			   str-len
			   (flonum->fixnum (any->integer end-any))))
		  (start-bounded (minfx (maxfx start 0) str-len))
		  (end-bounded (minfx (maxfx end 0) str-len)))
	      (if (<fx start-bounded end-bounded)
		  (substring str start-bounded end-bounded)
		  (substring str end-bounded start-bounded)))))
