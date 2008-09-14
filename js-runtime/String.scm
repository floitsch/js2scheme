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
	   jsre-RegExp
	   jsre-RegExp-match
	   jsre-RegExp-fsm
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
(define *js-String-prototype*::Js-Object (js-undeclared))

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
				"charAt"
				(charAt)
				(built-in-attributes))
      (js-property-generic-set! prototype
				"charCodeAt"
				(charCodeAt)
				(built-in-attributes))
      (js-property-generic-set! prototype
				"substring"
				(str-substring)
				(built-in-attributes))
      (js-property-generic-set! prototype
				"match"
				(match)
				(built-in-attributes))
      ))

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
	   (searchValue replaceValue) (any->string this)))

(define (js-string-ref o pos-any)         ;; 15.5.4.4 && 15.5.4.5
   (let* ((str (any->string o))
	  (str-len (string-length str))
	  (pos (any->integer pos-any)))
      (cond
	 ((<fl pos 0.0)
	  #f)
	 ((>= pos str-len)
	  #f)
	 (else
	  (string-ref str (flonum->fixnum pos))))))
   
(define (charAt)                         ;; 15.5.4.4
   (js-fun this #f #f "String.charAt"
	   (pos-any)
	   (let ((c (js-string-ref this pos-any)))
	      (if c
		  (string c)
		  ""))))

(define (charCodeAt)                    ;; 15.5.4.5
   (js-fun this #f #f "String.charCodeAt"
	   (pos-any)
	   (let ((c (js-string-ref this pos-any)))
	      (if c
		  (fixnum->flonum
		   (char->integer c))
		  (NaN)))))

(define (str-substring)                ;; 15.5.4.15
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

(define (match)                      ;; 15.5.4.10
   (js-fun
    this #f #f "String.match"
    (regexp)
    (let* ((re (if (Js-RegExp? regexp) ;; re does not need to be an object
		   regexp
		   (js-new *js-RegExp* regexp)))
	   (s (any->string this)))
       (when (not (Js-RegExp? re))
	  (type-error "RegExp required" re))
       (let ((global? (js-property-safe-get re "global")))
	  (if (not global?)
	      (js-call *js-RegExp-exec* re s)
	      ;; mostly copied from RegExp.
	      (let ((native-re (Js-RegExp-re re))
		    (len (string-length s))
		    (res-a (empty-js-Array)))
		 (let loop ((s-pos 0)
			    (a-index 0))
		    (cond
		       ((>fx s-pos len) ;; done
			(js-property-safe-set! re "lastIndex" 0.0)
			res-a)
		       ((regexp-match native-re s s-pos)
			=>
			(lambda (match)
			   (let ((start-index (car match))
				 (final-index (cadr match)))
			      (js-property-safe-set! res-a
						     (integer->string a-index)
						     (substring s
								start-index
								final-index))
			      (loop (if (=fx final-index s-pos)
					(+fx s-pos 1)
					final-index)
				    (+fx a-index 1)))))
		       (else ;; no match
			;; finished -> loop one last time and let the first
			;; condition handle the rest.
			(loop (+fx len 1) a-index))))))))))
