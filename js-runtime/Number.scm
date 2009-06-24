(module jsre-Number
   (import jsre-base-object
	   jsre-base-string
	   jsre-double)
   (use jsre-natives
	jsre-Error
	jsre-Object
	jsre-Date
	jsre-Function
	jsre-String
	jsre-Bool
	jsre-primitives
	jsre-conversion
	jsre-global-object
	jsre-scope-object
	)
   (export (class Js-Number::Js-Object
	      (value::double read-only))
	   *jsg-Number* ;; can be modified by user -> can't be ::procedure
	   *js-Number-orig*::procedure
	   (Number-init)))

(define *jsg-Number* #unspecified)
(define *js-Number-orig* (lambda () 'to-be-replaced))
(define *js-Number-prototype*::Js-Object (js-null))

(define-method (js-class-name::bstring o::Js-Number)
   (STR "Number"))

(define (Number-init)
   (set! *js-Number-orig* (Number-lambda))
   (set! *jsg-Number* (create-runtime-global (STR "Number") *js-Number-orig*))

   (let* ((text-repr (STR "function(v) {/*native Number*/ throw 'native'; }"))
	  (number-object (create-function-object *js-Number-orig*
						 (Number-new)
						 Number-construct
						 text-repr))
	  (prototype (instantiate::Js-Number       ;; 15.7.4
			(props (make-props-hashtable))
			(proto (js-object-prototype))
			(value 0.0))))
      (set! *js-Number-prototype* prototype)

      (js-property-generic-set! number-object ;; 15.7.3
				(STR "length")
				1.0
				(length-attributes))
      (js-property-generic-set! number-object ;; 15.7.3.1
				(STR "prototype")
				prototype
				(get-Attributes dont-enum dont-delete
						read-only))
      (js-property-generic-set! number-object ;; 15.7.3.2
				(STR "MAX_VALUE")
				;; TODO: MAX_VALUE
				;; for now close enough
				1.7976931348623157E308
				(get-Attributes dont-enum dont-delete
						read-only))
      (js-property-generic-set! number-object ;; 15.7.3.3
				(STR "MIN_VALUE")
				;; TODO: MIN_VALUE
				;; for now close enough
				4.9E-324
				(get-Attributes dont-enum dont-delete
						read-only))
      (js-property-generic-set! number-object ;; 15.7.3.4
				(STR "NaN")
				+nan.0
				(get-Attributes dont-enum dont-delete
						read-only))
      (js-property-generic-set! number-object ;; 15.7.3.5
				(STR "NEGATIVE_INFINITY")
				-inf.0
				(get-Attributes dont-enum dont-delete
						read-only))
      (js-property-generic-set! number-object ;; 15.7.3.6
				(STR "POSITIVE_INFINITY")
				+inf.0
				(get-Attributes dont-enum dont-delete
						read-only))

      (js-property-generic-set! prototype     ;; 15.7.4.1
				(STR "constructor")
				*js-Number-orig*
				(constructor-attributes))
      (js-property-generic-set! prototype     ;; 15.7.4.2
				(STR "toString")
				(toString)
				(built-in-attributes))
      (js-property-generic-set! prototype     ;; 15.7.4.3
				(STR "toLocaleString")
				(toString) ;; HACK: number locale string.
				(built-in-attributes))
      (js-property-generic-set! prototype     ;; 15.7.4.4
				(STR "valueOf")
				(valueOf)
				(built-in-attributes))
      (js-property-generic-set! prototype     ;; 15.7.4.5
				(STR "toFixed")
				(toFixed)
				(built-in-attributes))
      (js-property-generic-set! prototype     ;; 15.7.4.6
				(STR "toExponential")
				(toExponential)
				(built-in-attributes))
      (js-property-generic-set! prototype     ;; 15.7.4.7
				(STR "toPrecision")
				(toPrecision)
				(built-in-attributes))
				))

(define (Number-lambda)   ;; 15.7.1.1
   (js-fun-lambda
    #f
    #f
    (nb-args get-arg)
    ()
    (if (= nb-args 0)
	0.0
	(any->number (get-arg 0)))))

;; TODO: maybe 'new' should construct the object, so 'value' can be constant.
(define (Number-new)      ;; 15.7.2.1
   (js-fun-lambda
    #f
    #f
    (nb-args get-arg)
    ()
    (instantiate::Js-Number
       (props (make-props-hashtable))
       (proto *js-Number-prototype*)
       (value (if (= nb-args 0)
		  0.0
		  (any->number (get-arg 0)))))))
   
(define (Number-construct f-o::Js-Function)
   ;; Number-new always returns an Object.
   ;; so we can ignore this one.
   #f)

(define (toString)                    ;; 15.7.4.2 (and 15.7.4.3 toLocaleString)
   (js-fun this #f #f (STR "Number.valueOf")
	   (radix)
	   (define (convert radix)
	      (cond
		 ((not (Js-Number? this))
		  (type-error (STR "Number.toString applied to") this))
		 ((or (js-undefined? radix)
		      (and (flonum? radix)
			   (=fl radix 10.0)))
		  (any->js-string (Js-Number-value this)))
		 ;; we are allowed to return a limited form of toString here.
		 ;; spec explicitely allows a implementation dependent
		 ;; representation.
		 ((and (flonum? radix)
		       (>= radix 2.0)
		       (<= radix 36.0)
		       (=fl radix (any->integer radix)))
		  (llong->js-string (flonum->llong (Js-Number-value this))
				    (flonum->fixnum radix)))
		 
		 ;; unspecified in spec.
		 ;; v v v v v v v v v v 
		 ((flonum? radix) ;; outside boundary
		  (any->js-string (Js-Number-value this)))
		 (else
		  (convert (any->number radix)))))
	   (convert radix)))

(define (valueOf)                          ;; 15.7.4.4
   (js-fun this #f #f (STR "Number.valueOf")
	   ()
	   (if (not (Js-Number? this))
	       (type-error (STR "Number.valueOf applied to") this)
	       (Js-Number-value this))))

(define (toFixed)                          ;; 15.7.4.5
   (js-fun
    this #f #f (STR "Number.toFixed")
    (fraction-digits)
    (let ((f (any->integer fraction-digits)))
       (when (or (<fl f 0.0) (>fl f 20.0))
	  (range-error
	   (STR "invalide parameter to 'toFixed'. must be in range 0-20")
	   f))
       (when (not (Js-Number? this))
	  (type-error (STR "Number.toFixed applied to") this))
       (let ((x (Js-Number-value this)))
	  (cond
	     ((nanfl? x) (STR "NaN"))
	     ((or (>fl x 10e21)
		  (<fl x -10e21))
	      (utf8->js-string (double->string x 'shortest 0)))
	     (else
	      (utf8->js-string
	       (double->string x 'fixed (flonum->fixnum f)))))))))

(define (toExponential)                    ;; 15.7.4.6
   (js-fun
    this #f #f (STR "Number.toExponential")
    (fraction-digits)
    (when (not (Js-Number? this))
       (type-error (STR "Number.toExponential applied to") this))
    (let* ((x (Js-Number-value this))
	   (f (any->integer fraction-digits)))
       (cond
	  ((nanfl? x) (STR "NaN"))
	  ((infinitefl? x)
	   (if (<fl x 0.0)
	       (STR "-Infinity")
	       (STR "Infinity")))
	  ((js-undefined? fraction-digits)
	   (utf8->js-string (double->string x 'shortest-exponential 0)))
	  ((or (<fl f 0.0) (>fl f 20.0))
	   (range-error
	    (STR "invalid parameter to 'toExponential'. must be in range 0-20")
	    f))
	  (else
	   (utf8->js-string
	    (double->string x 'exponential (flonum->fixnum f))))))))

(define (toPrecision)                      ;; 15.7.4.7
   (js-fun
    this #f #f (STR "Number.toPrecision")
    (precision)
    (when (not (Js-Number? this))
       (type-error (STR "Number.toPrecision applied to") this))
    (let ((x (Js-Number-value this)))
       (if (js-undefined? precision)
	   (utf8->js-string (double->string x 'shortest 0))
	   (let ((p (any->integer precision)))
	      (cond
		 ((nanfl? x) (STR "NaN"))
		 ((infinitefl? x)
		  (if (<fl x 0.0)
		      (STR "-Infinity")
		      (STR "Infinity")))
		 ((or (<fl p 1.0) (>fl p 21.0))
		  (range-error
		   (STR "invalid parameter to 'toPrecision'. must be in range 1-21")
		   p))
		 (else
		  (utf8->js-string
		   (double->string x 'precision (flonum->fixnum p))))))))))
