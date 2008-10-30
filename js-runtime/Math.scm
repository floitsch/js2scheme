(module jsre-Math
   (include "macros.sch")
   (import jsre-object
	   jsre-Error
	   jsre-Object
	   jsre-Date
	   jsre-Function
	   jsre-String
	   jsre-Number
	   jsre-Bool
	   jsre-natives
	   jsre-primitives
	   jsre-conversion
	   jsre-global-object
	   jsre-scope-object
	   jsre-globals-tmp
	   )
   (export (class Js-Math::Js-Object)
	   *js-Math* ;; can be modified by user -> can't be ::Js-Object
	   (Math-init)))

;; 15.8 Math Object

(define *js-Math* #unspecified)

(define-method (js-class-name::bstring o::Js-Math)
   "Math")

(define (Math-init)
   (set! *js-Math* (instantiate::Js-Math
		      (props (make-props-hashtable))
		      (proto (js-object-prototype))))
   
   (globals-tmp-add! (lambda () (global-runtime-add! 'Math *js-Math*)))
   (js-property-generic-set! *js-Math* ;; 15.8.1.1
			     "E"
			     2.71828182845904523536
			     (get-Attributes dont-enum dont-delete read-only))
   (js-property-generic-set! *js-Math* ;; 15.8.1.2
			     "LN10"
			     2.3025850929940459011
			     (get-Attributes dont-enum dont-delete read-only))
   (js-property-generic-set! *js-Math* ;; 15.8.1.3
			     "LN2"
			     0.69314718055994528623
			     (get-Attributes dont-enum dont-delete read-only))
   (js-property-generic-set! *js-Math* ;; 15.8.1.4
			     "LOG2E"
			     1.442695040888963387
			     (get-Attributes dont-enum dont-delete read-only))
   (js-property-generic-set! *js-Math* ;; 15.8.1.5
			     "LOG10E"
			     0.43429448190325181667
			     (get-Attributes dont-enum dont-delete read-only))
   (js-property-generic-set! *js-Math* ;; 15.8.1.6
			     "PI"
			     3.14159265358979323846264338327950288419716939937510
			     (get-Attributes dont-enum dont-delete read-only))
   (js-property-generic-set! *js-Math* ;; 15.8.1.7
			     "SQRT1_2"
			     0.70710678118654757274
			     (get-Attributes dont-enum dont-delete read-only))
   (js-property-generic-set! *js-Math* ;; 15.8.1.8
			     "SQRT2"
			     1.4142135623730951455
			     (get-Attributes dont-enum dont-delete read-only))
   (js-property-generic-set! *js-Math*    ;; 15.8.2.1
			     "abs"
			     (abs)
			     (built-in-attributes))
   (js-property-generic-set! *js-Math*    ;; 15.8.2.2
			     "acos"
			     (acos)
			     (built-in-attributes))
   (js-property-generic-set! *js-Math*    ;; 15.8.2.3
			     "asin"
			     (asin)
			     (built-in-attributes))
   (js-property-generic-set! *js-Math*    ;; 15.8.2.4
			     "atan"
			     (atan)
			     (built-in-attributes))
   (js-property-generic-set! *js-Math*    ;; 15.8.2.5
			     "atan2"
			     (atan2)
			     (built-in-attributes))
   (js-property-generic-set! *js-Math*    ;; 15.8.2.6
			     "ceil"
			     (ceil)
			     (built-in-attributes))
   (js-property-generic-set! *js-Math*    ;; 15.8.2.7
			     "cos"
			     (cos)
			     (built-in-attributes))
   (js-property-generic-set! *js-Math*    ;; 15.8.2.8
			     "exp"
			     (exp)
			     (built-in-attributes))
   (js-property-generic-set! *js-Math*    ;; 15.8.2.9
			     "floor"
			     (floor)
			     (built-in-attributes))
   (js-property-generic-set! *js-Math*    ;; 15.8.2.10
			     "log"
			     (log)
			     (built-in-attributes))
   (js-property-generic-set! *js-Math*    ;; 15.8.2.11
			     "max"
			     (max)
			     (built-in-attributes))
   (js-property-generic-set! *js-Math*    ;; 15.8.2.12
			     "min"
			     (min)
			     (built-in-attributes))
   (js-property-generic-set! *js-Math*    ;; 15.8.2.13
			     "pow"
			     (pow)
			     (built-in-attributes))
   (js-property-generic-set! *js-Math*    ;; 15.8.2.14
			     "random"
			     (random-js)
			     (built-in-attributes))
   (js-property-generic-set! *js-Math*    ;; 15.8.2.15
			     "round"
			     (round)
			     (built-in-attributes))
   (js-property-generic-set! *js-Math*    ;; 15.8.2.16
			     "sin"
			     (sin)
			     (built-in-attributes))
   (js-property-generic-set! *js-Math*    ;; 15.8.2.17
			     "sqrt"
			     (sqrt)
			     (built-in-attributes))
   (js-property-generic-set! *js-Math*    ;; 15.8.2.18
			     "tan"
			     (tan)
			     (built-in-attributes)))

(define (abs)                           ;; 15.8.2.1
   (js-fun #f #f #f "Math.abs"
	   (val)
	   (absfl (any->number val))))

(define (acos)                          ;; 15.8.2.2
   (js-fun #f #f #f "Math.acos"
	   (val)
	   (acosfl (any->number val))))

(define (asin)                          ;; 15.8.2.3
   (js-fun #f #f #f "Math.asin"
	   (val)
	   (asinfl (any->number val))))

(define (atan)                          ;; 15.8.2.4
   (js-fun #f #f #f "Math.atan"
	   (val)
	   (atanfl (any->number val))))

(define (atan2)                         ;; 15.8.2.5
   (js-fun #f #f #f "Math.atan2"
	   (y x)
	   ;; ensure left-to-right order of eval.
	   (let* ((yfl (any->number y))
		  (xfl (any->number x)))
	      (atan-2fl-ur yfl xfl))))

(define (ceil)                          ;; 15.8.2.6
   (js-fun #f #f #f "Math.ceil"
	   (v)
	   (ceilingfl (any->number v))))

(define (cos)                           ;; 15.8.2.7
   (js-fun #f #f #f "Math.cos"
	   (v)
	   (cosfl (any->number v))))

(define (exp)                           ;; 15.8.2.8
   (js-fun #f #f #f "Math.exp"
	   (v)
	   (expfl (any->number v))))

(define (floor)                         ;; 15.8.2.9
   (js-fun #f #f #f "Math.floor"
	   (v)
	   (floorfl (any->number v))))

(define (log)                           ;; 15.8.2.11
   (js-fun #f #f #f "Math.log"
	   (v)
	   (logfl (any->number v))))

(define (max)                           ;; 15.8.2.11
   (js-fun
    #f #f
    (nb-args get-arg) "Math.max"
    (x y) ;; length 2
    (cond
       ((=fx nb-args 2)
	;; ensure left-to-right order
	(let* ((xfl (any->number x))
	       (yfl (any->number y)))
	   (maxfl xfl yfl)))
       ((=fx nb-args 0)
	(-infinity))
       ((=fx nb-args 1)
	(any->number x))
       (else
	(let loop ((m (any->number x))
		   (i 1))
	   (if (<fx i nb-args)
	       (loop (maxfl m (any->number (get-arg i)))
		     (+fx i 1))
	       m))))))

(define (min)                           ;; 15.8.2.12
   (js-fun
    #f #f
    (nb-args get-arg) "Math.min"
    (x y) ;; length 2
    (cond
       ((=fx nb-args 2)
	;; ensure left-to-right order
	(let* ((xfl (any->number x))
	       (yfl (any->number y)))
	   (minfl xfl yfl)))
       ((=fx nb-args 0)
	(+infinity))
       ((=fx nb-args 1)
	(any->number x))
       (else
	(let loop ((m (any->number x))
		   (i 1))
	   (if (<fx i nb-args)
	       (loop (minfl m (any->number (get-arg i)))
		     (+fx i 1))
	       m))))))

(define (pow)                           ;; 15.8.2.13
   (js-fun #f #f #f "Math.pow"
	   (x-any y-any)
	   (let* ((x (any->number x-any))
		  (y (any->number y-any)))
	      (cond
		 ((and (or (=fl x 1.0)
			   (=fl x -1.0))
		       (or (+infinity? y)
			   (-infinity? y)))
		  (NaN))
		 (else
		  (exptfl x y))))))

(define (random-js)                     ;; 15.8.2.14
   (js-fun #f #f #f "Math.random"
	   ()
	   ;; use same formula as Java.
	   (let* ((r1 (random (bit-lsh 1 26)))
		  (r2 (random (bit-lsh 1 27)))
		  (r1l (fixnum->llong r1))
		  (r2l (fixnum->llong r2))
		  (l (+llong (bit-lshllong r1l 27)
			     r2l)))
	      (/fl (llong->flonum l)
		   (llong->flonum (bit-lshllong #l1 53))))))

(define (round)                         ;; 15.8.2.15
   (js-fun #f #f #f "Math.round"
	   (v-any)
	   (let ((v (any->number v-any)))
	      (cond
		 ((or (>fl v 0.0)
		      (<fl v -0.5))
		  (floorfl (+fl v 0.5)))
		 ((=fl v 0.0) ;; could be -0.0 too
		  v)
		 (else -0.0)))))

(define (sin)                           ;; 15.8.2.16
   (js-fun #f #f #f "Math.sin"
	   (v)
	   (sinfl (any->number v))))

(define (sqrt)                          ;; 15.8.2.17
   (js-fun #f #f #f "Math.sqrt"
	   (v)
	   (sqrtfl-ur (any->number v))))

(define (tan)                           ;; 15.8.2.18
   (js-fun #f #f #f "Math.tan"
	   (v)
	   (tanfl (any->number v))))
