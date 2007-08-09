(module jsre-Date
   (include "macros.sch")
   (import jsre-object
	   jsre-exceptions
	   jsre-Object
	   jsre-Function
	   jsre-String
	   jsre-Number
	   jsre-Bool
	   jsre-natives
	   jsre-primitives
	   jsre-conversion
	   jsre-global-object
	   jsre-globals-tmp
	   )
   (export *js-Date* ;; can be modified by user -> can't be ::Js-Object
	   *js-Date-prototype*::Js-Object
	   (class Js-Date::Js-Object
	      bdate)
;	      value::double)
	   (Date-init)))

(define *js-Date* (tmp-js-object))
(define *js-Date-prototype* (tmp-js-object))

;; TODO: js-object->string

(define (Date-init)
   (set! *js-Date* Date-lambda)
   (register-function-object! Date-lambda
			      Date-new
			      Date-construct
			      (js-function-prototype)
			      1 ;; TODO
			      "TODO [native]")
   (globals-tmp-add! (lambda () (global-add! 'Date *js-Date*)))
   ;; TODO: add other properties (like prototype) ?

   (co-instantiate ((tmp (instantiate::Js-Date
			    (props (make-props-hashtable))
			    (proto tmp)
			    (bdate #f))))
      (set! *js-Date-prototype* tmp))
   (js-property-safe-set! *js-Date-prototype*
			  "getTimezoneOffset"
			  (js-fun #f #f #f ()
				  ;; TODO: number
				  0.0))
   (js-property-safe-set! *js-Date-prototype*
			  "valueOf"
			  valueOf))

(define Date-lambda
   (js-fun-lambda
    #f
    #f
    #f
    ()
    (date->string (current-date))))

(define Date-new
   (js-fun-lambda
    this
    #f
    (nb-args get-arg)
    ()
    (case nb-args
       ((0) (Js-Date-bdate-set! this (current-date)))
       ((1)
	(let ((prim (any->primitive (get-arg 0) #f)))
	   (if (string? prim)
	       (Js-Date-bdate-set! this (string->date prim))
	       (let* ((v (any->number prim))
		      (ms (time-clip v)))
		  (Js-Date-bdate-set! this
				      (and ms
					   (seconds->date (/ ms 1000))))))))
       (else
	(let* ((year (any->number (get-arg 0)))
	       (month (any->number (get-arg 1)))
	       (day (if (> nb-args 2)
			(any->number (get-arg 2))
			1))
	       (hours (if (> nb-args 3)
			  (any->number (get-arg 3))
			  0))
	       (minutes (if (> nb-args 4)
			    (any->number (get-arg 4))
			    0))
	       (seconds (if (> nb-args 5)
			    (any->number (get-arg 5))
			    0))
	       (ms (if (> nb-args 6)
		       (any->number (get-arg 6))
		       0))
	       (normalized-year (if (and (not (eq? *NaN* year))
					 (<=fl year 99.0))
				    (+fl 1900.0 year)
				    year)))
	   ;; TODO: convert each element to integer
	   ;; If not possible bdate-set! #f
	   ;; TODO: make-date should be #f if the date is not possible...
	   (Js-Date-bdate-set! this
			    (make-date seconds minutes hours
				       day month normalized-year)))))
    this))

(define (Date-construct c . L)
   (instantiate::Js-Date
      (props (make-props-hashtable))
      (proto *js-Date-prototype*)
      (bdate #f))) ;; TODO

(define (time-clip v)
   ;; TODO: care for NaN
   (cond
      ((or (eq? v *NaN*)
	   (eq? v *+infinity*)
	   (eq? v *-infinity*))
       #f)
      (else
       (flonum->elong v))))

;; always returns a date. If the str is not parseable anything can be returned.
(define (string->date str::bstring)
   ;; TODO
   (current-date))

(define valueOf
   (js-fun this #f #f ()
	   (cond
	      ((not (Js-Date? this))
	       (type-error "TODO"))
	      ((not (date? this))
	       *NaN*)
	      (else
	       (* 1000
		  (elong->flonum (date->seconds (Js-Date-bdate this))))))))
