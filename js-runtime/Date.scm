(module jsre-Date
   (include "macros.sch")
   (import jsre-object
	   jsre-Error
	   jsre-Object
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
   (include "date-impl.scm")
   (export *js-Date* ;; can be modified by user -> can't be ::procedure
	   (class Js-Date::Js-Object
	      ms::double)
	   (Date-init)))

(define *js-Date* #unspecified)
(define *js-Date-prototype*::Js-Object (js-undeclared))

;; TODO: js-object->string

(define (Date-init)
   (set! *js-Date* (Date-lambda))
   (globals-tmp-add! (lambda () (global-runtime-add! 'Date *js-Date*)))
   (let ((date-object (create-function-object *js-Date*
					      (Date-new)
					      Date-construct
					      "TODO [native]"))
	 (prototype (instantiate::Js-Date    ;; 15.9.5
		       (props (make-props-hashtable))
		       (proto (js-object-prototype))
		       (ms (NaN)))))

      (set! *js-Date-prototype* prototype)

      (js-property-generic-set! date-object    ;; 15.9.4
				"length"
				7.0
				(length-attributes))
      (js-property-generic-set! date-object    ;; 15.9.4.1
				"prototype"
				prototype
				(prototype-attributes))

      (js-property-generic-set! date-object       ;; 15.9.4.2
				"parse"
				(parse)
				(built-in-attributes))
      (js-property-generic-set! date-object       ;; 15.9.4.3
				"UTC"
				(UTC)
				(built-in-attributes))
      

      (js-property-generic-set! prototype         ;; 15.9.5.1
				"constructor"
				*js-Date*
				(constructor-attributes))
      (js-property-generic-set! prototype         ;; 15.9.5.2
				"toString"
				(toString)
				(built-in-attributes))
      (js-property-generic-set! prototype         ;; 15.9.5.3
				"toDateString"
				(toDateString)
				(built-in-attributes))
      (js-property-generic-set! prototype         ;; 15.9.5.4
				"toTimeString"
				(toTimeString)
				(built-in-attributes))
      (js-property-generic-set! prototype         ;; 15.9.5.5
				"toLocaleString"
				(toLocaleString)
				(built-in-attributes))
      (js-property-generic-set! prototype         ;; 15.9.5.6
				"toLocaleDateString"
				(toLocaleDateString)
				(built-in-attributes))
      (js-property-generic-set! prototype         ;; 15.9.5.7
				"toLocaleTimeString"
				(toLocaleTimeString)
				(built-in-attributes))
      (js-property-generic-set! prototype         ;; 15.9.5.8
				"valueOf"
				(valueOf)
				(built-in-attributes))
      (js-property-generic-set! prototype         ;; 15.9.5.9
				"getTime"
				(getTime)
				(built-in-attributes))
      (js-property-generic-set! prototype         ;; 15.9.5.10
				"getFullYear"
				(getFullYear)
				(built-in-attributes))
      (js-property-generic-set! prototype         ;; 15.9.5.11
				"getUTCFullYear"
				(getUTCFullYear)
				(built-in-attributes))
      (js-property-generic-set! prototype         ;; 15.9.5.12
				"getMonth"
				(getMonth)
				(built-in-attributes))
      (js-property-generic-set! prototype         ;; 15.9.5.13
				"getUTCMonth"
				(getUTCMonth)
				(built-in-attributes))
      (js-property-generic-set! prototype         ;; 15.9.5.14
				"getDate"
				(getDate)
				(built-in-attributes))
      (js-property-generic-set! prototype         ;; 15.9.5.15
				"getUTCDate"
				(getUTCDate)
				(built-in-attributes))
      (js-property-generic-set! prototype         ;; 15.9.5.16
				"getDay"
				(getDay)
				(built-in-attributes))
      (js-property-generic-set! prototype         ;; 15.9.5.17
				"getUTCDay"
				(getUTCDay)
				(built-in-attributes))
      (js-property-generic-set! prototype         ;; 15.9.5.18
				"getHours"
				(getHours)
				(built-in-attributes))
      (js-property-generic-set! prototype         ;; 15.9.5.19
				"getUTCHours"
				(getUTCHours)
				(built-in-attributes))
      (js-property-generic-set! prototype         ;; 15.9.5.20
				"getMinutes"
				(getMinutes)
				(built-in-attributes))
      (js-property-generic-set! prototype         ;; 15.9.5.21
				"getUTCMinutes"
				(getUTCMinutes)
				(built-in-attributes))
      (js-property-generic-set! prototype         ;; 15.9.5.22
				"getSeconds"
				(getSeconds)
				(built-in-attributes))
      (js-property-generic-set! prototype         ;; 15.9.5.23
				"getUTCSeconds"
				(getUTCSeconds)
				(built-in-attributes))
      (js-property-generic-set! prototype         ;; 15.9.5.24
				"getMilliseconds"
				(getMilliseconds)
				(built-in-attributes))
      (js-property-generic-set! prototype         ;; 15.9.5.25
				"getUTCMilliseconds"
				(getUTCMilliseconds)
				(built-in-attributes))
      (js-property-generic-set! prototype         ;; 15.9.5.26
				"getTimezoneOffset"
				(getTimezoneOffset)
				(built-in-attributes))
      (js-property-generic-set! prototype         ;; 15.9.5.27
				"setTime"
				(setTime)
				(built-in-attributes))
      (js-property-generic-set! prototype         ;; 15.9.5.28
				"setMilliseconds"
				(setMilliseconds)
				(built-in-attributes))
      (js-property-generic-set! prototype         ;; 15.9.5.29
				"setUTCMilliseconds"
				(setUTCMilliseconds)
				(built-in-attributes))
      (js-property-generic-set! prototype         ;; 15.9.5.30
				"setSeconds"
				(setSeconds)
				(built-in-attributes))
      (js-property-generic-set! prototype         ;; 15.9.5.31
				"setUTCSeconds"
				(setUTCSeconds)
				(built-in-attributes))
      (js-property-generic-set! prototype         ;; 15.9.5.33
				"setMinutes"
				(setMinutes)
				(built-in-attributes))
      (js-property-generic-set! prototype         ;; 15.9.5.34
				"setUTCMinutes"
				(setUTCMinutes)
				(built-in-attributes))
      (js-property-generic-set! prototype         ;; 15.9.5.35
				"setHours"
				(setHours)
				(built-in-attributes))
      (js-property-generic-set! prototype         ;; 15.9.5.36
				"setUTCHours"
				(setUTCHours)
				(built-in-attributes))
      (js-property-generic-set! prototype         ;; 15.9.5.36
				"setDate"
				(setDate)
				(built-in-attributes))
      (js-property-generic-set! prototype         ;; 15.9.5.37
				"setUTCDate"
				(setUTCDate)
				(built-in-attributes))
      (js-property-generic-set! prototype         ;; 15.9.5.38
				"setMonth"
				(setMonth)
				(built-in-attributes))
      (js-property-generic-set! prototype         ;; 15.9.5.39
				"setUTCMonth"
				(setUTCMonth)
				(built-in-attributes))
      (js-property-generic-set! prototype         ;; 15.9.5.40
				"setFullYear"
				(setFullYear)
				(built-in-attributes))
      (js-property-generic-set! prototype         ;; 15.9.5.41
				"setUTCFullYear"
				(setUTCFullYear)
				(built-in-attributes))
      (js-property-generic-set! prototype         ;; 15.9.5.42
				"toUTCString"
				(toUTCString)
				(built-in-attributes))
      ))

(define (Date-lambda) ;; 15.9.2.1
   (js-fun-lambda #f #f #f
    ()
    (let ((d (js-new *js-Date-prototype*)))
       (js-method-call d "toString"))))

(define (Date-new) ;; 15.9.3
   (js-fun-lambda this #f (nb-args get-arg)
    ()
    (case nb-args
       ((0) ;; 15.9.3.3
	(Js-Date-ms-set! this (llong->flonum
			       (/llong (current-microseconds) 1000))))
       ((1) ;; 15.9.3.2
	(let ((prim (any->primitive (get-arg 0) #f)))
	   (if (string? prim)
	       (Js-Date-ms-set! this (string->time prim))
	       (let* ((v (any->number prim))
		      (ms (time-clip v)))
		  (Js-Date-ms-set! this ms)))))
       (else ;; 15.9.3.1
	(let* ((year (any->number (get-arg 0)))
	       (month (any->number (get-arg 1)))
	       (date (if (> nb-args 2)
			 (any->number (get-arg 2))
			 1.0))
	       (hours (if (> nb-args 3)
			  (any->number (get-arg 3))
			  0.0))
	       (minutes (if (> nb-args 4)
			    (any->number (get-arg 4))
			    0.0))
	       (seconds (if (> nb-args 5)
			    (any->number (get-arg 5))
			    0.0))
	       (ms (if (> nb-args 6)
		       (any->number (get-arg 6))
		       0.0))
	       (normalized-year (if (and (not (eq? *NaN* year))
					 (<=fl year 99.0))
				    (+fl 1900.0 year)
				    year))
	       (js-day (make-js-day normalized-year month date))
	       (js-time (make-js-time hour min sec ms))
	       (js-date (make-js-date js-day js-time)))
	   (Js-Date-ms-set! this (time-clip (UTC js-date))))))
    this))

(define (Date-construct c . L)
   (instantiate::Js-Date
      (props (make-props-hashtable))
      (proto *js-Date-prototype*)
      (ms #f))) ;; TODO

(define (parse) ;; 15.9.4.2
   (js-fun this #f #f "Date.parse"
	   (str)
	   (string->time str)))

(define (UTC) ;; 15.9.4.3
   (js-fun
    this #f #f "Date.UTC"
    (year month date hours minutes seconds ms)
    (let* ((year (any->number year))
	   (month (any->number month))
	   (date (if (> nb-args 2)
		     (any->number (get-arg 2))
		     1.0))
	   (hours (if (> nb-args 3)
		      (any->number (get-arg 3))
		      0.0))
	   (minutes (if (> nb-args 4)
			(any->number (get-arg 4))
			0.0))
	   (seconds (if (> nb-args 5)
			(any->number (get-arg 5))
			0.0))
	   (ms (if (> nb-args 6)
		   (any->number (get-arg 6))
		   0.0))
	   (normalized-year (if (and (not (eq? *NaN* year))
				     (<=fl year 99.0))
				(+fl 1900.0 year)
				year))
	   (js-day (make-js-day normalized-year month date))
	   (js-time (make-js-time hour min sec ms))
	   (js-date (make-js-date js-day js-time)))
       (time-clip js-date))))



(define (toString)                       ;; 15.9.5.2
   (js-fun this #f #f "Date.toString"
	   ()
	   (cond
	      ((not (Js-Date? this))
	       (type-error "Date-toString applied to" this))
	      ((NaN? (Js-Date-ms this))
	       "Invalid Date")
	      (else
	       (time->string (Js-Date-ms this) #f)))))

(define (toDateString)                   ;; 15.9.5.3
   (js-fun this #f #f "Date.toDateString"
	   ()
	   (cond
	      ((not (Js-Date? this))
	       (type-error "Date-toDateString applied to" this))
	      ((NaN? (Js-Date-ms this))
	       "Invalid Date")
	      (else
	       (time->date-string (Js-Date-ms this))))))

(define (toTimeString)                   ;; 15.9.5.4
   (js-fun this #f #f "Date.toTimeString"
	   ()
	   (cond
	      ((not (Js-Date? this))
	       (type-error "Date-toTimeString applied to" this))
	      ((NaN? (Js-Date-ms this))
	       "Invalid Date")
	      (else
	       (time->time-string (Js-Date-ms this) #f)))))

(define (toLocaleString)                       ;; 15.9.5.5
   ;; TODO: adapt for locale strings.
   (js-fun this #f #f "Date.toLocaleString"
	   ()
	   (cond
	      ((not (Js-Date? this))
	       (type-error "Date-toLocaleString applied to" this))
	      ((NaN? (Js-Date-ms this))
	       "Invalid Date")
	      (else
	       (time->string (Js-Date-ms this) #f)))))

(define (toLocaleDateString)                   ;; 15.9.5.6
   ;; TODO: adapt for locale strings.
   (js-fun this #f #f "Date.toLocaleDateString"
	   ()
	   (cond
	      ((not (Js-Date? this))
	       (type-error "Date-toLocaleDateString applied to" this))
	      ((NaN? (Js-Date-ms this))
	       "Invalid Date")
	      (else
	       (time->date-string (Js-Date-ms this) #f)))))

(define (toLocaleTimeString)                   ;; 15.9.5.7
   ;; TODO: adapt for locale strings.
   (js-fun this #f #f "Date.toLocaleTimeString"
	   ()
	   (cond
	      ((not (Js-Date? this))
	       (type-error "Date-toLocaleTimeString applied to" this))
	      ((NaN? (Js-Date-ms this))
	       "Invalid Date")
	      (else
	       (time->time-string (Js-Date-ms this))))))

(define (valueOf)                              ;; 15.9.5.8
   (js-fun this #f #f "Date.valueOf"
	   ()
	   (cond
	      ((not (Js-Date? this))
	       (type-error "Date-valueOf applied to" this))
	      (else
	       (Js-Date-ms this)))))

(define (getTime)                              ;; 15.9.5.9
   (js-fun this #f #f "Date.getTime"
	   ()
	   (cond
	      ((not (Js-Date? this))
	       (type-error "Date-getTime applied to" this))
	      (else
	       (Js-Date-ms this)))))

(define (getFullYear)                          ;; 15.9.5.10
   (js-fun this #f #f "Date.getFullYear"
	   ()
	   (cond
	      ((not (Js-Date? this))
	       (type-error "Date-getFullYear applied to" this))
	      ((NaN? (Js-Date-ms this))
	       (NaN))
	      (else
	       (llong->flonum
		(year-from-time (flonum->llong
				 (local-time (Js-Date-ms this)))))))))

(define (getUTCFullYear)                       ;; 15.9.5.11
   (js-fun this #f #f "Date.getFullYear"
	   ()
	   (cond
	      ((not (Js-Date? this))
	       (type-error "Date-getUTCFullYear applied to" this))
	      ((NaN? (Js-Date-ms this))
	       (NaN))
	      (else
	       (fixnum->flonum
		(year-from-time (flonum->llong (Js-Date-ms this))))))))
   
(define (getMonth)                             ;; 15.9.5.12
   (js-fun this #f #f "Date.getMonth"
	   ()
	   (cond
	      ((not (Js-Date? this))
	       (type-error "Date-getMonth applied to" this))
	      ((NaN? (Js-Date-ms this))
	       (NaN))
	      (else
	       (fixnum->flonum
		(month-from-time (flonum->llong
				 (local-time (Js-Date-ms this)))))))))

(define (getUTCMonth)                          ;; 15.9.5.13
   (js-fun this #f #f "Date.getUTCMonth"
	   ()
	   (cond
	      ((not (Js-Date? this))
	       (type-error "Date-getUTCMonth applied to" this))
	      ((NaN? (Js-Date-ms this))
	       (NaN))
	      (else
	       (fixnum->flonum
		(month-from-time (flonum->llong (Js-Date-ms this))))))))

(define (getDate)                              ;; 15.9.5.14
   (js-fun this #f #f "Date.getDate"
	   ()
	   (cond
	      ((not (Js-Date? this))
	       (type-error "Date-getDate applied to" this))
	      ((NaN? (Js-Date-ms this))
	       (NaN))
	      (else
	       (fixnum->flonum
		(date-from-time (flonum->llong
				 (local-time (Js-Date-ms this)))))))))

(define (getUTCDate)                           ;; 15.9.5.15
   (js-fun this #f #f "Date.getUTCDate"
	   ()
	   (cond
	      ((not (Js-Date? this))
	       (type-error "Date-getUTCDate applied to" this))
	      ((NaN? (Js-Date-ms this))
	       (NaN))
	      (else
	       (fixnum->flonum
		(date-from-time (flonum->llong (Js-Date-ms this))))))))

(define (getDay)                              ;; 15.9.5.16
   (js-fun this #f #f "Date.getDay"
	   ()
	   (cond
	      ((not (Js-Date? this))
	       (type-error "Date-getDay applied to" this))
	      ((NaN? (Js-Date-ms this))
	       (NaN))
	      (else
	       (fixnum->flonum
		(week-day (flonum->llong (local-time (Js-Date-ms this)))))))))

(define (getUTCDay)                           ;; 15.9.5.17
   (js-fun this #f #f "Date.getUTCDay"
	   ()
	   (cond
	      ((not (Js-Date? this))
	       (type-error "Date-getUTCDay applied to" this))
	      ((NaN? (Js-Date-ms this))
	       (NaN))
	      (else
	       (fixnum->flonum
		(week-day (flonum->llong (Js-Date-ms this))))))))

(define (getHours)                             ;; 15.9.5.18
   (js-fun this #f #f "Date.getHours"
	   ()
	   (cond
	      ((not (Js-Date? this))
	       (type-error "Date-getHours applied to" this))
	      ((NaN? (Js-Date-ms this))
	       (NaN))
	      (else
	       (fixnum->flonum
		(hours-from-time
		 (flonum->llong (local-time (Js-Date-ms this)))))))))

(define (getUTCHours)                          ;; 15.9.5.19
   (js-fun this #f #f "Date.getUTCHours"
	   ()
	   (cond
	      ((not (Js-Date? this))
	       (type-error "Date-getUTCHours applied to" this))
	      ((NaN? (Js-Date-ms this))
	       (NaN))
	      (else
	       (fixnum->flonum
		(hours-from-time (flonum->llong (Js-Date-ms this))))))))

(define (getMinutes)                           ;; 15.9.5.20
   (js-fun this #f #f "Date.getMinutes"
	   ()
	   (cond
	      ((not (Js-Date? this))
	       (type-error "Date-getMinutes applied to" this))
	      ((NaN? (Js-Date-ms this))
	       (NaN))
	      (else
	       (fixnum->flonum
		(min-from-time
		 (flonum->llong (local-time (Js-Date-ms this)))))))))

(define (getUTCMinutes)                        ;; 15.9.5.21
   (js-fun this #f #f "Date.getUTCMinutes"
	   ()
	   (cond
	      ((not (Js-Date? this))
	       (type-error "Date-getUTCMinutes applied to" this))
	      ((NaN? (Js-Date-ms this))
	       (NaN))
	      (else
	       (fixnum->flonum
		(min-from-time (flonum->llong (Js-Date-ms this))))))))

(define (getSeconds)                           ;; 15.9.5.22
   (js-fun this #f #f "Date.getSeconds"
	   ()
	   (cond
	      ((not (Js-Date? this))
	       (type-error "Date-getSeconds applied to" this))
	      ((NaN? (Js-Date-ms this))
	       (NaN))
	      (else
	       (fixnum->flonum
		(seconds-from-time
		 (flonum->llong (local-time (Js-Date-ms this)))))))))

(define (getUTCSeconds)                        ;; 15.9.5.23
   (js-fun this #f #f "Date.getUTCSeconds"
	   ()
	   (cond
	      ((not (Js-Date? this))
	       (type-error "Date-getUTCSeconds applied to" this))
	      ((NaN? (Js-Date-ms this))
	       (NaN))
	      (else
	       (fixnum->flonum
		(seconds-from-time (flonum->llong (Js-Date-ms this))))))))

(define (getMilliseconds)                       ;; 15.9.5.24
   (js-fun this #f #f "Date.getMilliseconds"
	   ()
	   (cond
	      ((not (Js-Date? this))
	       (type-error "Date-getMilliseconds applied to" this))
	      ((NaN? (Js-Date-ms this))
	       (NaN))
	      (else
	       (fixnum->flonum
		(ms-from-time
		 (flonum->llong (local-time (Js-Date-ms this)))))))))

(define (getUTCMilliseconds)                       ;; 15.9.5.25
   (js-fun this #f #f "Date.getUTCMilliseconds"
	   ()
	   (cond
	      ((not (Js-Date? this))
	       (type-error "Date-getUTCMilliseconds applied to" this))
	      ((NaN? (Js-Date-ms this))
	       (NaN))
	      (else
	       (fixnum->flonum
		(ms-from-time (flonum->llong (Js-Date-ms this))))))))

(define (getTimezoneOffset)                        ;; 15.9.5.26
   (js-fun this #f #f "Date.getTimezoneOffset"
	   ()
	   (cond
	      ((not (Js-Date? this))
	       (type-error "Date-getTimezoneOffset applied to" this))
	      ((NaN? (Js-Date-ms this))
	       (NaN))
	      (else
	       (let ((t (Js-Date-ms this))
		     (ms-per-minute 60000.0))
		  (/fl (-fl t (local-time t))
		       ms-per-minute))))))

(define (setTime)                                  ;; 15.9.5.27
   (js-fun this #f #f "Date.setTime"
	   (t)
	   (cond
	      ((not (Js-Date? this))
	       (type-error "Date-setTime applied to" this))
	      (else
	       (with-access::Js-Date this (ms)
		  (set! ms (time-clip (any->number t)))
		  ms)))))

(define (replace-milliseconds n new-ms)
   (let ((without-ms (-fl n (remainderfl n 1000.0))))
      (+fl without-ms new-ms)))

(define (setMilliseconds)                          ;; 15.9.5.28
   (js-fun this #f #f "Date.setMilliseconds"
	   (new-ms)
	   ;; new-ms is not the new value of 'ms' but the new
	   ;; ms-parts of the time.
	   (cond
	      ((not (Js-Date? this))
	       (type-error "Date-setMilliseconds applied to" this))
	      ((NaN? (Js-Date-ms this))
	       (Js-Date-ms this))
	      (else
	       ;; UTC and local-time are ridiculous, but if the
	       ;; date is completely at the limit then it could make a
	       ;; difference...
	       (with-access::Js-Date this (ms)
		  (set! ms (UTC (time-clip
				 (replace-milliseconds (local-time ms)
						       (any->number new-ms)))))
		  ms)))))

(define (setUTCMilliseconds)                        ;; 15.9.5.29
   (js-fun this #f #f "Date.setUTCMilliseconds"
	   (new-ms)
	   ;; new-ms is not the new value of 'ms' but the new
	   ;; ms-parts of the time.
	   (cond
	      ((not (Js-Date? this))
	       (type-error "Date-setUTCMilliseconds applied to" this))
	      ((NaN? (Js-Date-ms this))
	       (Js-Date-ms this))
	      (else
	       (with-access::Js-Date this (ms)
		  (set! ms
			(replace-milliseconds ms (any->number new-ms)))
		  ms)))))


(define (replace-seconds n s ms)
   (if (not ms)
       (replace-seconds n s (remainderfl n 1000.0))
       (let ((w/o-secs (-fl n (remainderfl n *ms-per-minute*))))
	  (+fl (+fl w/o-secs s) ms))))

(define (setSeconds)                                  ;; 15.9.5.30
   (js-fun this #f (nb-args get-arg)
	   "Date.setSeconds"
	   (new-sec new-ms)
	   (cond
	      ((not (Js-Date? this))
	       (type-error "Date-setSeconds applied to" this))
	      ((NaN? (Js-Date-ms this))
	       (Js-Date-ms this))
	      (else
	       ;; UTC and local-time are ridiculous here, but if the
	       ;; date is completely at the limit then it could make a
	       ;; difference...
	       (with-access::Js-Date this (ms)
		  (set! ms (time-clip
			    (UTC (replaceSeconds (local-time ms)
						 (any->number new-sec)
						 (if (>=fx nb-args 2)
						     (any->number new-ms)
						     #f)))))
		  ms)))))

(define (setUTCSeconds)                               ;; 15.9.5.31
   (js-fun this #f (nb-args get-arg)
	   "Date.setUTCSeconds"
	   (new-sec new-ms)
	   (cond
	      ((not (Js-Date? this))
	       (type-error "Date-setUTCSeconds applied to" this))
	      ((NaN? (Js-Date-ms this))
	       (Js-Date-ms this))
	      (else
	       (with-access::Js-Date this (ms)
		  (set! ms (time-clip (replaceSeconds ms
						      (any->number new-sec)
						      (if (>=fx nb-args 2)
							  (any->number new-ms)
							  #f))))
		  ms)))))

;; Note: ECMAScript spec does not contain any 15.9.5.32 (MS Word...)

(define (replace-minutes n min s ms)
   (cond
      ((not s)
       (replace-minutes n min (remainderfl n *ms-per-minute*) 0.0))
      ((not ms)
       (replace-minutes n min s (remainderfl n 1000.0)))
      (else
       (let ((w/o-mins (-fl n (remainderfl n *ms-per-minutes*))))
	  (+fl (+fl w/o-mins min)
	       (+fl s ms))))))

(define (setMinutes)                                  ;; 15.9.5.33
   (js-fun this #f (nb-args get-arg)
	   "Date.setMinutes"
	   (new-min new-sec new-ms)
	   (cond
	      ((not (Js-Date? this))
	       (type-error "Date-setMinutes applied to" this))
	      ((NaN? (Js-Date-ms this))
	       (Js-Date-ms this))
	      (else
	       (with-access::Js-Date this (ms)
		  (let ((min-n (any->number new-min))
			(maybe-sec (if (>=fx nb-args 1)
				       (any->number new-sec)
				       #f))
			(maybe-ms (if (>=fx nb-args 2)
				      (any->number new-ms)
				      #f)))
		     (set! ms (time-clip (UTC (replace-minutes (local-time ms)
							       min-n
							       maybe-sec
							       maybe-ms)))))
		  ms)))))

(define (setUTCMinutes)                               ;; 15.9.5.34
   (js-fun this #f (nb-args get-arg)
	   "Date.setUTCMinutes"
	   (new-min new-sec new-ms)
	   (cond
	      ((not (Js-Date? this))
	       (type-error "Date-setUTCMinutes applied to" this))
	      ((NaN? (Js-Date-ms this))
	       (Js-Date-ms this))
	      (else
	       (with-access::Js-Date this (ms)
		  (let ((min-n (any->number new-min))
			(maybe-sec (if (>=fx nb-args 1)
				       (any->number new-sec)
				       #f))
			(maybe-ms (if (>=fx nb-args 2)
				      (any->number new-ms)
				      #f)))
		     (set! ms (time-clip (replaceMinutes ms
							 min-n
							 maybe-sec
							 maybe-ms)))
		     ms))))))

(define (replace-hours n h min s ms)
   (cond
      ((not min)
       (replace-hours n h (modulofl n *ms-per-hour*) 0.0 0.0))
      ((not s)
       (replace-hours n h min (modulofl n *ms-per-minute*) 0.0))
      ((not ms)
       (replace-hours n h min s (modulofl n 1000.0)))
      (else
       (let ((w/o-hours (-fl n (modulofl n *ms-per-day*))))
	  (+fl h
	       (+fl (+fl w/o-mins min)
		    (+fl s ms)))))))

(define (setHours)                                    ;; 15.9.5.35
   (js-fun this #f (nb-args get-arg)
	   "Date.setHours"
	   (new-hour new-min new-sec new-ms)
	   (cond
	      ((not (Js-Date? this))
	       (type-error "Date-setHours applied to" this))
	      ((NaN? (Js-Date-ms this))
	       (Js-Date-ms this))
	      (else
	       (with-access::Js-Date this (ms)
		  (let ((hours-n (any->number new-hour))
			(maybe-min (if (>=fx nb-args 1)
				       (any->number new-min)
				       #f))
			(maybe-sec (if (>=fx nb-args 1)
				       (any->number new-sec)
				       #f))
			(maybe-ms (if (>=fx nb-args 2)
				      (any->number new-ms)
				      #f)))
		     (set! ms (time-clip (UTC (replacehours (local-time ms)
							    hours-n
							    maybe-min
							    maybe-sec
							    maybe-ms))))
		     ms))))))

(define (setUTCHours)                                 ;; 15.9.5.36
   (js-fun this #f (nb-args get-arg)
	   "Date.setUTCHours"
	   (new-hour new-min new-sec new-ms)
	   (cond
	      ((not (Js-Date? this))
	       (type-error "Date-setUTCHours applied to" this))
	      ((NaN? (Js-Date-ms this))
	       (Js-Date-ms this))
	      (else
	       (with-access::Js-Date this (ms)
		  (let ((hours-n (any->number new-hour))
			(maybe-min (if (>=fx nb-args 1)
				       (any->number new-min)
				       #f))
			(maybe-sec (if (>=fx nb-args 1)
				       (any->number new-sec)
				       #f))
			(maybe-ms (if (>=fx nb-args 2)
				      (any->number new-ms)
				      #f)))
		     (set! ms (time-clip (replacehours ms
						       hours-n
						       maybe-min
						       maybe-sec
						       maybe-ms))))
		  ms)))))

(define (replace-date n d)
   (let ((y (fixnum->flonum (year-from-time n)))
	 (m (fixnum->flonum (month-from-time n))))
      (make-js-date (make-js-day y m d) (time-within-day n))))

;; Note: there are two 15.9.5.36 subsections...

(define (setDate)                                     ;; 15.9.5.36
   (js-fun this #f #f "Date.setDate"
	   (new-date)
	   (cond
	      ((not (Js-Date? this))
	       (type-error "Date-setDate applied to" this))
	      ((NaN? (Js-Date-ms this))
	       (Js-Date-ms this))
	      (else
	       (with-access::Js-Date this (ms)
		  (let ((l-t (local-time ms))
			(date-n (any->number new-date)))
		     (set! ms (time-clip (UTC (replace-date l-t date-n))))
		     ms))))))

(define (setUTCDate)                                  ;; 15.9.5.37
   (js-fun this #f #f "Date.setUTCDate"
	   (new-date)
	   (cond
	      ((not (Js-Date? this))
	       (type-error "Date-setUTCDate applied to" this))
	      ((NaN? (Js-Date-ms this))
	       (Js-Date-ms this))
	      (else
	       (with-access::Js-Date this (ms)
		  (let ((date-n (any->number new-date)))
		     (set! ms (time-clip (replace-date ms date-n))))
		  ms)))))

(define (replace-month n m d)
   (cond
      ((not d)
       (replace-month n m (date-from-time n)))
      (else
       (make-js-date (make-js-day (year-from-time n) m d)
		     (time-within-day n)))))

(define (setMonth)                                     ;; 15.9.5.38
   (js-fun this #f (nb-args get-arg)
	   "Date.setMonth"
	   (new-month new-date)
	   (cond
	      ((not (Js-Date? this))
	       (type-error "Date-setMonth applied to" this))
	      ((NaN? (Js-Date-ms this))
	       (Js-Date-ms this))
	      (else
	       (with-access::Js-Date this (ms)
		  (let ((l-t (local-time ms))
			(month-n (any->number new-date))
			(maybe-d (if (>fx nb-args 1)
				     (any->number new-date)
				     #f)))
		     (set! ms (time-clip (UTC (replace-month l-t
							     month-n
							     maybe-d))))
		     ms))))))

(define (setUTCMonth)                                     ;; 15.9.5.39
   (js-fun this #f (nb-args get-arg)
	   "Date.setMonth"
	   (new-month new-date)
	   (cond
	      ((not (Js-Date? this))
	       (type-error "Date-setMonth applied to" this))
	      ((NaN? (Js-Date-ms this))
	       (Js-Date-ms this))
	      (else
	       (with-access::Js-Date this (ms)
		  (let ((month-n (any->number new-date))
			(maybe-d (and (>fx nb-args 1)
				      (any->number new-date))))
		     (set! ms (time-clip (replace-month ms
							month-n
							maybe-d)))
		     ms))))))

(define (replace-year n y m d)
   (cond
      ((not m)
       (replace-year n y 0.0 (day-within-year n)))
      ((not d)
       (replace-year n y m (date-from-time n)))
      (else
       (make-js-date (make-js-day y m d)
		     (time-within-day n)))))

(define (setFullYear)                                 ;; 15.9.5.40
   (js-fun this #f (nb-args get-arg)
	   "Date.setFullYear"
	   (new-year new-month new-date)
	   (cond
	      ((not (Js-Date? this))
	       (type-error "Date-setFullYear applied to" this))
	      (else
	       (with-access::Js-Date this (ms)
		  (let ((l-t (if (NaN? ms)
				 0.0
				 (local-time ms)))
			(year-n (any->number new-date))
			(maybe-m (and (>fx nb-args 1)
				      (any->number new-month)))
			(maybe-d (and (>fx nb-args 2)
				      (any->number new-date))))
		     (set! ms (time-clip (UTC (replace-year l-t
							    year-n
							    maybe-m
							    maybe-d))))
		     ms))))))

(define (setUTCFullYear)                               ;; 15.9.5.41
   (js-fun this #f (nb-args get-arg)
	   "Date.setUTCFullYear"
	   (new-year new-month new-date)
	   (cond
	      ((not (Js-Date? this))
	       (type-error "Date-setUTCFullYear applied to" this))
	      (else
	       (with-access::Js-Date this (ms)
		  (let ((t (if (NaN? ms) 0.0 ms))
			(year-n (any->number new-date))
			(maybe-m (and (>fx nb-args 1)
				      (any->number new-month)))
			(maybe-d (and (>fx nb-args 2)
				      (any->number new-date))))
		     (set! ms (time-clip (replace-year t
						       year-n
						       maybe-m
						       maybe-d)))
		     ms))))))

(define (toUTCString)                                 ;; 15.9.5.42
   (js-fun this #f #f "Date.toUTCString"
	   ()
	   (cond
	      ((not (Js-Date? this))
	       (type-error "Date-toUTCString applied to" this))
	      ((NaN? (Js-Date-ms this))
	       "Invalid Date")
	      (else
	       (time->string (Js-Date-ms this) #t)))))
