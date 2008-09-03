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
	      t::double)
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
		       (t (NaN)))))

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
				(UTC-method)
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
    (let ((d (js-new *js-Date*)))
       (js-method-call d "toString"))))

(define (Date-new) ;; 15.9.3
   (js-fun-lambda this #f (nb-args get-arg)
    ()
    (case nb-args
       ((0) ;; 15.9.3.3
	(Js-Date-t-set! this (llong->flonum
			       (/llong (current-microseconds) 1000))))
       ((1) ;; 15.9.3.2
	(let ((prim (any->primitive (get-arg 0) #f)))
	   (if (string? prim)
	       (Js-Date-t-set! this (string->time prim))
	       (let* ((v (any->number prim))
		      (t (time-clip v)))
		  (Js-Date-t-set! this t)))))
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
	       (js-time (make-js-time hours minutes seconds ms))
	       (js-date (make-js-date js-day js-time)))
	   (Js-Date-t-set! this (time-clip (UTC js-date))))))
    this))

(define (Date-construct c)
   (instantiate::Js-Date
      (props (make-props-hashtable))
      (proto *js-Date-prototype*)
      (t (NaN))))

(define (parse) ;; 15.9.4.2
   (js-fun this #f #f "Date.parse"
	   (str)
	   (string->time str)))

(define (UTC-method) ;; 15.9.4.3
   (js-fun this #f (nb-args get-arg)
	   "Date.UTC"
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
	   (js-time (make-js-time hours minutes seconds ms))
	   (js-date (make-js-date js-day js-time)))
       (time-clip js-date))))



(define (toString)                       ;; 15.9.5.2
   (js-fun this #f #f "Date.toString"
	   ()
	   (cond
	      ((not (Js-Date? this))
	       (type-error "Date-toString applied to" this))
	      ((NaN? (Js-Date-t this))
	       "Invalid Date")
	      (else
	       (time->string (Js-Date-t this) #f)))))

(define (toDateString)                   ;; 15.9.5.3
   (js-fun this #f #f "Date.toDateString"
	   ()
	   (cond
	      ((not (Js-Date? this))
	       (type-error "Date-toDateString applied to" this))
	      ((NaN? (Js-Date-t this))
	       "Invalid Date")
	      (else
	       (time->date-string (Js-Date-t this))))))

(define (toTimeString)                   ;; 15.9.5.4
   (js-fun this #f #f "Date.toTimeString"
	   ()
	   (cond
	      ((not (Js-Date? this))
	       (type-error "Date-toTimeString applied to" this))
	      ((NaN? (Js-Date-t this))
	       "Invalid Date")
	      (else
	       (time->time-string (Js-Date-t this))))))

(define (toLocaleString)                       ;; 15.9.5.5
   ;; TODO: adapt for locale strings.
   (js-fun this #f #f "Date.toLocaleString"
	   ()
	   (cond
	      ((not (Js-Date? this))
	       (type-error "Date-toLocaleString applied to" this))
	      ((NaN? (Js-Date-t this))
	       "Invalid Date")
	      (else
	       (time->string (Js-Date-t this) #f)))))

(define (toLocaleDateString)                   ;; 15.9.5.6
   ;; TODO: adapt for locale strings.
   (js-fun this #f #f "Date.toLocaleDateString"
	   ()
	   (cond
	      ((not (Js-Date? this))
	       (type-error "Date-toLocaleDateString applied to" this))
	      ((NaN? (Js-Date-t this))
	       "Invalid Date")
	      (else
	       (time->date-string (Js-Date-t this))))))

(define (toLocaleTimeString)                   ;; 15.9.5.7
   ;; TODO: adapt for locale strings.
   (js-fun this #f #f "Date.toLocaleTimeString"
	   ()
	   (cond
	      ((not (Js-Date? this))
	       (type-error "Date-toLocaleTimeString applied to" this))
	      ((NaN? (Js-Date-t this))
	       "Invalid Date")
	      (else
	       (time->time-string (Js-Date-t this))))))

(define (valueOf)                              ;; 15.9.5.8
   (js-fun this #f #f "Date.valueOf"
	   ()
	   (cond
	      ((not (Js-Date? this))
	       (type-error "Date-valueOf applied to" this))
	      (else
	       (Js-Date-t this)))))

(define (getTime)                              ;; 15.9.5.9
   (js-fun this #f #f "Date.getTime"
	   ()
	   (cond
	      ((not (Js-Date? this))
	       (type-error "Date-getTime applied to" this))
	      (else
	       (Js-Date-t this)))))

(define (getFullYear)                          ;; 15.9.5.10
   (js-fun this #f #f "Date.getFullYear"
	   ()
	   (cond
	      ((not (Js-Date? this))
	       (type-error "Date-getFullYear applied to" this))
	      ((NaN? (Js-Date-t this))
	       (NaN))
	      (else
	       (year-from-time (local-time (Js-Date-t this)))))))

(define (getUTCFullYear)                       ;; 15.9.5.11
   (js-fun this #f #f "Date.getFullYear"
	   ()
	   (cond
	      ((not (Js-Date? this))
	       (type-error "Date-getUTCFullYear applied to" this))
	      ((NaN? (Js-Date-t this))
	       (NaN))
	      (else
	       (year-from-time (Js-Date-t this))))))
   
(define (getMonth)                             ;; 15.9.5.12
   (js-fun this #f #f "Date.getMonth"
	   ()
	   (cond
	      ((not (Js-Date? this))
	       (type-error "Date-getMonth applied to" this))
	      ((NaN? (Js-Date-t this))
	       (NaN))
	      (else
	       (month-from-time (local-time (Js-Date-t this)))))))

(define (getUTCMonth)                          ;; 15.9.5.13
   (js-fun this #f #f "Date.getUTCMonth"
	   ()
	   (cond
	      ((not (Js-Date? this))
	       (type-error "Date-getUTCMonth applied to" this))
	      ((NaN? (Js-Date-t this))
	       (NaN))
	      (else
	       (month-from-time (Js-Date-t this))))))

(define (getDate)                              ;; 15.9.5.14
   (js-fun this #f #f "Date.getDate"
	   ()
	   (cond
	      ((not (Js-Date? this))
	       (type-error "Date-getDate applied to" this))
	      ((NaN? (Js-Date-t this))
	       (NaN))
	      (else
	       (date-from-time (local-time (Js-Date-t this)))))))

(define (getUTCDate)                           ;; 15.9.5.15
   (js-fun this #f #f "Date.getUTCDate"
	   ()
	   (cond
	      ((not (Js-Date? this))
	       (type-error "Date-getUTCDate applied to" this))
	      ((NaN? (Js-Date-t this))
	       (NaN))
	      (else
	       (date-from-time (Js-Date-t this))))))

(define (getDay)                              ;; 15.9.5.16
   (js-fun this #f #f "Date.getDay"
	   ()
	   (cond
	      ((not (Js-Date? this))
	       (type-error "Date-getDay applied to" this))
	      ((NaN? (Js-Date-t this))
	       (NaN))
	      (else
	       (week-day (local-time (Js-Date-t this)))))))

(define (getUTCDay)                           ;; 15.9.5.17
   (js-fun this #f #f "Date.getUTCDay"
	   ()
	   (cond
	      ((not (Js-Date? this))
	       (type-error "Date-getUTCDay applied to" this))
	      ((NaN? (Js-Date-t this))
	       (NaN))
	      (else
	       (week-day (Js-Date-t this))))))

(define (getHours)                             ;; 15.9.5.18
   (js-fun this #f #f "Date.getHours"
	   ()
	   (cond
	      ((not (Js-Date? this))
	       (type-error "Date-getHours applied to" this))
	      ((NaN? (Js-Date-t this))
	       (NaN))
	      (else
	       (hours-from-time (local-time (Js-Date-t this)))))))

(define (getUTCHours)                          ;; 15.9.5.19
   (js-fun this #f #f "Date.getUTCHours"
	   ()
	   (cond
	      ((not (Js-Date? this))
	       (type-error "Date-getUTCHours applied to" this))
	      ((NaN? (Js-Date-t this))
	       (NaN))
	      (else
	       (hours-from-time (Js-Date-t this))))))

(define (getMinutes)                           ;; 15.9.5.20
   (js-fun this #f #f "Date.getMinutes"
	   ()
	   (cond
	      ((not (Js-Date? this))
	       (type-error "Date-getMinutes applied to" this))
	      ((NaN? (Js-Date-t this))
	       (NaN))
	      (else
	       (min-from-time (local-time (Js-Date-t this)))))))

(define (getUTCMinutes)                        ;; 15.9.5.21
   (js-fun this #f #f "Date.getUTCMinutes"
	   ()
	   (cond
	      ((not (Js-Date? this))
	       (type-error "Date-getUTCMinutes applied to" this))
	      ((NaN? (Js-Date-t this))
	       (NaN))
	      (else
		(min-from-time (Js-Date-t this))))))

(define (getSeconds)                           ;; 15.9.5.22
   (js-fun this #f #f "Date.getSeconds"
	   ()
	   (cond
	      ((not (Js-Date? this))
	       (type-error "Date-getSeconds applied to" this))
	      ((NaN? (Js-Date-t this))
	       (NaN))
	      (else
	       (sec-from-time (local-time (Js-Date-t this)))))))

(define (getUTCSeconds)                        ;; 15.9.5.23
   (js-fun this #f #f "Date.getUTCSeconds"
	   ()
	   (cond
	      ((not (Js-Date? this))
	       (type-error "Date-getUTCSeconds applied to" this))
	      ((NaN? (Js-Date-t this))
	       (NaN))
	      (else
	       (sec-from-time (Js-Date-t this))))))

(define (getMilliseconds)                       ;; 15.9.5.24
   (js-fun this #f #f "Date.getMilliseconds"
	   ()
	   (cond
	      ((not (Js-Date? this))
	       (type-error "Date-getMilliseconds applied to" this))
	      ((NaN? (Js-Date-t this))
	       (NaN))
	      (else
	       (ms-from-time (local-time (Js-Date-t this)))))))

(define (getUTCMilliseconds)                       ;; 15.9.5.25
   (js-fun this #f #f "Date.getUTCMilliseconds"
	   ()
	   (cond
	      ((not (Js-Date? this))
	       (type-error "Date-getUTCMilliseconds applied to" this))
	      ((NaN? (Js-Date-t this))
	       (NaN))
	      (else
	       (ms-from-time (Js-Date-t this))))))

(define (getTimezoneOffset)                        ;; 15.9.5.26
   (js-fun this #f #f "Date.getTimezoneOffset"
	   ()
	   (cond
	      ((not (Js-Date? this))
	       (type-error "Date-getTimezoneOffset applied to" this))
	      ((NaN? (Js-Date-t this))
	       (NaN))
	      (else
	       (let ((t (Js-Date-t this))
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
	       (with-access::Js-Date this (t)
		  (set! t (time-clip (any->number t)))
		  t)))))

(define (update-time t maybe-h maybe-m maybe-s maybe-ms)
   (if (NaN? t)
       t
       (let ((h (or maybe-h (hours-from-time t)))
	     (m (or maybe-m (min-from-time t)))
	     (s (or maybe-s (sec-from-time t)))
	     (ms (or maybe-ms (ms-from-time t))))
	  (make-js-date (day t) (make-js-time h m s ms)))))

(define (update-UTC-time t h m s ms)
   (time-clip (update-time t h m s ms)))

(define (update-local-time t h m s ms)
   (time-clip (UTC (update-time (local-time t) h m s ms))))
      
(define (setMilliseconds)                          ;; 15.9.5.28
   (js-fun this #f #f "Date.setMilliseconds"
	   (ms)
	   (cond
	      ((not (Js-Date? this))
	       (type-error "Date-setMilliseconds applied to" this))
	      (else
	       ;; UTC and local-time are ridiculous, but if the
	       ;; date is completely at the limit then it could make a
	       ;; difference...
	       (with-access::Js-Date this (t)
		  (set! t (update-local-time t #f #f #f (any->number ms)))
		  t)))))

(define (setUTCMilliseconds)                        ;; 15.9.5.29
   (js-fun this #f #f "Date.setUTCMilliseconds"
	   (ms)
	   (cond
	      ((not (Js-Date? this))
	       (type-error "Date-setUTCMilliseconds applied to" this))
	      (else
	       (with-access::Js-Date this (t)
		  (set! t (update-UTC-time t #f #f #f (any->number ms)))
		  t)))))


(define (setSeconds)                                  ;; 15.9.5.30
   (js-fun this #f (nb-args get-arg)
	   "Date.setSeconds"
	   (s ms)
	   (cond
	      ((not (Js-Date? this))
	       (type-error "Date-setSeconds applied to" this))
	      (else
	       ;; UTC and local-time are ridiculous here, but if the
	       ;; date is completely at the limit then it could make a
	       ;; difference...
	       (with-access::Js-Date this (t)
		  (set! t (update-local-time t #f #f
					     (any->number s)
					     (and (>=fx nb-args 2)
						  (any->number ms))))
		  t)))))

(define (setUTCSeconds)                               ;; 15.9.5.31
   (js-fun this #f (nb-args get-arg)
	   "Date.setUTCSeconds"
	   (s ms)
	   (cond
	      ((not (Js-Date? this))
	       (type-error "Date-setUTCSeconds applied to" this))
	      (else
	       (with-access::Js-Date this (t)
		  (set! t (update-UTC-time t #f #f
					   (any->number s)
					   (and (>=fx nb-args 2)
						(any->number ms))))
		  t)))))

;; Note: ECMAScript spec does not contain any 15.9.5.32 (MS Word...)

(define (setMinutes)                                  ;; 15.9.5.33
   (js-fun this #f (nb-args get-arg)
	   "Date.setMinutes"
	   (m s ms)
	   (cond
	      ((not (Js-Date? this))
	       (type-error "Date-setMinutes applied to" this))
	      (else
	       (with-access::Js-Date this (t)
		  (set! t (update-local-time t #f
					     (any->number m)
					     (and (>=fx nb-args 2)
						  (any->number s))
					     (and (>=fx nb-args 3)
						  (any->number ms))))
		  t)))))

(define (setUTCMinutes)                               ;; 15.9.5.34
   (js-fun this #f (nb-args get-arg)
	   "Date.setUTCMinutes"
	   (m s ms)
	   (cond
	      ((not (Js-Date? this))
	       (type-error "Date-setUTCMinutes applied to" this))
	      (else
	       (with-access::Js-Date this (t)
		  (set! t (update-UTC-time t #f
					     (any->number m)
					     (and (>=fx nb-args 2)
						  (any->number s))
					     (and (>=fx nb-args 3)
						  (any->number ms))))
		  t)))))

(define (setHours)                                    ;; 15.9.5.35
   (js-fun this #f (nb-args get-arg)
	   "Date.setHours"
	   (h m s ms)
	   (cond
	      ((not (Js-Date? this))
	       (type-error "Date-setHours applied to" this))
	      (else
	       (with-access::Js-Date this (t)
		  (set! t (update-local-time t
					     (any->number h)
					     (and (>=fx nb-args 2)
						  (any->number m))
					     (and (>=fx nb-args 3)
						  (any->number s))
					     (and (>=fx nb-args 4)
						  (any->number ms))))
		  t)))))

(define (setUTCHours)                                 ;; 15.9.5.36
   (js-fun this #f (nb-args get-arg)
	   "Date.setUTCHours"
	   (h m s ms)
	   (cond
	      ((not (Js-Date? this))
	       (type-error "Date-setUTCHours applied to" this))
	      (else
	       (with-access::Js-Date this (t)
		  (set! t (update-UTC-time t
					   (any->number h)
					   (and (>=fx nb-args 2)
						(any->number m))
					   (and (>=fx nb-args 3)
						(any->number s))
					   (and (>=fx nb-args 4)
						(any->number ms))))
		  t)))))

(define (update-date t y m d)
   (cond
      ((NaN? t)
       t)
      ((and y m d)
       (make-js-date (make-js-day y m d) (time-within-day t)))
      (else
       (receive (old-y old-m old-d)
	  (date-decomposition t)
	  (make-js-date (make-js-day (or y old-y)
				     (or m old-m)
				     (or d old-d))
			(time-within-day t))))))

(define (update-UTC-date t y m d)
   (time-clip (update-date t y m d)))
(define (update-local-date t y m d)
   (time-clip (UTC (update-date (local-time t) y m d))))

;; Note: there are two 15.9.5.36 subsections...

(define (setDate)                                     ;; 15.9.5.36
   (js-fun this #f #f "Date.setDate"
	   (d)
	   (cond
	      ((not (Js-Date? this))
	       (type-error "Date-setDate applied to" this))
	      (else
	       (with-access::Js-Date this (t)
		  (set! t (update-local-date t #f #f (any->number d)))
		  t)))))

(define (setUTCDate)                                  ;; 15.9.5.37
   (js-fun this #f #f "Date.setUTCDate"
	   (d)
	   (cond
	      ((not (Js-Date? this))
	       (type-error "Date-setUTCDate applied to" this))
	      (else
	       (with-access::Js-Date this (t)
		  (set! t (update-UTC-date t #f #f (any->number d)))
		  t)))))

(define (setMonth)                                     ;; 15.9.5.38
   (js-fun this #f (nb-args get-arg)
	   "Date.setMonth"
	   (m d)
	   (cond
	      ((not (Js-Date? this))
	       (type-error "Date-setMonth applied to" this))
	      (else
	       (with-access::Js-Date this (t)
		  (set! t (update-local-date t #f
					     (any->number m)
					     (and (>= nb-args 2)
						  (any->number d))))
		  t)))))

(define (setUTCMonth)                                     ;; 15.9.5.39
   (js-fun this #f (nb-args get-arg)
	   "Date.setUTCMonth"
	   (m d)
	   (cond
	      ((not (Js-Date? this))
	       (type-error "Date-setMonth applied to" this))
	      (else
	       (with-access::Js-Date this (t)
		  (set! t (update-UTC-date t #f
					   (any->number m)
					   (and (>= nb-args 2)
						(any->number d))))
		  t)))))

(define (setFullYear)                                 ;; 15.9.5.40
   (js-fun this #f (nb-args get-arg)
	   "Date.setFullYear"
	   (y m d)
	   (cond
	      ((not (Js-Date? this))
	       (type-error "Date-setFullYear applied to" this))
	      (else
	       (with-access::Js-Date this (t)
		  (set! t (update-local-date (if (NaN? t) 0.0 t)
					     (any->number y)
					     (and (>= nb-args 2)
						  (any->number m))
					     (and (>= nb-args 3)
						  (any->number d))))
		  t)))))

(define (setUTCFullYear)                               ;; 15.9.5.41
   (js-fun this #f (nb-args get-arg)
	   "Date.setUTCFullYear"
	   (y m d)
	   (cond
	      ((not (Js-Date? this))
	       (type-error "Date-setUTCFullYear applied to" this))
	      (else
	       (with-access::Js-Date this (t)
		  (set! t (update-UTC-date (if (NaN? t) 0.0 t)
					   (any->number y)
					   (and (>= nb-args 2)
						(any->number m))
					   (and (>= nb-args 3)
						(any->number d))))
		  t)))))

(define (toUTCString)                                 ;; 15.9.5.42
   (js-fun this #f #f "Date.toUTCString"
	   ()
	   (cond
	      ((not (Js-Date? this))
	       (type-error "Date-toUTCString applied to" this))
	      ((NaN? (Js-Date-t this))
	       "Invalid Date")
	      (else
	       (time->string (Js-Date-t this) #t)))))
