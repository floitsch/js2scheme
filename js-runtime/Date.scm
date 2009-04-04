(module jsre-Date
   (import jsre-base-object)
   (use jsre-natives
	jsre-Error
	jsre-Object
	jsre-Function
	jsre-String
	jsre-Number
	jsre-Bool
	jsre-primitives
	jsre-conversion
	jsre-global-object
	jsre-scope-object
	)
   (include "date-impl.scm")
   (export *jsg-Date*
	   (class Js-Date::Js-Object
	      t::double
	      (cached-t::double (default 0.0))
	      (dst::double (default 0.0)))
	   (Date-init)))

(define *jsg-Date* #unspecified)
(define *js-Date-prototype*::Js-Object (js-null))

(define-method (js-class-name::bstring o::Js-Date)
   "Date")

(define (Date-init)
   (let* ((date-fun (Date-lambda))
	  (date-object (create-function-object date-fun
					       (Date-new)
					       Date-construct
					       "TODO [native]"))
	  (prototype (instantiate::Js-Date    ;; 15.9.5
			(props (make-props-hashtable))
			(proto (js-object-prototype))
			(t +nan.0))))

      (set! *jsg-Date* (create-runtime-global "Date" date-fun))
      (set! *js-Date-prototype* prototype)

      (js-property-generic-set! date-object       ;; 15.9.4
				"length"
				7.0
				(length-attributes))
      (js-property-generic-set! date-object       ;; 15.9.4.1
				"prototype"
				prototype
				(get-Attributes dont-enum dont-delete
						read-only))

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
				date-fun
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
      (js-property-generic-set! prototype         ;; B.2.4
				"getYear"
				(getYear)
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
      (js-property-generic-set! prototype         ;; B.2.5
				"setYear"
				(setYear)
				(built-in-attributes))
      (js-property-generic-set! prototype         ;; 15.9.5.41
				"setUTCFullYear"
				(setUTCFullYear)
				(built-in-attributes))
      (let ((utc-string (toUTCString)))
	 (js-property-generic-set! prototype         ;; 15.9.5.42
				   "toUTCString"
				   utc-string
				   (built-in-attributes))
	 (js-property-generic-set! prototype         ;; B.2.6
				   "toGMTString"
				   utc-string
				   (built-in-attributes)))
      ))

(define (Date-lambda) ;; 15.9.2.1
   (js-fun-lambda #f #f #f
    ()
    (let ((d (js-new (global-read *jsg-Date*))))
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
	       (normalized-year
		(if (not (finitefl? year))
		    year
		    (let ((int-year (finite->integer year)))
		       (if (and (<=fl 0.0 int-year)
				(<=fl int-year 99.0))
			   (+fl 1900.0 year)
			   year))))
	       (js-day (make-js-day normalized-year month date))
	       (js-time (make-js-time hours minutes seconds ms))
	       (js-date (make-js-date js-day js-time)))
	   (Js-Date-t-set! this (time-clip (UTC js-date))))))
    this))

(define (Date-construct c)
   (instantiate::Js-Date
      (props (make-props-hashtable))
      (proto *js-Date-prototype*)
      (t +nan.0)))

;; CARE: cache is clearly not thread-safe...
(define (local-info-update! d)
   (with-access::Js-Date d (t cached-t dst)
      (cond
	 ((=fl t cached-t)
	  #t) ;; already set.
	 (else
	  (set! dst (daylight-saving-TA t))
	  (set! cached-t t)))))
	 

;; same as 'local-time', but uses cached info, and directly takes Js-Date
(define (Js-local-time::double d::Js-Date)
   (local-info-update! d)
   (with-access::Js-Date d (t dst)
      (+fl (+fl t *localTZA*) dst)))

(define (parse) ;; 15.9.4.2
   (js-fun this #f #f "Date.parse"
	   (str)
	   (string->time (any->string str))))

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
	   (normalized-year
	    (if (not (finitefl? year))
		year
		(let ((int-year (finite->integer year)))
		   (if (and (<=fl 0.0 int-year)
			    (<=fl int-year 99.0))
		       (+fl 1900.0 year)
		       year))))
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
	      ((nanfl? (Js-Date-t this))
	       "Invalid Date")
	      (else
	       (local-info-update! this)
	       (with-access::Js-Date this (t dst)
		  (time->local-string t dst))))))

(define (toDateString)                   ;; 15.9.5.3
   (js-fun this #f #f "Date.toDateString"
	   ()
	   (cond
	      ((not (Js-Date? this))
	       (type-error "Date-toDateString applied to" this))
	      ((nanfl? (Js-Date-t this))
	       "Invalid Date")
	      (else
	       (time->date-string (Js-local-time this))))))

(define (toTimeString)                   ;; 15.9.5.4
   (js-fun this #f #f "Date.toTimeString"
	   ()
	   (cond
	      ((not (Js-Date? this))
	       (type-error "Date-toTimeString applied to" this))
	      ((nanfl? (Js-Date-t this))
	       "Invalid Date")
	      (else
	       (time->time-string (Js-local-time this))))))

(define (toLocaleString)                       ;; 15.9.5.5
   ;; TODO: adapt for locale strings.
   (js-fun this #f #f "Date.toLocaleString"
	   ()
	   (cond
	      ((not (Js-Date? this))
	       (type-error "Date-toLocaleString applied to" this))
	      ((nanfl? (Js-Date-t this))
	       "Invalid Date")
	      (else
	       (local-info-update! this)
	       (with-access::Js-Date this (t dst)
		  (time->local-string t dst))))))

(define (toLocaleDateString)                   ;; 15.9.5.6
   ;; TODO: adapt for locale strings.
   (js-fun this #f #f "Date.toLocaleDateString"
	   ()
	   (cond
	      ((not (Js-Date? this))
	       (type-error "Date-toLocaleDateString applied to" this))
	      ((nanfl? (Js-Date-t this))
	       "Invalid Date")
	      (else
	       (time->date-string (Js-local-time this))))))

(define (toLocaleTimeString)                   ;; 15.9.5.7
   ;; TODO: adapt for locale strings.
   (js-fun this #f #f "Date.toLocaleTimeString"
	   ()
	   (cond
	      ((not (Js-Date? this))
	       (type-error "Date-toLocaleTimeString applied to" this))
	      ((nanfl? (Js-Date-t this))
	       "Invalid Date")
	      (else
	       (time->time-string (Js-local-time this))))))

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
	      ((nanfl? (Js-Date-t this))
	       +nan.0)
	      (else
	       (year-from-time (Js-local-time this))))))

(define (getYear)                              ;; B.2.4
   (js-fun this #f #f "Date.getYear"
	   ()
	   (cond
	      ((not (Js-Date? this))
	       (type-error "Date-getYear applied to" this))
	      ((nanfl? (Js-Date-t this))
	       +nan.0)
	      (else
	       (-fl (year-from-time (Js-local-time this))
		    1900.)))))

(define (getUTCFullYear)                       ;; 15.9.5.11
   (js-fun this #f #f "Date.getFullYear"
	   ()
	   (cond
	      ((not (Js-Date? this))
	       (type-error "Date-getUTCFullYear applied to" this))
	      ((nanfl? (Js-Date-t this))
	       +nan.0)
	      (else
	       (year-from-time (Js-Date-t this))))))
   
(define (getMonth)                             ;; 15.9.5.12
   (js-fun this #f #f "Date.getMonth"
	   ()
	   (cond
	      ((not (Js-Date? this))
	       (type-error "Date-getMonth applied to" this))
	      ((nanfl? (Js-Date-t this))
	       +nan.0)
	      (else
	       (month-from-time (Js-local-time this))))))

(define (getUTCMonth)                          ;; 15.9.5.13
   (js-fun this #f #f "Date.getUTCMonth"
	   ()
	   (cond
	      ((not (Js-Date? this))
	       (type-error "Date-getUTCMonth applied to" this))
	      ((nanfl? (Js-Date-t this))
	       +nan.0)
	      (else
	       (month-from-time (Js-Date-t this))))))

(define (getDate)                              ;; 15.9.5.14
   (js-fun this #f #f "Date.getDate"
	   ()
	   (cond
	      ((not (Js-Date? this))
	       (type-error "Date-getDate applied to" this))
	      ((nanfl? (Js-Date-t this))
	       +nan.0)
	      (else
	       (date-from-time (Js-local-time this))))))

(define (getUTCDate)                           ;; 15.9.5.15
   (js-fun this #f #f "Date.getUTCDate"
	   ()
	   (cond
	      ((not (Js-Date? this))
	       (type-error "Date-getUTCDate applied to" this))
	      ((nanfl? (Js-Date-t this))
	       +nan.0)
	      (else
	       (date-from-time (Js-Date-t this))))))

(define (getDay)                              ;; 15.9.5.16
   (js-fun this #f #f "Date.getDay"
	   ()
	   (cond
	      ((not (Js-Date? this))
	       (type-error "Date-getDay applied to" this))
	      ((nanfl? (Js-Date-t this))
	       +nan.0)
	      (else
	       (week-day (Js-local-time this))))))

(define (getUTCDay)                           ;; 15.9.5.17
   (js-fun this #f #f "Date.getUTCDay"
	   ()
	   (cond
	      ((not (Js-Date? this))
	       (type-error "Date-getUTCDay applied to" this))
	      ((nanfl? (Js-Date-t this))
	       +nan.0)
	      (else
	       (week-day (Js-Date-t this))))))

(define (getHours)                             ;; 15.9.5.18
   (js-fun this #f #f "Date.getHours"
	   ()
	   (cond
	      ((not (Js-Date? this))
	       (type-error "Date-getHours applied to" this))
	      ((nanfl? (Js-Date-t this))
	       +nan.0)
	      (else
	       (hours-from-time (Js-local-time this))))))

(define (getUTCHours)                          ;; 15.9.5.19
   (js-fun this #f #f "Date.getUTCHours"
	   ()
	   (cond
	      ((not (Js-Date? this))
	       (type-error "Date-getUTCHours applied to" this))
	      ((nanfl? (Js-Date-t this))
	       +nan.0)
	      (else
	       (hours-from-time (Js-Date-t this))))))

(define (getMinutes)                           ;; 15.9.5.20
   (js-fun this #f #f "Date.getMinutes"
	   ()
	   (cond
	      ((not (Js-Date? this))
	       (type-error "Date-getMinutes applied to" this))
	      ((nanfl? (Js-Date-t this))
	       +nan.0)
	      (else
	       (min-from-time (Js-local-time this))))))

(define (getUTCMinutes)                        ;; 15.9.5.21
   (js-fun this #f #f "Date.getUTCMinutes"
	   ()
	   (cond
	      ((not (Js-Date? this))
	       (type-error "Date-getUTCMinutes applied to" this))
	      ((nanfl? (Js-Date-t this))
	       +nan.0)
	      (else
		(min-from-time (Js-Date-t this))))))

(define (getSeconds)                           ;; 15.9.5.22
   (js-fun this #f #f "Date.getSeconds"
	   ()
	   (cond
	      ((not (Js-Date? this))
	       (type-error "Date-getSeconds applied to" this))
	      ((nanfl? (Js-Date-t this))
	       +nan.0)
	      (else
	       (sec-from-time (Js-local-time this))))))

(define (getUTCSeconds)                        ;; 15.9.5.23
   (js-fun this #f #f "Date.getUTCSeconds"
	   ()
	   (cond
	      ((not (Js-Date? this))
	       (type-error "Date-getUTCSeconds applied to" this))
	      ((nanfl? (Js-Date-t this))
	       +nan.0)
	      (else
	       (sec-from-time (Js-Date-t this))))))

(define (getMilliseconds)                       ;; 15.9.5.24
   (js-fun this #f #f "Date.getMilliseconds"
	   ()
	   (cond
	      ((not (Js-Date? this))
	       (type-error "Date-getMilliseconds applied to" this))
	      ((nanfl? (Js-Date-t this))
	       +nan.0)
	      (else
	       (ms-from-time (Js-local-time this))))))

(define (getUTCMilliseconds)                       ;; 15.9.5.25
   (js-fun this #f #f "Date.getUTCMilliseconds"
	   ()
	   (cond
	      ((not (Js-Date? this))
	       (type-error "Date-getUTCMilliseconds applied to" this))
	      ((nanfl? (Js-Date-t this))
	       +nan.0)
	      (else
	       (ms-from-time (Js-Date-t this))))))

(define (getTimezoneOffset)                        ;; 15.9.5.26
   (js-fun this #f #f "Date.getTimezoneOffset"
	   ()
	   (cond
	      ((not (Js-Date? this))
	       (type-error "Date-getTimezoneOffset applied to" this))
	      ((nanfl? (Js-Date-t this))
	       +nan.0)
	      (else
	       (let ((t (Js-Date-t this))
		     (ms-per-minute 60000.0))
		  (/fl (-fl t (Js-local-time this))
		       ms-per-minute))))))

(define (setTime)                                  ;; 15.9.5.27
   (js-fun this #f #f "Date.setTime"
	   (new-t)
	   (cond
	      ((not (Js-Date? this))
	       (type-error "Date-setTime applied to" this))
	      (else
	       (with-access::Js-Date this (t)
		  (set! t (time-clip (any->number new-t)))
		  t)))))

(define (update-time t maybe-h maybe-m maybe-s maybe-ms)
   (if (nanfl? t)
       t
       (let ((h (or maybe-h (hours-from-time t)))
	     (m (or maybe-m (min-from-time t)))
	     (s (or maybe-s (sec-from-time t)))
	     (ms (or maybe-ms (ms-from-time t))))
	  (make-js-date (day t) (make-js-time h m s ms)))))

(define (update-UTC-time! d::Js-Date h m s ms)
   (with-access::Js-Date d (t)
      (set! t (time-clip (update-time t h m s ms)))
      t))

(define (update-local-time! d::Js-Date h m s ms)
   (with-access::Js-Date d (t)
      (set! t (time-clip (UTC (update-time (Js-local-time d) h m s ms))))
      t))
      
(define (setMilliseconds)                          ;; 15.9.5.28
   (js-fun this #f #f "Date.setMilliseconds"
	   (ms)
	   (cond
	      ((not (Js-Date? this))
	       (type-error "Date-setMilliseconds applied to" this))
	      (else
	       ;; UTC and local-time are ridiculous, but if the
	       ;; date is completely at the limit of floating point number
	       ;; then it could make a difference...
	       (update-local-time! this #f #f #f (any->number ms))))))

(define (setUTCMilliseconds)                        ;; 15.9.5.29
   (js-fun this #f #f "Date.setUTCMilliseconds"
	   (ms)
	   (cond
	      ((not (Js-Date? this))
	       (type-error "Date-setUTCMilliseconds applied to" this))
	      (else
	       (update-UTC-time! this #f #f #f (any->number ms))))))


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
	       (update-local-time! this #f #f
				   (any->number s)
				   (and (>=fx nb-args 2) (any->number ms)))))))

(define (setUTCSeconds)                               ;; 15.9.5.31
   (js-fun this #f (nb-args get-arg)
	   "Date.setUTCSeconds"
	   (s ms)
	   (cond
	      ((not (Js-Date? this))
	       (type-error "Date-setUTCSeconds applied to" this))
	      (else
	       (update-UTC-time! this #f #f
				 (any->number s)
				 (and (>=fx nb-args 2) (any->number ms)))))))

;; Note: ECMAScript spec does not contain any 15.9.5.32 (MS Word...)

(define (setMinutes)                                  ;; 15.9.5.33
   (js-fun this #f (nb-args get-arg)
	   "Date.setMinutes"
	   (m s ms)
	   (cond
	      ((not (Js-Date? this))
	       (type-error "Date-setMinutes applied to" this))
	      (else
	       (update-local-time! this #f
				   (any->number m)
				   (and (>=fx nb-args 2) (any->number s))
				   (and (>=fx nb-args 3) (any->number ms)))))))

(define (setUTCMinutes)                               ;; 15.9.5.34
   (js-fun this #f (nb-args get-arg)
	   "Date.setUTCMinutes"
	   (m s ms)
	   (cond
	      ((not (Js-Date? this))
	       (type-error "Date-setUTCMinutes applied to" this))
	      (else
	       (update-UTC-time! this #f
				 (any->number m)
				 (and (>=fx nb-args 2) (any->number s))
				 (and (>=fx nb-args 3) (any->number ms)))))))

(define (setHours)                                    ;; 15.9.5.35
   (js-fun this #f (nb-args get-arg)
	   "Date.setHours"
	   (h m s ms)
	   (cond
	      ((not (Js-Date? this))
	       (type-error "Date-setHours applied to" this))
	      (else
	       (update-local-time! this
				   (any->number h)
				   (and (>=fx nb-args 2) (any->number m))
				   (and (>=fx nb-args 3) (any->number s))
				   (and (>=fx nb-args 4) (any->number ms)))))))

(define (setUTCHours)                                 ;; 15.9.5.36
   (js-fun this #f (nb-args get-arg)
	   "Date.setUTCHours"
	   (h m s ms)
	   (cond
	      ((not (Js-Date? this))
	       (type-error "Date-setUTCHours applied to" this))
	      (else
	       (update-UTC-time! this
				 (any->number h)
				 (and (>=fx nb-args 2) (any->number m))
				 (and (>=fx nb-args 3) (any->number s))
				 (and (>=fx nb-args 4) (any->number ms)))))))

(define (update-date t y m d)
   (cond
      ((nanfl? t)
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

(define (update-UTC-date! jdate::Js-Date y m d)
   (with-access::Js-Date jdate (t)
      (set! t (time-clip (update-date t y m d)))
      t))
(define (update-local-date! jdate::Js-Date y m d)
   (with-access::Js-Date jdate (t)
      (set! t (time-clip (UTC (update-date (Js-local-time jdate) y m d))))
      t))

;; Note: there are two 15.9.5.36 subsections...

(define (setDate)                                     ;; 15.9.5.36
   (js-fun this #f #f "Date.setDate"
	   (d)
	   (cond
	      ((not (Js-Date? this))
	       (type-error "Date-setDate applied to" this))
	      (else
	       (update-local-date! this #f #f (any->number d))))))

(define (setUTCDate)                                  ;; 15.9.5.37
   (js-fun this #f #f "Date.setUTCDate"
	   (d)
	   (cond
	      ((not (Js-Date? this))
	       (type-error "Date-setUTCDate applied to" this))
	      (else
	       (update-UTC-date! this #f #f (any->number d))))))

(define (setMonth)                                     ;; 15.9.5.38
   (js-fun this #f (nb-args get-arg)
	   "Date.setMonth"
	   (m d)
	   (cond
	      ((not (Js-Date? this))
	       (type-error "Date-setMonth applied to" this))
	      (else
	       (update-local-date! this #f
				   (any->number m)
				   (and (>= nb-args 2) (any->number d)))))))

(define (setUTCMonth)                                     ;; 15.9.5.39
   (js-fun this #f (nb-args get-arg)
	   "Date.setUTCMonth"
	   (m d)
	   (cond
	      ((not (Js-Date? this))
	       (type-error "Date-setMonth applied to" this))
	      (else
	       (update-UTC-date! this #f
				 (any->number m)
				 (and (>= nb-args 2) (any->number d)))))))

(define (setFullYear)                                 ;; 15.9.5.40
   (js-fun this #f (nb-args get-arg)
	   "Date.setFullYear"
	   (y m d)
	   (cond
	      ((not (Js-Date? this))
	       (type-error "Date-setFullYear applied to" this))
	      (else
	       (with-access::Js-Date this (t)
		  (if (nanfl? t) (set! t 0.0)))
	       (update-local-date! this
				   (any->number y)
				   (and (>= nb-args 2) (any->number m))
				   (and (>= nb-args 3) (any->number d)))))))

(define (setYear)                                 ;; 15.9.5.40
   (js-fun this #f (nb-args get-arg)
	   "Date.setYear"
	   (year)
	   (cond
	      ((not (Js-Date? this))
	       (type-error "Date-setYear applied to" this))
	      (else
	       (with-access::Js-Date this (t)
		  (if (nanfl? t) (set! t 0.0)))
	       (let* ((y (any->number year))
		      (yint (any->integer y)))
		  (if (and (finitefl? y)
			   (>=fl yint 0.0)
			   (<=fl yint 99.0))
		      (update-local-date! this (+fl 1900.0 yint) #f #f)
		      (update-local-date! this y #f #f)))))))

(define (setUTCFullYear)                               ;; 15.9.5.41
   (js-fun this #f (nb-args get-arg)
	   "Date.setUTCFullYear"
	   (y m d)
	   (cond
	      ((not (Js-Date? this))
	       (type-error "Date-setUTCFullYear applied to" this))
	      (else
	       (with-access::Js-Date this (t)
		  (if (nanfl? t) (set! t 0.0)))
	       (update-UTC-date! this
				 (any->number y)
				 (and (>= nb-args 2) (any->number m))
				 (and (>= nb-args 3) (any->number d)))))))

(define (toUTCString)                                 ;; 15.9.5.42
   (js-fun this #f #f "Date.toUTCString"
	   ()
	   (cond
	      ((not (Js-Date? this))
	       (type-error "Date-toUTCString applied to" this))
	      ((nanfl? (Js-Date-t this))
	       "Invalid Date")
	      (else
	       (with-access::Js-Date this (t)
		  (time->utc-string t))))))
