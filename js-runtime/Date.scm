(module jsre-Date
   (import jsre-base-object
	   jsre-base-string)
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
	   (final-class NatO-Date::Js-Object
	      t::double
	      (cached-t::double (default 0.0))
	      (dst::double (default 0.0)))
	   (Date-init)))

(define *jsg-Date* #unspecified)
(define *js-Date-prototype*::Js-Object (js-null))

(define-method (js-class-name::js-string o::NatO-Date)
   (STR "Date"))

(define (Date-init)
   (let* ((date-fun (Date-lambda))
	  (text-repr (STR "function(v) {/*native Date*/ throw 'native'; }"))
	  (date-object (create-function-object date-fun
					       (Date-new)
					       Date-construct
					       text-repr))
	  (prototype (instantiate::NatO-Date    ;; 15.9.5
			(props (make-props-hashtable))
			(proto (natO-object-prototype))
			(t +nan.0))))

      (set! *jsg-Date* (create-runtime-global (STR "Date") date-fun))
      (set! *js-Date-prototype* prototype)

      (js-property-generic-set! date-object       ;; 15.9.4
				(STR "length")
				7.0
				(length-attributes))
      (js-property-generic-set! date-object       ;; 15.9.4.1
				(STR "prototype")
				prototype
				(get-Attributes dont-enum dont-delete
						read-only))

      (js-property-generic-set! date-object       ;; 15.9.4.2
				(STR "parse")
				(parse)
				(built-in-attributes))
      (js-property-generic-set! date-object       ;; 15.9.4.3
				(STR "UTC")
				(UTC-method)
				(built-in-attributes))
      

      (js-property-generic-set! prototype         ;; 15.9.5.1
				(STR "constructor")
				date-fun
				(constructor-attributes))
      (js-property-generic-set! prototype         ;; 15.9.5.2
				(STR "toString")
				(toString)
				(built-in-attributes))
      (js-property-generic-set! prototype         ;; 15.9.5.3
				(STR "toDateString")
				(toDateString)
				(built-in-attributes))
      (js-property-generic-set! prototype         ;; 15.9.5.4
				(STR "toTimeString")
				(toTimeString)
				(built-in-attributes))
      (js-property-generic-set! prototype         ;; 15.9.5.5
				(STR "toLocaleString")
				(toLocaleString)
				(built-in-attributes))
      (js-property-generic-set! prototype         ;; 15.9.5.6
				(STR "toLocaleDateString")
				(toLocaleDateString)
				(built-in-attributes))
      (js-property-generic-set! prototype         ;; 15.9.5.7
				(STR "toLocaleTimeString")
				(toLocaleTimeString)
				(built-in-attributes))
      (js-property-generic-set! prototype         ;; 15.9.5.8
				(STR "valueOf")
				(valueOf)
				(built-in-attributes))
      (js-property-generic-set! prototype         ;; 15.9.5.9
				(STR "getTime")
				(getTime)
				(built-in-attributes))
      (js-property-generic-set! prototype         ;; 15.9.5.10
				(STR "getFullYear")
				(getFullYear)
				(built-in-attributes))
      (js-property-generic-set! prototype         ;; B.2.4
				(STR "getYear")
				(getYear)
				(built-in-attributes))
      (js-property-generic-set! prototype         ;; 15.9.5.11
				(STR "getUTCFullYear")
				(getUTCFullYear)
				(built-in-attributes))
      (js-property-generic-set! prototype         ;; 15.9.5.12
				(STR "getMonth")
				(getMonth)
				(built-in-attributes))
      (js-property-generic-set! prototype         ;; 15.9.5.13
				(STR "getUTCMonth")
				(getUTCMonth)
				(built-in-attributes))
      (js-property-generic-set! prototype         ;; 15.9.5.14
				(STR "getDate")
				(getDate)
				(built-in-attributes))
      (js-property-generic-set! prototype         ;; 15.9.5.15
				(STR "getUTCDate")
				(getUTCDate)
				(built-in-attributes))
      (js-property-generic-set! prototype         ;; 15.9.5.16
				(STR "getDay")
				(getDay)
				(built-in-attributes))
      (js-property-generic-set! prototype         ;; 15.9.5.17
				(STR "getUTCDay")
				(getUTCDay)
				(built-in-attributes))
      (js-property-generic-set! prototype         ;; 15.9.5.18
				(STR "getHours")
				(getHours)
				(built-in-attributes))
      (js-property-generic-set! prototype         ;; 15.9.5.19
				(STR "getUTCHours")
				(getUTCHours)
				(built-in-attributes))
      (js-property-generic-set! prototype         ;; 15.9.5.20
				(STR "getMinutes")
				(getMinutes)
				(built-in-attributes))
      (js-property-generic-set! prototype         ;; 15.9.5.21
				(STR "getUTCMinutes")
				(getUTCMinutes)
				(built-in-attributes))
      (js-property-generic-set! prototype         ;; 15.9.5.22
				(STR "getSeconds")
				(getSeconds)
				(built-in-attributes))
      (js-property-generic-set! prototype         ;; 15.9.5.23
				(STR "getUTCSeconds")
				(getUTCSeconds)
				(built-in-attributes))
      (js-property-generic-set! prototype         ;; 15.9.5.24
				(STR "getMilliseconds")
				(getMilliseconds)
				(built-in-attributes))
      (js-property-generic-set! prototype         ;; 15.9.5.25
				(STR "getUTCMilliseconds")
				(getUTCMilliseconds)
				(built-in-attributes))
      (js-property-generic-set! prototype         ;; 15.9.5.26
				(STR "getTimezoneOffset")
				(getTimezoneOffset)
				(built-in-attributes))
      (js-property-generic-set! prototype         ;; 15.9.5.27
				(STR "setTime")
				(setTime)
				(built-in-attributes))
      (js-property-generic-set! prototype         ;; 15.9.5.28
				(STR "setMilliseconds")
				(setMilliseconds)
				(built-in-attributes))
      (js-property-generic-set! prototype         ;; 15.9.5.29
				(STR "setUTCMilliseconds")
				(setUTCMilliseconds)
				(built-in-attributes))
      (js-property-generic-set! prototype         ;; 15.9.5.30
				(STR "setSeconds")
				(setSeconds)
				(built-in-attributes))
      (js-property-generic-set! prototype         ;; 15.9.5.31
				(STR "setUTCSeconds")
				(setUTCSeconds)
				(built-in-attributes))
      (js-property-generic-set! prototype         ;; 15.9.5.33
				(STR "setMinutes")
				(setMinutes)
				(built-in-attributes))
      (js-property-generic-set! prototype         ;; 15.9.5.34
				(STR "setUTCMinutes")
				(setUTCMinutes)
				(built-in-attributes))
      (js-property-generic-set! prototype         ;; 15.9.5.35
				(STR "setHours")
				(setHours)
				(built-in-attributes))
      (js-property-generic-set! prototype         ;; 15.9.5.36
				(STR "setUTCHours")
				(setUTCHours)
				(built-in-attributes))
      (js-property-generic-set! prototype         ;; 15.9.5.36
				(STR "setDate")
				(setDate)
				(built-in-attributes))
      (js-property-generic-set! prototype         ;; 15.9.5.37
				(STR "setUTCDate")
				(setUTCDate)
				(built-in-attributes))
      (js-property-generic-set! prototype         ;; 15.9.5.38
				(STR "setMonth")
				(setMonth)
				(built-in-attributes))
      (js-property-generic-set! prototype         ;; 15.9.5.39
				(STR "setUTCMonth")
				(setUTCMonth)
				(built-in-attributes))
      (js-property-generic-set! prototype         ;; 15.9.5.40
				(STR "setFullYear")
				(setFullYear)
				(built-in-attributes))
      (js-property-generic-set! prototype         ;; B.2.5
				(STR "setYear")
				(setYear)
				(built-in-attributes))
      (js-property-generic-set! prototype         ;; 15.9.5.41
				(STR "setUTCFullYear")
				(setUTCFullYear)
				(built-in-attributes))
      (let ((utc-string (toUTCString)))
	 (js-property-generic-set! prototype         ;; 15.9.5.42
				   (STR "toUTCString")
				   utc-string
				   (built-in-attributes))
	 (js-property-generic-set! prototype         ;; B.2.6
				   (STR "toGMTString")
				   utc-string
				   (built-in-attributes)))
      ))

(define (Date-lambda) ;; 15.9.2.1
   (js-fun-lambda #f #f #f
    ()
    (let ((d (js-new (global-read *jsg-Date*))))
       (js-method-call d (STR "toString")))))

(define (Date-new) ;; 15.9.3
   (js-fun-lambda this #f (nb-args get-arg)
    ()
    (case nb-args
       ((0) ;; 15.9.3.3
	(NatO-Date-t-set! this (llong->flonum
			       (/llong (current-microseconds) 1000))))
       ((1) ;; 15.9.3.2
	(let ((prim (any->primitive (get-arg 0) #f)))
	   (if (js-string? prim)
	       (NatO-Date-t-set! this (js-string->time prim))
	       (let* ((v (any->number prim))
		      (t (time-clip v)))
		  (NatO-Date-t-set! this t)))))
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
	   (NatO-Date-t-set! this (time-clip (UTC js-date))))))
    this))

(define (Date-construct c)
   (instantiate::NatO-Date
      (props (make-props-hashtable))
      (proto *js-Date-prototype*)
      (t +nan.0)))

;; CARE: cache is clearly not thread-safe...
(define (local-info-update! d)
   (with-access::NatO-Date d (t cached-t dst)
      (cond
	 ((=fl t cached-t)
	  #t) ;; already set.
	 (else
	  (set! dst (daylight-saving-TA t))
	  (set! cached-t t)))))
	 

;; same as 'local-time', but uses cached info, and directly takes NatO-Date
(define (Js-local-time::double d::NatO-Date)
   (local-info-update! d)
   (with-access::NatO-Date d (t dst)
      (+fl (+fl t *localTZA*) dst)))

(define (parse) ;; 15.9.4.2
   (js-fun this #f #f (STR "Date.parse")
	   (str)
	   (js-string->time (any->js-string str))))

(define (UTC-method) ;; 15.9.4.3
   (js-fun this #f (nb-args get-arg)
	   (STR "Date.UTC")
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
   (js-fun this #f #f (STR "Date.prototype.toString")
	   ()
	   (cond
	      ((not (NatO-Date? this))
	       (type-error (STR "Date-toString applied to") this))
	      ((nanfl? (NatO-Date-t this))
	       (STR "Invalid Date"))
	      (else
	       (local-info-update! this)
	       (with-access::NatO-Date this (t dst)
		  (time->local-string t dst))))))

(define (toDateString)                   ;; 15.9.5.3
   (js-fun this #f #f (STR "Date.prototype.toDateString")
	   ()
	   (cond
	      ((not (NatO-Date? this))
	       (type-error (STR "Date-toDateString applied to") this))
	      ((nanfl? (NatO-Date-t this))
	       (STR "Invalid Date"))
	      (else
	       (time->date-string (Js-local-time this))))))

(define (toTimeString)                   ;; 15.9.5.4
   (js-fun this #f #f "Date.prototype.toTimeString"
	   ()
	   (cond
	      ((not (NatO-Date? this))
	       (type-error (STR "Date-toTimeString applied to") this))
	      ((nanfl? (NatO-Date-t this))
	       (STR "Invalid Date"))
	      (else
	       (time->time-string (Js-local-time this))))))

(define (toLocaleString)                       ;; 15.9.5.5
   ;; TODO: adapt for locale strings.
   (js-fun this #f #f (STR "Date.prototype.toLocaleString")
	   ()
	   (cond
	      ((not (NatO-Date? this))
	       (type-error (STR "Date-toLocaleString applied to") this))
	      ((nanfl? (NatO-Date-t this))
	       (STR "Invalid Date"))
	      (else
	       (local-info-update! this)
	       (with-access::NatO-Date this (t dst)
		  (time->local-string t dst))))))

(define (toLocaleDateString)                   ;; 15.9.5.6
   ;; TODO: adapt for locale strings.
   (js-fun this #f #f (STR "Date.prototype.toLocaleDateString")
	   ()
	   (cond
	      ((not (NatO-Date? this))
	       (type-error (STR "Date-toLocaleDateString applied to") this))
	      ((nanfl? (NatO-Date-t this))
	       (STR "Invalid Date"))
	      (else
	       (time->date-string (Js-local-time this))))))

(define (toLocaleTimeString)                   ;; 15.9.5.7
   ;; TODO: adapt for locale strings.
   (js-fun this #f #f (STR "Date.prototype.toLocaleTimeString")
	   ()
	   (cond
	      ((not (NatO-Date? this))
	       (type-error (STR "Date-toLocaleTimeString applied to") this))
	      ((nanfl? (NatO-Date-t this))
	       (STR "Invalid Date"))
	      (else
	       (time->time-string (Js-local-time this))))))

(define (valueOf)                              ;; 15.9.5.8
   (js-fun this #f #f (STR "Date.prototype.valueOf")
	   ()
	   (cond
	      ((not (NatO-Date? this))
	       (type-error (STR "Date-valueOf applied to") this))
	      (else
	       (NatO-Date-t this)))))

(define (getTime)                              ;; 15.9.5.9
   (js-fun this #f #f (STR "Date.prototype.getTime")
	   ()
	   (cond
	      ((not (NatO-Date? this))
	       (type-error (STR "Date-getTime applied to") this))
	      (else
	       (NatO-Date-t this)))))

(define (getFullYear)                          ;; 15.9.5.10
   (js-fun this #f #f (STR "Date.prototype.getFullYear")
	   ()
	   (cond
	      ((not (NatO-Date? this))
	       (type-error (STR "Date-getFullYear applied to") this))
	      ((nanfl? (NatO-Date-t this))
	       +nan.0)
	      (else
	       (year-from-time (Js-local-time this))))))

(define (getYear)                              ;; B.2.4
   (js-fun this #f #f (STR "Date.prototype.getYear")
	   ()
	   (cond
	      ((not (NatO-Date? this))
	       (type-error (STR "Date-getYear applied to") this))
	      ((nanfl? (NatO-Date-t this))
	       +nan.0)
	      (else
	       (-fl (year-from-time (Js-local-time this))
		    1900.)))))

(define (getUTCFullYear)                       ;; 15.9.5.11
   (js-fun this #f #f (STR "Date.prototype.getFullYear")
	   ()
	   (cond
	      ((not (NatO-Date? this))
	       (type-error (STR "Date-getUTCFullYear applied to") this))
	      ((nanfl? (NatO-Date-t this))
	       +nan.0)
	      (else
	       (year-from-time (NatO-Date-t this))))))
   
(define (getMonth)                             ;; 15.9.5.12
   (js-fun this #f #f (STR "Date.prototype.getMonth")
	   ()
	   (cond
	      ((not (NatO-Date? this))
	       (type-error (STR "Date-getMonth applied to") this))
	      ((nanfl? (NatO-Date-t this))
	       +nan.0)
	      (else
	       (month-from-time (Js-local-time this))))))

(define (getUTCMonth)                          ;; 15.9.5.13
   (js-fun this #f #f (STR "Date.prototype.getUTCMonth")
	   ()
	   (cond
	      ((not (NatO-Date? this))
	       (type-error (STR "Date-getUTCMonth applied to") this))
	      ((nanfl? (NatO-Date-t this))
	       +nan.0)
	      (else
	       (month-from-time (NatO-Date-t this))))))

(define (getDate)                              ;; 15.9.5.14
   (js-fun this #f #f (STR "Date.prototype.getDate")
	   ()
	   (cond
	      ((not (NatO-Date? this))
	       (type-error (STR "Date-getDate applied to") this))
	      ((nanfl? (NatO-Date-t this))
	       +nan.0)
	      (else
	       (date-from-time (Js-local-time this))))))

(define (getUTCDate)                           ;; 15.9.5.15
   (js-fun this #f #f (STR "Date.prototype.getUTCDate")
	   ()
	   (cond
	      ((not (NatO-Date? this))
	       (type-error (STR "Date-getUTCDate applied to") this))
	      ((nanfl? (NatO-Date-t this))
	       +nan.0)
	      (else
	       (date-from-time (NatO-Date-t this))))))

(define (getDay)                              ;; 15.9.5.16
   (js-fun this #f #f (STR "Date.prototype.getDay")
	   ()
	   (cond
	      ((not (NatO-Date? this))
	       (type-error (STR "Date-getDay applied to") this))
	      ((nanfl? (NatO-Date-t this))
	       +nan.0)
	      (else
	       (week-day (Js-local-time this))))))

(define (getUTCDay)                           ;; 15.9.5.17
   (js-fun this #f #f (STR "Date.prototype.getUTCDay")
	   ()
	   (cond
	      ((not (NatO-Date? this))
	       (type-error (STR "Date-getUTCDay applied to") this))
	      ((nanfl? (NatO-Date-t this))
	       +nan.0)
	      (else
	       (week-day (NatO-Date-t this))))))

(define (getHours)                             ;; 15.9.5.18
   (js-fun this #f #f (STR "Date.prototype.getHours")
	   ()
	   (cond
	      ((not (NatO-Date? this))
	       (type-error (STR "Date-getHours applied to") this))
	      ((nanfl? (NatO-Date-t this))
	       +nan.0)
	      (else
	       (hours-from-time (Js-local-time this))))))

(define (getUTCHours)                          ;; 15.9.5.19
   (js-fun this #f #f (STR "Date.prototype.getUTCHours")
	   ()
	   (cond
	      ((not (NatO-Date? this))
	       (type-error (STR "Date-getUTCHours applied to") this))
	      ((nanfl? (NatO-Date-t this))
	       +nan.0)
	      (else
	       (hours-from-time (NatO-Date-t this))))))

(define (getMinutes)                           ;; 15.9.5.20
   (js-fun this #f #f (STR "Date.prototype.getMinutes")
	   ()
	   (cond
	      ((not (NatO-Date? this))
	       (type-error (STR "Date-getMinutes applied to") this))
	      ((nanfl? (NatO-Date-t this))
	       +nan.0)
	      (else
	       (min-from-time (Js-local-time this))))))

(define (getUTCMinutes)                        ;; 15.9.5.21
   (js-fun this #f #f (STR "Date.prototype.getUTCMinutes")
	   ()
	   (cond
	      ((not (NatO-Date? this))
	       (type-error (STR "Date-getUTCMinutes applied to") this))
	      ((nanfl? (NatO-Date-t this))
	       +nan.0)
	      (else
		(min-from-time (NatO-Date-t this))))))

(define (getSeconds)                           ;; 15.9.5.22
   (js-fun this #f #f (STR "Date.prototype.getSeconds")
	   ()
	   (cond
	      ((not (NatO-Date? this))
	       (type-error (STR "Date-getSeconds applied to") this))
	      ((nanfl? (NatO-Date-t this))
	       +nan.0)
	      (else
	       (sec-from-time (Js-local-time this))))))

(define (getUTCSeconds)                        ;; 15.9.5.23
   (js-fun this #f #f (STR "Date.prototype.getUTCSeconds")
	   ()
	   (cond
	      ((not (NatO-Date? this))
	       (type-error (STR "Date-getUTCSeconds applied to") this))
	      ((nanfl? (NatO-Date-t this))
	       +nan.0)
	      (else
	       (sec-from-time (NatO-Date-t this))))))

(define (getMilliseconds)                       ;; 15.9.5.24
   (js-fun this #f #f (STR "Date.prototype.getMilliseconds")
	   ()
	   (cond
	      ((not (NatO-Date? this))
	       (type-error (STR "Date-getMilliseconds applied to") this))
	      ((nanfl? (NatO-Date-t this))
	       +nan.0)
	      (else
	       (ms-from-time (Js-local-time this))))))

(define (getUTCMilliseconds)                       ;; 15.9.5.25
   (js-fun this #f #f (STR "Date.prototype.getUTCMilliseconds")
	   ()
	   (cond
	      ((not (NatO-Date? this))
	       (type-error (STR "Date-getUTCMilliseconds applied to") this))
	      ((nanfl? (NatO-Date-t this))
	       +nan.0)
	      (else
	       (ms-from-time (NatO-Date-t this))))))

(define (getTimezoneOffset)                        ;; 15.9.5.26
   (js-fun this #f #f (STR "Date.prototype.getTimezoneOffset")
	   ()
	   (cond
	      ((not (NatO-Date? this))
	       (type-error (STR "Date-getTimezoneOffset applied to") this))
	      ((nanfl? (NatO-Date-t this))
	       +nan.0)
	      (else
	       (let ((t (NatO-Date-t this))
		     (ms-per-minute 60000.0))
		  (/fl (-fl t (Js-local-time this))
		       ms-per-minute))))))

(define (setTime)                                  ;; 15.9.5.27
   (js-fun this #f #f (STR "Date.prototype.setTime")
	   (new-t)
	   (cond
	      ((not (NatO-Date? this))
	       (type-error (STR "Date-setTime applied to") this))
	      (else
	       (with-access::NatO-Date this (t)
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

(define (update-UTC-time! d::NatO-Date h m s ms)
   (with-access::NatO-Date d (t)
      (set! t (time-clip (update-time t h m s ms)))
      t))

(define (update-local-time! d::NatO-Date h m s ms)
   (with-access::NatO-Date d (t)
      (set! t (time-clip (UTC (update-time (Js-local-time d) h m s ms))))
      t))
      
(define (setMilliseconds)                          ;; 15.9.5.28
   (js-fun this #f #f (STR "Date.prototype.setMilliseconds")
	   (ms)
	   (cond
	      ((not (NatO-Date? this))
	       (type-error (STR "Date-setMilliseconds applied to") this))
	      (else
	       ;; UTC and local-time are ridiculous, but if the
	       ;; date is completely at the limit of floating point number
	       ;; then it could make a difference...
	       (update-local-time! this #f #f #f (any->number ms))))))

(define (setUTCMilliseconds)                        ;; 15.9.5.29
   (js-fun this #f #f (STR "Date.prototype.setUTCMilliseconds")
	   (ms)
	   (cond
	      ((not (NatO-Date? this))
	       (type-error (STR "Date-setUTCMilliseconds applied to") this))
	      (else
	       (update-UTC-time! this #f #f #f (any->number ms))))))


(define (setSeconds)                                  ;; 15.9.5.30
   (js-fun this #f (nb-args get-arg)
	   (STR "Date.prototype.setSeconds")
	   (s ms)
	   (cond
	      ((not (NatO-Date? this))
	       (type-error (STR "Date-setSeconds applied to") this))
	      (else
	       ;; UTC and local-time are ridiculous here, but if the
	       ;; date is completely at the limit then it could make a
	       ;; difference...
	       (update-local-time! this #f #f
				   (any->number s)
				   (and (>=fx nb-args 2) (any->number ms)))))))

(define (setUTCSeconds)                               ;; 15.9.5.31
   (js-fun this #f (nb-args get-arg)
	   (STR "Date.prototype.setUTCSeconds")
	   (s ms)
	   (cond
	      ((not (NatO-Date? this))
	       (type-error (STR "Date-setUTCSeconds applied to") this))
	      (else
	       (update-UTC-time! this #f #f
				 (any->number s)
				 (and (>=fx nb-args 2) (any->number ms)))))))

;; Note: ECMAScript spec does not contain any 15.9.5.32 (MS Word...)

(define (setMinutes)                                  ;; 15.9.5.33
   (js-fun this #f (nb-args get-arg)
	   (STR "Date.prototype.setMinutes")
	   (m s ms)
	   (cond
	      ((not (NatO-Date? this))
	       (type-error (STR "Date-setMinutes applied to") this))
	      (else
	       (update-local-time! this #f
				   (any->number m)
				   (and (>=fx nb-args 2) (any->number s))
				   (and (>=fx nb-args 3) (any->number ms)))))))

(define (setUTCMinutes)                               ;; 15.9.5.34
   (js-fun this #f (nb-args get-arg)
	   (STR "Date.prototype.setUTCMinutes")
	   (m s ms)
	   (cond
	      ((not (NatO-Date? this))
	       (type-error (STR "Date-setUTCMinutes applied to") this))
	      (else
	       (update-UTC-time! this #f
				 (any->number m)
				 (and (>=fx nb-args 2) (any->number s))
				 (and (>=fx nb-args 3) (any->number ms)))))))

(define (setHours)                                    ;; 15.9.5.35
   (js-fun this #f (nb-args get-arg)
	   (STR "Date.prototype.setHours")
	   (h m s ms)
	   (cond
	      ((not (NatO-Date? this))
	       (type-error (STR "Date-setHours applied to") this))
	      (else
	       (update-local-time! this
				   (any->number h)
				   (and (>=fx nb-args 2) (any->number m))
				   (and (>=fx nb-args 3) (any->number s))
				   (and (>=fx nb-args 4) (any->number ms)))))))

(define (setUTCHours)                                 ;; 15.9.5.36
   (js-fun this #f (nb-args get-arg)
	   (STR "Date.prototype.setUTCHours")
	   (h m s ms)
	   (cond
	      ((not (NatO-Date? this))
	       (type-error (STR "Date-setUTCHours applied to") this))
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

(define (update-UTC-date! jdate::NatO-Date y m d)
   (with-access::NatO-Date jdate (t)
      (set! t (time-clip (update-date t y m d)))
      t))
(define (update-local-date! jdate::NatO-Date y m d)
   (with-access::NatO-Date jdate (t)
      (set! t (time-clip (UTC (update-date (Js-local-time jdate) y m d))))
      t))

;; Note: there are two 15.9.5.36 subsections...

(define (setDate)                                     ;; 15.9.5.36
   (js-fun this #f #f (STR "Date.prototype.setDate")
	   (d)
	   (cond
	      ((not (NatO-Date? this))
	       (type-error (STR "Date-setDate applied to") this))
	      (else
	       (update-local-date! this #f #f (any->number d))))))

(define (setUTCDate)                                  ;; 15.9.5.37
   (js-fun this #f #f (STR "Date.prototype.setUTCDate")
	   (d)
	   (cond
	      ((not (NatO-Date? this))
	       (type-error (STR "Date-setUTCDate applied to") this))
	      (else
	       (update-UTC-date! this #f #f (any->number d))))))

(define (setMonth)                                     ;; 15.9.5.38
   (js-fun this #f (nb-args get-arg)
	   (STR "Date.prototype.setMonth")
	   (m d)
	   (cond
	      ((not (NatO-Date? this))
	       (type-error (STR "Date-setMonth applied to") this))
	      (else
	       (update-local-date! this #f
				   (any->number m)
				   (and (>= nb-args 2) (any->number d)))))))

(define (setUTCMonth)                                     ;; 15.9.5.39
   (js-fun this #f (nb-args get-arg)
	   (STR "Date.prototype.setUTCMonth")
	   (m d)
	   (cond
	      ((not (NatO-Date? this))
	       (type-error (STR "Date-setMonth applied to") this))
	      (else
	       (update-UTC-date! this #f
				 (any->number m)
				 (and (>= nb-args 2) (any->number d)))))))

(define (setFullYear)                                 ;; 15.9.5.40
   (js-fun this #f (nb-args get-arg)
	   (STR "Date.prototype.setFullYear")
	   (y m d)
	   (cond
	      ((not (NatO-Date? this))
	       (type-error (STR "Date-setFullYear applied to") this))
	      (else
	       (with-access::NatO-Date this (t)
		  (if (nanfl? t) (set! t 0.0)))
	       (update-local-date! this
				   (any->number y)
				   (and (>= nb-args 2) (any->number m))
				   (and (>= nb-args 3) (any->number d)))))))

(define (setYear)                                 ;; 15.9.5.40
   (js-fun this #f (nb-args get-arg)
	   (STR "Date.prototype.setYear")
	   (year)
	   (cond
	      ((not (NatO-Date? this))
	       (type-error (STR "Date-setYear applied to") this))
	      (else
	       (with-access::NatO-Date this (t)
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
	   (STR "Date.prototype.setUTCFullYear")
	   (y m d)
	   (cond
	      ((not (NatO-Date? this))
	       (type-error (STR "Date-setUTCFullYear applied to") this))
	      (else
	       (with-access::NatO-Date this (t)
		  (if (nanfl? t) (set! t 0.0)))
	       (update-UTC-date! this
				 (any->number y)
				 (and (>= nb-args 2) (any->number m))
				 (and (>= nb-args 3) (any->number d)))))))

(define (toUTCString)                                 ;; 15.9.5.42
   (js-fun this #f #f (STR "Date.prototype.toUTCString")
	   ()
	   (cond
	      ((not (NatO-Date? this))
	       (type-error (STR "Date-toUTCString applied to") this))
	      ((nanfl? (NatO-Date-t this))
	       (STR "Invalid Date"))
	      (else
	       (with-access::NatO-Date this (t)
		  (time->utc-js-string t))))))
