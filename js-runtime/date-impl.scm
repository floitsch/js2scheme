(define (modulofl n::double n2::double)
   (let ((t (remainderfl n n2)))
      (if (<fl t 0.0)
	  (+fl t n2)
	  t)))

;; TODO: year-from-time is used quite often, but recalculated each time.

;; 15.9.1.2
(define *ms-per-day* 86400000.0)

(define (day::double t::double)
   (floorfl (/fl t *ms-per-day*)))
(define (time-within-day::double t::double)
   (modulofl t *ms-per-day*))

;; 15.9.1.3
(define (js-leap-year?::bool y::double)
   (cond
      ((not (zerofl? (remainderfl y 4.0)))
       #f)
      ((not (zerofl? (remainderfl y 100.0)))
       #t)
      ((not (zerofl? (remainderfl y 400.0)))
       #f)
      (else
       #t)))
   
(define (days-in-year::double y::double)
   (if (js-leap-year? y)
       366.0
       365.0))

;; first day of year y
(define (day-from-year::double y::double)
;    (+ (* 365 (- y 1970))
;       (/ (- y 1969) 4)
;       (- (/ (- y 1901) 100))
;       (/ (- y 1601) 400))
   (+fl (-fl (+fl (*fl 365.0 (-fl y 1970.0))
		  (floorfl (/fl (-fl y 1969.0) 4.0)))
	     (floorfl (/fl (-fl y 1901.0) 100.0)))
	(floorfl (/fl (-fl y 1601.0) 400.0))))

;; time value at start of year
(define (time-from-year::double y::double)
   (*fl *ms-per-day* (day-from-year y)))

;; a time value determines a year
(define (year-from-time::double t::double)
   (let loop ((y (+fl (truncatefl (/fl t (*fl 365.0 *ms-per-day*)))
		      1970.0))) ;; get estimate. we will improve on that
      (let* ((t-y (time-from-year y))
	     (diff (-fl t-y t))
	     (diff-y (truncatefl (/fl diff (*fl 366.0 *ms-per-day*)))))
	 (cond
	    ((and (zerofl? diff-y)
		  (<=fl t-y t)
		  (<fl t (time-from-year (+fl y 1.0))))
	     y) ;; we got there
	    ((and (zerofl? diff-y)
		  (<=fl t-y t))
	     ;; we are close. but a little bit too low
	     ;; should only happen in leap-years.
	     (loop (+fl y 1.0)))
	    ((zerofl? diff-y)
	     ;; close but too high
	     (loop (-fl y 1.0)))
	    (else
	     ;; diff-y is not 0 -> use the estimate
	     (loop (-fl y diff-y)))))))

(define (in-leap-year?::bool t::double)
   (js-leap-year? (year-from-time t)))
(define (in-leap-year::double t::double)
   (if (in-leap-year? t) 0.0 1.0))

;; 15.9.1.4 and 15.9.1.5
(define (date-decomposition t::double)
   (let* ((year (year-from-time t))
	  (leap (if (js-leap-year? year) 1.0 0.0))
	  (year-day (-fl (day t) (day-from-year year))))
      (cond
	 ((<fl year-day 31.0)
	  (values year 0.0 (+fl year-day 1.0)))
	 ((<fl year-day (+fl 59.0 leap))
	  (values year 1.0 (-fl year-day 30.0)))
	 ((<fl year-day (+fl 90.0 leap))
	  (values year 2.0 (-fl (-fl year-day 58.0) leap)))
	 ((<fl year-day (+fl 120.0 leap))
	  (values year 3.0 (-fl (-fl year-day 89.0) leap)))
	 ((<fl year-day (+fl 151.0 leap))
	  (values year 4.0 (-fl (-fl year-day 119.0) leap)))
	 ((<fl year-day (+fl 181.0 leap))
	  (values year 5.0 (-fl (-fl year-day 150.0) leap)))
	 ((<fl year-day (+fl 212.0 leap))
	  (values year 6.0 (-fl (-fl year-day 180.0) leap)))
	 ((<fl year-day (+fl 243.0 leap))
	  (values year 7.0 (-fl (-fl year-day 211.0) leap)))
	 ((<fl year-day (+fl 273.0 leap))
	  (values year 8.0 (-fl (-fl year-day 242.0) leap)))
	 ((<fl year-day (+fl 304.0 leap))
	  (values year 9.0 (-fl (-fl year-day 272.0) leap)))
	 ((<fl year-day (+fl 334.0 leap))
	  (values year 10.0 (-fl (-fl year-day 303.0) leap)))
	 (else
	  (values year 11.0 (-fl (-fl year-day 333.0) leap))))))
	  
(define (day-within-year::double t::double)
   (-fl (day t) (day-from-year (year-from-time t))))

;; between 0 and 11
(define (month-from-time::double t::double)
   (receive (year month date)
      (date-decomposition t)
      month))

;; day in month. between 1 and 31
(define (date-from-time::double t::double)
   (receive (year month date)
      (date-decomposition t)
      date))

;; 15.9.1.6
;; 0 = Sun, 1 = Mon, ...
(define (week-day::double t::double)
   ;; 4 for thursday, 01 january 1970
   (modulofl (+fl (day t) 4.0) 7.0))

;; 15.9.1.8
;; TODO: caching of TZA is not ok.
;; Bigloo does this during configure. so we can't be better anyways.
(define *localTZA* (*fl -1000.0
			(fixnum->flonum (date-timezone (current-date)))))

;; 15.9.1.9
(define (daylight-saving-TA::double t::double)
   (define (equivalent-year::double y::double)
      (let* ((leap-year? (in-leap-year? y))
	     (wd (week-day (day-from-year y))))
	 (if leap-year?
	     (case (flonum->fixnum wd)
		;; 0 = sunday.
		;; the 'else' clause is just to make bigloo typing happy
		((0) 1984.0) ((1) 1996.0) ((2) 1980.0) ((3) 1992.0)
		((4) 1976.0) ((5) 1988.0) ((6) 1972.0) (else (NaN)))
	     (case (flonum->fixnum wd)
		((0) 1978.0) ((1) 1973.0) ((2) 1974.0) ((3) 1975.0)
		((4) 1981.0) ((5) 1971.0) ((6) 1977.0) (else (NaN))))))

   ;; same as t, but in interval 1970-2038
   (define (get-equivalent-ms t)
      (cond
	 ((and (>=fl t 0.0) ;; year-1970
	       (<fl t 2147385660000.0)) ;; ~ jan 17 2038
	  t)
	 (else
	  (receive (y m d)
	     (date-decomposition t)
	     (let* ((eq-year (equivalent-year y))
		    (js-date (make-js-date (make-js-day eq-year m d)
					   (time-within-day t))))
		js-date)))))

   (if (not (finite? t))
       (NaN)
       (let* ((equiv-ms (get-equivalent-ms t))
	      (secs (flonum->elong (/fl equiv-ms 1000.0)))
	      (bdate (seconds->date secs))
	      (tz-offset (fixnum->flonum (*fx (date-timezone bdate)
					      1000)))) ;; in ms
	  (if (<=fx (date-is-dst bdate) 0) ;; either none or unknown
	      0.0
	      ;; ok. so we know there is a dst. but we don't know how much.
	      (-fl (+fl (make-js-date ;; same date but without daysaving
			 (make-js-day
			  (fixnum->flonum (date-year bdate))
			  (fixnum->flonum (-fx (date-month bdate) 1))
			  (fixnum->flonum (date-day bdate)))
			 (make-js-time
			  (fixnum->flonum (date-hour bdate))
			  (fixnum->flonum (date-minute bdate))
			  (fixnum->flonum (date-second bdate))
			  0.0))
			tz-offset)
		   (*fl (elong->flonum secs) 1000.0))))))


;; 15.9.1.9
;; (UTC (local-time t)) is not necessarily always equal to t.

(define (local-time::double t::double)
   (+fl (+fl t *localTZA*) (daylight-saving-TA t)))

(define (UTC::double t::double)
   (let ((t-local (-fl t *localTZA*)))
      (-fl t-local (daylight-saving-TA t-local))))

   
;; 15.9.1.10
(define *hours-per-day* 24.0)
(define *minutes-per-hour* 60.0)
(define *seconds-per-minute* 60.0)
(define *ms-per-hour* 3600000.0)
(define *ms-per-minute* 60000.0)
(define *ms-per-second* 1000.0)

(define (hours-from-time::double t::double)
   (modulofl (floorfl (/fl t *ms-per-hour*)) *hours-per-day*))
(define (min-from-time::double t::double)
   (modulofl (floorfl (/fl t *ms-per-minute*)) *minutes-per-hour*))
(define (sec-from-time::double t::double)
   (modulofl (floorfl (/fl t *ms-per-second*)) *seconds-per-minute*))
(define (ms-from-time::double t::double)
   (modulofl t *ms-per-second*))

;; 15.9.1.11
(define (finite?::bool n::double)
   (not (or (NaN? n)
	    (+infinity? n)
	    (-infinity? n))))

(define (make-js-time::double hour::double min::double sec::double ms::double)
   (if (not (and (finite? hour) (finite? min) (finite? sec) (finite? ms)))
       (NaN)
       (let ((hour-i (finite->integer hour))
	     (min-i (finite->integer min))
	     (sec-i (finite->integer sec))
	     (ms-i (finite->integer ms)))
	  (+fl (*fl hour-i *ms-per-hour*)
	       (+fl (*fl min-i *ms-per-minute*)
		    (+fl (*fl sec-i *ms-per-second*)
			 ms-i))))))

;; 15.9.1.12
(define (make-js-day::double year::double month::double date::double)
   ;; min-date: 20/04/-271821 (-100.000.000 days)
   ;; max-date: 13/09/275760  (+100.000.000 days)
  
   (define *min-years* -271821.0)
   (define *min-month* 3.0) ;; april -> 3. only used when min-years is equal
   (define *max-years* 275760.0)
   (define *max-month* 8.0) ;; sep -> 8. only used when max-years is equal

   (define (time-from-month::double m::double leap-year?::bool)
      (let* ((leap-year (if leap-year? 1.0 0.0))
	     (nb-days (case m
			((0.0) 0.0)
			((1.0) 31.0)
			((2.0) (+fl 59.0 leap-year))
			((3.0) (+fl 90.0 leap-year))
			((4.0) (+fl 120.0 leap-year))
			((5.0) (+fl 151.0 leap-year))
			((6.0) (+fl 181.0 leap-year))
			((7.0) (+fl 212.0 leap-year))
			((8.0) (+fl 243.0 leap-year))
			((9.0) (+fl 273.0 leap-year))
			((10.0) (+fl 304.0 leap-year))
			((11.0) (+fl 334.0 leap-year)))))
	 (*fl nb-days *ms-per-day*)))

   (if (not (and (finite? year) (finite? month) (finite? date)))
       (NaN)
       (let* ((year-i (finite->integer year))
	      (month-i (finite->integer month))
	      (date-i (finite->integer date))
	      (norm-year (+fl year-i (floorfl (/fl month-i 12.0))))
	      (norm-month (modulofl month-i 12.0)))
	  (if (or (<fl norm-year *min-years*)
		  (and (=fl norm-year *min-years*)
		       (<fl month-i *min-month*))
		  (>fl norm-year *max-years*)
		  (and (=fl norm-year *max-years*)
		       (>fl month-i *max-month*)))
	      (NaN)
	      (let* ((t-y (time-from-year norm-year))
		     (t-m (time-from-month norm-month
					   (js-leap-year? norm-year)))
		     (d (day (+fl t-y t-m))))
		 (-fl (+fl d date-i) 1.0))))))

;; 15.9.1.13
(define (make-js-date::double day::double time::double)
   (if (not (and (finite? day) (finite? time)))
       (NaN)
       (+fl (*fl day *ms-per-day*)
	    time)))

(define (time-clip::double t::double)
   (cond
      ((not (finite? t)) (NaN))
      ((>fl (absfl t) 8.64e15) (NaN))
      (else (finite->integer t))))

;; RFC2822 string.
(define (time->utc-string::bstring t::double)
   (cond
      ((NaN? t)
       "Invalid Date")
      (else
       (format "~a ~a -0000"
	       (time->utc-date-string t)
	       (time->time-string t)))))

(define (zone-name offset::double daylight-saving?)
   (let ((mins (flonum->fixnum (/fl offset *ms-per-minute*))))
      (if (not daylight-saving?)
	  (case mins
	     ((-600) "HAST")
	     ((-540) "AKST")
	     ((-480) "PST")
	     ((-420) "MST")
	     ((-360) "CST")
	     ((-300) "EST")
	     ((-240) "AST")
	     ((-210) "NST")
	     ((0) "WET")
	     ((60) "CET")
	     ((120) "EET")
	     ((180) "MST")
	     ((420) "CXT")
	     ((480) "AWST")
	     ((570) "ACST")
	     ((600) "AEST")
	     ((690) "NFT")
	     (else #f))
	  (case mins
	     ((-600) "HADT")
	     ((-540) "AKDT")
	     ((-480) "PDT")
	     ((-420) "MDT")
	     ((-360) "CDT")
	     ((-300) "EDT")
	     ((-240) "ADT")
	     ((-210) "NDT")
	     ((0) "WEST")
	     ((60) "CEST")
	     ((120) "EEST")
	     ((180) "MSD")
	     ((480) "AWDT")
	     ((570) "ACDT")
	     ((600) "AEDT")
	     (else #f)))))
       
(define (2-digit n)
   (let* ((i (flonum->fixnum n))
	  (s (number->string i)))
      (if (< i 10)
	  (string-append "0" s)
	  s)))

(define (time->local-string::bstring t::double dst::double)
   (cond
      ((NaN? t)
       "Invalid Date")
      (else
       (let* ((offset (+fl dst *localTZA*))
	      (abs-offset (absfl offset))
	      (hour-offset (floorfl (/fl abs-offset *ms-per-hour*)))
	      (min-offset (floorfl (/fl (modulofl abs-offset *ms-per-hour*)
					*ms-per-minute*)))
	      (l-time (+fl t offset)))
	  (format "~a ~a GMT~a~a~a~a"
		  (time->date-string l-time)
		  (time->time-string l-time)
		  (cond
		     ((and (=fl offset 0.0)
			   (=fl *localTZA* 0.0))
		      "+")
		     ((<=fl offset 0.0)
		      "-")
		     (else "+"))
		  (2-digit hour-offset)
		  (2-digit min-offset)
		  (let ((name (zone-name *localTZA* (not (=fl 0.0 dst)))))
		     (if name
			 (string-append " (" name ")")
			 "")))))))

(define (time->date-string::bstring t::double)
   (cond
      ((NaN? t)
       "Invalid Date")
      (else
       (let* ((w (flonum->fixnum (week-day t)))
	      (d (date-from-time t))
	      (m (flonum->fixnum (month-from-time t)))
	      (y (flonum->fixnum (year-from-time t))))
	  (format "~a ~a ~a ~a"
		  (vector-ref '#("Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat") w)
		  (vector-ref '#("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul"
				       "Aug" "Sep" "Oct" "Nov" "Dec")
			      m)
		  (2-digit d)
		  y)))))
(define (time->utc-date-string::bstring t::double)
   (cond
      ((NaN? t)
       "Invalid Date")
      (else
       (let* ((w (flonum->fixnum (week-day t)))
	      (d (date-from-time t))
	      (m (flonum->fixnum (month-from-time t)))
	      (y (flonum->fixnum (year-from-time t))))
	  (format "~a, ~a ~a ~a"
		  (vector-ref '#("Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat") w)
		  (2-digit d)
		  (vector-ref '#("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul"
				       "Aug" "Sep" "Oct" "Nov" "Dec")
			      m)
		  y)))))
(define (time->time-string::bstring t::double)
   (cond
      ((NaN? t)
       "Invalid Date")
      (else
       (format "~a:~a:~a"
	       (2-digit (hours-from-time t))
	       (2-digit (min-from-time t))
	       (2-digit (sec-from-time t))))))

(define-macro (the-digit i)
   `(-fx (byte->fixnum (the-byte-ref ,i)) (char->integer #\0)))

(define (string->time::double s::bstring)
   (define (offset-number->ms-offset::double n::double)
      (let* ((i (flonum->fixnum n))
	     (h (/fx i 100))
	     (m (modulofx i 100)))
	 (*fl (fixnum->flonum (+fx (*fx h 60) m))
	      *ms-per-minute*)))

   (define *date-lalr*
      (lalr-grammar
	 (number month weekday comma colon GMT plus minus info)
	 (date-time
	  ((maybe-weekday maybe-comma date maybe-time zone-info maybe-info)
	   (time-clip (-fl (make-js-date date maybe-time) zone-info)))
	  ((maybe-weekday maybe-comma date maybe-time maybe-info)
	   (time-clip (UTC (make-js-date date maybe-time)))))
	 (maybe-weekday
	  (()        'done)
	  ((weekday) 'done))
	 (maybe-comma
	  (()      'done)
	  ((comma) 'done))
	 (maybe-info
	  (()     'done)
	  ((info) 'done))
	 (date
	  ((number@day month number@year)
	   (make-js-day year month day))
	  ((month number@day number@year)
	   (make-js-day year month day)))
	 (maybe-time
	  (() 0.0)
	  ((time) time))
	 (time
	  ((number@h colon number@m colon number@s colon number@ms)
	   (make-js-time h m s ms))
	  ((number@h colon number@m colon number@s)
	   (make-js-time h m s 0.0))
	  ((number@h colon number@m)
	   (make-js-time h m 0.0 0.0)))
	 (zone-info
	  ((GMT plus number)
	   (offset-number->ms-offset number))
	  ((GMT minus number)
	   (-fl 0.0 (offset-number->ms-offset number)))
	  ((plus number)
	   (offset-number->ms-offset number))
	  ((minus number)
	   (-fl 0.0 (offset-number->ms-offset number))))))

   (define *date-grammar*
      (regular-grammar ((white (in " \r\n\t")))
	 ((+ white) (ignore))
	 ((+ digit) (cons 'number (the-flonum)))
	 ((or "Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun")
	  'weekday)
	 ((: (in "JFMASOND") (in "aepuco") (in "nbrylgptvc"))
	  (cons 'month (case (the-symbol)
			  ((Jan) 0.0)
			  ((Feb) 1.0)
			  ((Mar) 2.0)
			  ((Apr) 3.0)
			  ((May) 4.0)
			  ((Jun) 5.0)
			  ((Jul) 6.0)
			  ((Aug) 7.0)
			  ((Sep) 8.0)
			  ((Oct) 9.0)
			  ((Nov) 10.0)
			  ((Dec) 11.0))))
	 (#\: 'colon)
	 (#\, 'comma)
	 ("GMT" 'GMT)
	 (#\+ 'plus)
	 (#\- 'minus)
	 ((: #\( (* (out #\))) #\))
	  'info)))
	  

   (tprint s)
   (with-handler
      (lambda (e)
	 (tprint e)
	 (NaN))
      (let ((p (open-input-string s)))
	 (unwind-protect
	    (read/lalrp *date-lalr* *date-grammar* p)
	    (close-input-port p)))))
