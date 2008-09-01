(module jsre-date-impl
   )

;; TODO: year-from-time is used quite often, but recalculated each time.
;; TODO: we might want to switch to llongs for Date-class.

;; 15.9.1.2
(define *ms-per-day* #l86400000) ;; TODO llong
(define (day::llong ms::llong)
   (/llong ms *ms-per-day*))
(define (time-within-day::llong ms::llong)
   (remainderllong ms *ms-per-day*))

;; 15.9.1.3
(define (days-in-year::bint y::bint)
   (cond
      ((not (zerofx? (modulo y 4)))
       365)
      ((not (zerofx? (modulo y 100)))
       366)
      ((not (zerofx? (modulo y 400)))
       365)
      (else
       366)))

;; first day of year y
(define (day-from-year::llong y::bint)
;    (+ (* 365 (- y 1970))
;       (/ (- y 1969) 4)
;       (- (/ (- y 1901) 100))
;       (/ (- y 1601) 400))
   (+llong (-llong (+llong (*llong #l365 (fixnum->llong(-fx y 1970)))
			   (fixnum->llong (/fx (-fx y 1969) 4)))
		   (fixnum->llong (/fx (-fx y 1901) 100)))
	   (fixnum->llong (/fx (-fx y 1601) 400))))

;; time value at start of year
(define (time-from-year::llong y::bint)
   (*llong *ms-per-day* (fixnum->llong (day-from-year y))))

;; a time value determines a year
(define (year-from-time::bint t::llong)
   (let loop ((y (+fx (llong->fixnum (/llong t (*llong 365 *ms-per-day*)))
		      1970))) ;; get estimate. we will improve on that
      (let* ((t-y (time-from-year y))
	     (diff (-llong t-y t))
	     (diff-y (llong->fixnum (/llong diff (*llong 365 *ms-per-day*)))))
	 (cond
	    ((and (zerofx? diff-y)
		  (<=llong t-y t)
		  (<llong (time-from-year (+llong y #l1)) t))
	     y) ;; we got there
	    ((and (zerofx? diff-y)
		  (<=llong t-y t))
	     ;; we are close. but a little bit too low
	     (loop (+fx y 1)))
	    ((zerollong? diff-y)
	     ;; close but too high
	     (loop (-fx y 1)))
	    (else
	     ;; diff-y is not 0 -> use the estimate
	     (loop (-fx y diff-y)))))))

(define (in-leap-year?::bool t::llong)
   (=llong (days-in-year (year-from-time t)) #l365))
(define (in-leap-year::bint t::llong)
   (if (in-leap-year? t) 0 1))

;; 15.9.1.4
(define (day-within-year::bint t::llong)
   (llong->fixnum (-llong (day t) (day-from-year (year-from-time t)))))

(define (month-from-time::bint t::llong)
   (let ((day (day-within-year t))
	 (leap-year (in-leap-year t)))
      (cond
	 ((<fx day 31) 0)
	 ((<fx day (+fx 59 leap-year)) 1)
	 ((<fx day (+fx 90 leap-year)) 2)
	 ((<fx day (+fx 120 leap-year)) 3)
	 ((<fx day (+fx 151 leap-year)) 4)
	 ((<fx day (+fx 181 leap-year)) 5)
	 ((<fx day (+fx 212 leap-year)) 6)
	 ((<fx day (+fx 243 leap-year)) 7)
	 ((<fx day (+fx 273 leap-year)) 8)
	 ((<fx day (+fx 304 leap-year)) 9)
	 ((<fx day (+fx 334 leap-year)) 10)
	 (else 11))))

;; 15.9.1.5
;; day in month. between 1 and 31
(define (date-from-time::bint t::llong)
   (let ((day (day-within-year t))
	 (month (month-from-time t))
	 (leap-year (in-leap-year t)))
      (case month
	 ((0)  (+fx day 1))
	 ((1)  (-fx day 30))
	 ((2)  (-fx (-fx day 58)  leap-year))
	 ((3)  (-fx (-fx day 89)  leap-year))
	 ((4)  (-fx (-fx day 119) leap-year))
	 ((5)  (-fx (-fx day 150) leap-year))
	 ((6)  (-fx (-fx day 180) leap-year))
	 ((7)  (-fx (-fx day 211) leap-year))
	 ((8)  (-fx (-fx day 242) leap-year))
	 ((9)  (-fx (-fx day 272) leap-year))
	 ((10) (-fx (-fx day 303) leap-year))
	 ((11) (-fx (-fx day 333) leap-year)))))

;; 15.9.1.6
(define (week-day::bint t::llong)
   ;; (modulo (+ (day t) 4) 7)
   ;; 4 for thursday, 01 january 1970
   (let ((t (llong->fixnum (remainderllong (+llong (day t) #l4) #l7))))
      (if (negative? t)
	  (+fx t 7)
	  t)))

;; 15.9.1.8
;(define (localTZA::llong)
;   (*llong #l1000 (fixnum->llong (date-timezone (current-date)))))

;; TODO: caching of TZA is not ok.
(define *localTZA* (*fl 1000.0
			(fixnum->flonum (date-timezone(current-date)))))

;; 15.9.1.9
;(define (daylight-saving-TA::llong t::llong)
;   ;; TODO: daylight-saving
;   #l0)
(define (daylight-saving-TA::double t::double)
   ;; TODO: daylight-saving
   0.0)


;; 15.9.1.9
;; (UTC (local-time t)) is not necessarily always equal to t.
;(define (local-time::llong t::llong)
;   (+llong (+llong t (localTZA))
;	   (daylight-saving-TA t)))
(define (local-time::double t::double)
   (+fl (+fl t *localTZA*) (daylight-saving-TA t)))

;(define (UTC::long t::llong)
;   (let ((local (localTZA))
;	 (t-local (-llong t local)))
;      (-llong t-local
;	      (daylight-saving-TA t-local))))
(define (UTC::double t::double)
   (let ((t-local (-fl t *localTZA*)))
      (-fl t-local (daylight-saving-TA t-local))))

;; 15.9.1.10
(define (hours-from-time::bint t::llong)
   (define *hours-per-day* #l24)
   (define *ms-per-hour* #l3600000)
   (llong->fixnum
    (remainderllong (/llong t *ms-per-hour*) *hours-per-day*)))
(define (min-from-time::bint t::llong)
   (define *minutes-per-hour* #l60)
   (define *ms-per-minute* #l60000)
   (llong->fixnum
    (remainderllong (/llong t *ms-per-minute*) *minutes-per-hour*)))
(define (sec-from-time::bint t::llong)
   (define *seconds-per-minute* #l60)
   (define *ms-per-second* #l1000)
   (llong->fixnum
    (remainderllong (/llong t *ms-per-second*) *seconds-per-minute*)))
(define (ms-from-time::bint t::llong)
   (llong->fixnum (remainderllong t #l1000)))

;; 15.9.1.11
(define (finite?::bool n::double)
   (not (or (NaN? n)
	    (+infinity? n)
	    (-infinity? n))))

(define (make-js-time::double hour::double min::double sec::double ms::double)
   (define *ms-per-hour* 3600000.0)
   (define *ms-per-minute* 60000.0)
   (define *ms-per-second* 1000.0)
   
   (if (not (and (finite? hour) (finite? min) (finite? sec) (finite? ms)))
       (NaN)
       ;; we have to use floats here, as they could potentiall overshoot.
       (let ((hour-i (finite->integer hour))
	     (min-i (finite->integer min))
	     (sec-i (finite->integer sec))
	     (ms-i (finite->integer ms)))
	  (+fl (*fl hour-i *ms-per-hour*)
	       (+fl (*fl min-i *ms-per-minute*)
		    (+fl (*fl sec-i *ms-per-second*)
			 ms-i))))))

;; 15.9.1.12
;; we do not follow completely here. That is we allow a slightly larger
;; interval to still succeed. the result must hence be time-clipped.
(define (make-js-day::double year::double month::double date::double)
   ;; according to 15.9.1.1 we only allow 100.000.000 days.
   ;; some years might take 366 days. So dividing by 365 gives a larger
   ;; interval.
   ;; TODO: find correct borders.
   (define *min-years* (/fl -100000000.0 365.0))
   (define *max-years* (/fl 100000000.0 365.0))

   (define (modulofl n::double n2::double)
      (let ((t (remainderfl n n2)))
	 (if (<fl t 0.0)
	     (+fl t n2)
	     t)))

   (define (time-from-month::llong m::bint leap-year?::bool)
      (let* ((leap-year (if leap-year? 1 0))
	     (nb-days (case m
			((0) 0)
			((1) 31)
			((2) (+fx 59 leap-year))
			((3) (+fx 90 leap-year))
			((4) (+fx 120 leap-year))
			((5) (+fx 151 leap-year))
			((6) (+fx 181 leap-year))
			((7) (+fx 212 leap-year))
			((8) (+fx 243 leap-year))
			((9) (+fx 273 leap-year))
			((10) (+fx 304 leap-year))
			((11) (+fx 334 leap-year)))))
	 (*llong (fixnum->llong nb-days) *ms-per-day*)))

   (if (not (and (finite? year) (finite? month) (finite? date)))
       (NaN)
       (let* ((year-i (finite->integer year))
	      (month-i (finite->integer month))
	      (date-i (finite->integer date))
	      (norm-year (+fl year-i (floorfl (/fl month-i 12.0))))
	      (norm-month (modulofl month-i 12.0)))
	  (if (or (<fl norm-year *min-years*)
		  (>fl norm-year *max-years*))
	      (NaN)
	      (let* ((y (flonum->fixnum norm-year))
		     (t-y (time-from-year y))
		     (leap-year? (=fx (days-in-year y) 366))
		     (t-m (time-from-month (flonum->fixnum norm-month)
					   leap-year?))
		     (d (day (+llong t-y t-m))))
		 (-fl (+fl (llong->flonum d) date-i) 1.0))))))

;; 15.9.1.13
(define (make-js-date::double day::double time::double)
   (if (not (and (finite? day) (finite? time)))
       (NaN)
       (+fl (*fl day (llong->flonum *ms-per-day*))
	    time)))

(define (time-clip::double t::double)
   (cond
      ((not (finite? t)) (NaN))
      ((>fl (absfl t) 8.64e15) (NaN))
      (else (finite->integer t))))

(define (time->string::bstring t::double UTC?::bool)
   (cond
      ((NaN? t)
       "Invalid Date")
      (else
       (let ((utc/local-t (if UTC? t (local-time t))))
	  (format "~a---~a"
		  (seconds->date (flonum->elong
				  (/fl utc/local-t #l1000000)))
		  (flonum->llong t))))))
(define (time->date-string::bstring t::double)
   (cond
      ((NaN? t)
       "Invalid Date")
      (else
       (let* ((t-l (flonum->llong (local-time t)))
	      (w (week-day t-l))
	      (d (date-from-time t-l))
	      (m (month-from-time t-l))
	      (y (year-from-time t-l)))
	  (format "~a ~a ~a ~a"
		  (vector-ref '#("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun") w)
		  (vector-ref '#("Jan" "Feb" "Mar" "Mai" "Jun" "Jul" "Aug"
				       "Sep" "Oct" "Nov" "Dec")
			      m)
		  d
		  y)))))
(define (time->time-string::bstring t::double)
   (cond
      ((NaN? t)
       "Invalid Date")
      (else
       (let* ((t-l (flonum->llong (local-time t)))
	      (h (hours-from-time t-l))
	      (m (min-from-time t-l))
	      (s (sec-from-time t-l)))
	  ;; TODO: include offset to UTC
	  (format "~a:~a:~a" h m s)))))

		  
(define (string->time::double s::bstring)
   (let ((llong-pos (string-cointains s "---")))
      (if (not llong-pos)
	  (NaN)
	  (let ((llong-str (substring s (+fx llong-pos 3)
				      (string-length s))))
	     (llong->flonum (string->llong llong-str))))))
