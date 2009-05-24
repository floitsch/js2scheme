(module jsre-primitives
   (use mset
	jsre-base-object
	jsre-base-string
	jsre-natives ;; undefined
	jsre-conversion)
   (export (js-property-get o::Js-Object prop::Js-Base-String)
	   ;; returns the given value
	   (js-property-set! o::Js-Object prop::Js-Base-String new-val)
	   (js-property-update! o::Js-Object prop::Js-Base-String new-val)

	   (js-property-for-each o::Js-Object f::procedure)

	   (inline js-hierarchy-for-each o::Js-Object f::procedure)
	   (js-for-in o::Js-Object f::procedure)))
	   


(define (js-property-get o::Js-Object prop::Js-Base-String)
   ;(write-circle o)(print)
   ;(write-circle prop)(print)
   (let ((res (js-property-contains o prop)))
      ;(write-circle res)(print)
      (if res
	  (unmangle-false res)
	  (js-undefined))))

;; non-generic. but js-property-generic-set! is.
(define (js-property-set! o::Js-Object prop::Js-Base-String new-value)
   (js-property-generic-set! o prop (mangle-false new-value) #f)
   new-value)

(define (js-property-update! o::Js-Object prop::Js-Base-String new-value)
   (cond
      ((js-null? o) #f)
      ((js-property-one-level-contains? o prop)
       (js-property-generic-set! o prop new-value #f)
       #t)
      (else
       (with-access::Js-Object o (proto)
	  (js-property-update! proto prop new-value)))))

(define (js-property-for-each start-o::Js-Object p::procedure)
   (let ((shadowed (make-mset :eqtest js-string=? :hash js-string-hash)))
      (js-hierarchy-for-each
       start-o
       (lambda (o)
	  (js-property-one-level-for-each
	   o
	   (lambda (prop val read-only? deletable? enumerable?)
	      (unless (mset-contains? shadowed prop)
		 (mset-put! shadowed prop)
		 (p prop val read-only? deletable? enumerable?))))))))
   
(define-inline (js-hierarchy-for-each o::Js-Object f::procedure)
   (f o)
   (with-access::Js-Object o (proto)
      (unless (js-null? proto)
	 (js-hierarchy-for-each proto f))))

(define (js-for-in o::Js-Object f::procedure)
   (js-property-for-each o
			 (lambda (prop val read-only? enumerable? enumerable?)
			    (when enumerable? (f prop)))))
