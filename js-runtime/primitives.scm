(module jsre-primitives
   (include "macros.sch")
   (use jsre-object
	jsre-natives) ;; undefined, null, ...
   (export (inline js-property-get o::Js-Object prop::bstring)
	   ;; returns the given value
	   (inline js-property-set! o::Js-Object prop::bstring new-val)
	   (js-property-update! o::Js-Object prop::bstring new-val)))


(define-inline (js-property-get o::Js-Object prop::bstring)
   ;(write-circle o)(print)
   ;(write-circle prop)(print)
   (let ((res (js-property-contains o prop)))
      ;(write-circle res)(print)
      (if res
	  (unmangle-false res)
	  (js-undefined))))

;; non-generic. but js-property-generic-set! is.
(define-inline (js-property-set! o::Js-Object prop::bstring new-value)
   (js-property-generic-set! o prop (mangle-false new-value) #f)
   new-value)

(define (js-property-update! o::Js-Object prop::bstring new-value)
   (cond
      ((js-null? o) #f)
      ((js-property-one-level-contains? o prop)
       (js-property-generic-set! o prop new-value #f)
       #t)
      (else
       (with-access::Js-Object o (proto)
	  (js-property-update! proto prop new-value)))))
