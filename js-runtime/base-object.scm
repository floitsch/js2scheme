(module jsre-base-object
   (import jsre-base-string
	   jsre-undefined
	   mset)
   (use jsre-conversion)
   (export
    (inline mangle-false val)
    (inline unmangle-false val)

    (inline js-null::Js-Object)
    (inline js-null? v)

    (js-property-get o::Js-Object prop::js-string)
    ;; returns the given value
    (js-property-set! o::Js-Object prop::js-string new-val)
    (js-property-update! o::Js-Object prop::js-string new-val)

    (js-property-for-each o::Js-Object f::procedure)

    (inline js-hierarchy-for-each o::Js-Object f::procedure)
    (js-for-in o::Js-Object f::procedure)


    (abstract-class Js-Object
       (proto::Js-Object read-only)) ;; prototype

    (generic js-property-one-level-contains?::bool o::Js-Object prop::js-string)
    (generic js-property-is-enumerable?::bool o::Js-Object prop::js-string)
    (generic js-property-contains o::Js-Object prop::js-string)
    (generic js-property-generic-set! o::Js-Object prop::js-string new-val attributes)
    (generic js-property-safe-delete!::bool o::Js-Object prop::js-string)
    (generic js-class-name::js-string o::Js-Object)

    (generic js-property-one-level-for-each o::Js-Object f::procedure)))

(define-inline (js-null)    (Js-Object-nil))
(define-inline (js-null? v) (eq? v (Js-Object-nil)))

(define-inline (mangle-false val)
   (or val 'false))
(define-inline (unmangle-false val)
   (if (eq? val 'false)
       #f
       val))

(define (js-property-get o::Js-Object prop::js-string)
   ;(write-circle o)(print)
   ;(write-circle prop)(print)
   (let ((res (js-property-contains o prop)))
      ;(write-circle res)(print)
      (if res
	  (unmangle-false res)
	  (js-undefined))))

(define (js-property-update! o::Js-Object prop::js-string new-value)
   (cond
      ((js-null? o) #f)
      ((js-property-one-level-contains? o prop)
       (js-property-generic-set! o prop new-value #f)
       #t)
      (else
       (with-access::Js-Object o (proto)
	  (js-property-update! proto prop new-value)))))

;; non-generic. but js-property-generic-set! is.
(define (js-property-set! o::Js-Object prop::js-string new-value)
   (js-property-generic-set! o prop (mangle-false new-value) #f)
   new-value)

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



(define-generic (js-property-one-level-contains?::bool o::Js-Object prop)
   (error 'base-object
	  "Internal Error: 'property-one-level-contains?' got through"
	  (class-name (object-class o))))

(define-generic (js-property-is-enumerable? o::Js-Object prop)
   (error 'base-object
	  "Internal Error: 'property-is-enumerable?' got through"
	  (class-name (object-class o))))
(define-generic (js-property-contains o::Js-Object prop)
   (error 'base-object
	  "Internal Error: 'property-contains' got through"
	  (class-name (object-class o))))

;; if attributes are given, then the original attributes are not used and
;; the original value is replaced (obviously the attributes too).
;; Subclasses might still forbid the replacement of properties (for instance
;; the length-property of an array)
;; if no attributes are given, but the value did not yet exist, then the
;; default-attributes are used.
(define-generic (js-property-generic-set! o::Js-Object prop
					  new-value attributes)
   (error 'base-object
	  "Internal Error: 'property-generic-set!' got through"
	  (class-name (object-class o))))

(define-generic (js-property-safe-delete!::bool o::Js-Object prop)
   (error 'base-object
	  "Internal Error: 'property-safe-delete!' got through"
	  (class-name (object-class o))))

(define-generic (js-class-name::js-string o::Js-Object)
   (error 'base-object
	  "Internal Error: 'class-name' got through"
	  (class-name (object-class o))))

;; calls f with prop::string val read-only? deletable? enumerable?
;; guarantees that the property still exists when the procedure is called.
;; The fun can hence be used for the 'for-in' construct.
(define-generic (js-property-one-level-for-each o::Js-Object p::procedure)
   (error 'base-object
	  "Internal Error: 'property-one-level-for-each' got through"
	  (class-name (object-class o))))
