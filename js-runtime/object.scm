(module jsre-object
   (export
    (inline mangle-false val)
    (inline unmangle-false val)
    (class Attributes
       read-only::bool
       deletable::bool
       enumerable::bool)
    (class Property-entry
       val
       attr::Attributes)
    (class Js-Object
       (props read-only) ;; hashtable
       (proto::Js-Object read-only)) ;; prototype
    (generic js-property-one-level-contains?::bool o::Js-Object prop::bstring)
    (generic js-property-is-enumerable? o::Js-Object prop::bstring)
    (generic js-property-contains o::Js-Object prop::bstring)
    (generic js-property-generic-set!
	     o::Js-Object prop::bstring
	     new-val attributes)
    (generic js-property-safe-delete!::bool o::Js-Object prop::bstring)
    (generic js-class-name::bstring o::Js-Object)
    (generic add-enumerables o::Js-Object enumerables-ht shadowed-ht
	     go-into-prototypes?::bool)

    (js-for-in-properties-list::pair-nil o::Js-Object)
    (inline make-props-hashtable)

    (macro get-Attributes)
    (length-attributes::Attributes)
    (built-in-attributes::Attributes)
    (constructor-attributes::Attributes)
    (declared-attributes::Attributes)
    (implicit-attributes::Attributes)
    (default-attributes::Attributes)
    (runtime-attributes::Attributes)
    (readOnly-dontEnum-dontDelete-attributes::Attributes)
    (dontEnum-dontDelete-attributes::Attributes)
    (readOnly-dontDelete-attributes::Attributes)
    (readOnly-dontEnum-attributes::Attributes)
    (readOnly-attributes::Attributes)
    (dontEnum-attributes::Attributes)
    (dontDelete-attributes::Attributes)
    (no-attributes::Attributes))
    )

(define-inline (mangle-false val)
   (or val 'false))
(define-inline (unmangle-false val)
   (if (eq? val 'false)
       #f
       val))

(define-inline (make-string-hashtable)
   (make-hashtable))
(define-inline (make-props-hashtable)
   (make-hashtable))

(define-macro (get-Attributes . Lattrs)
   (cond
      ((and (memq 'read-only Lattrs)
	    (memq 'dont-enum Lattrs)
	    (memq 'dont-delete Lattrs))
       '(readOnly-dontEnum-dontDelete-attributes))
      ((and (memq 'read-only Lattrs)
	    (memq 'dont-enum Lattrs))
       '(readOnly-dontEnum-attributes))
      ((and (memq 'read-only Lattrs)
	    (memq 'dont-delete Lattrs))
       '(readOnly-dontDelete-attributes))
      ((memq 'read-only Lattrs)
       '(readOnly-attributes))
      ((and (memq 'dont-enum Lattrs)
	    (memq 'dont-delete Lattrs))
       '(dontEnum-dontDelete-attributes))
      ((memq 'dont-enum Lattrs)
       '(dontEnum-attributes))
      ((memq 'dont-delete Lattrs)
       '(dontDelete-attributes))
      (else
       '(no-attributes))))

(define-macro (define-attributes)
   (define (concat-name n n2)
      (if (string-null? n)
	  n2
	  (string-append n "-" n2)))

   (define (build-name read-only? deletable? enumerable?)
      (let* ((ro (if read-only? "readOnly" ""))
	     (enum (if enumerable?
		       (concat-name ro "dontEnum")
		       ro))
	     (del (if deletable?
		      enum
		      (concat-name enum "dontDelete"))))
	 (string->symbol (concat-name del "attributes"))))
   (define (global-const n)
      (string->symbol (string-append "*" (symbol->string n) "*")))

   (let loop ((res '((define *no-attributes* (instantiate::Attributes
						(read-only #f)
						(deletable #t)
						(enumerable #f)))
		     (define (no-attributes) *no-attributes*)))
	      (i 1))
      (if (>=fx i 8)
	  (cons 'begin res)
	  (let* ((read-only? (>fx (bit-and i 4) 0))
		 (deletable? (not (>fx (bit-and i 2) 0)))
		 (enumerable? (>fx (bit-and i 1) 0))
		 (name (build-name read-only? deletable? enumerable?)))
	     (loop (cons* `(define ,(global-const name)
			      (instantiate::Attributes
				 (read-only ,read-only?)
				 (deletable ,deletable?)
				 (enumerable ,enumerable?)))
			  `(define (,name)
			      ,(global-const name))
			  res)
		   (+fx i 1))))))

(define-attributes)

;; ECMA 15
(define (length-attributes)   (get-Attributes read-only dont-delete dont-enum))
(define (built-in-attributes) (get-Attributes dont-enum))

;; ECMA 13.2
(define (constructor-attributes) (get-Attributes dont-enum))
	 
;; ECMA 10.2.1 & 10.2.3
(define (declared-attributes) (get-Attributes dont-delete))

;; ECMA 8.6.2.2
(define (default-attributes)  (get-Attributes empty))
;; ECMA 11.13.1->8.7.2->8.6.2.2
(define (implicit-attributes) (default-attributes))
;; runtime-functions are properties of global-object. -> built-in-attributes
;; apply.
(define (runtime-attributes)  (built-in-attributes))


(define-generic (js-property-one-level-contains?::bool o::Js-Object
						       prop::bstring)
   (with-access::Js-Object o (props)
      (and (hashtable-get props prop)
	   #t)))
(define-generic (js-property-is-enumerable? o::Js-Object
					    prop::bstring)
   (with-access::Js-Object o (props)
      (let ((ht-entry (hashtable-get props prop)))
	 (if ht-entry
	     (with-access::Property-entry ht-entry (attr)
		(with-access::Attributes attr (enumerable)
		   enumerable))
	     #f))))
(define-generic (js-property-contains o::Js-Object prop::bstring)
   (with-access::Js-Object o (props proto)
      (let* ((ht-entry (hashtable-get props prop))
	     (entry (and ht-entry (Property-entry-val ht-entry))))
	 (or entry
	     (js-property-contains proto prop)))))

;; if attributes are given, then the original attributes are not used and
;; the original value is replaced (obviously the attributes too).
;; Subclasses might still forbid the replacement of properties (for instance
;; the length-property of an array)
;; if no attributes are given, but the value did not yet exist, then the
;; default-attributes are used.
(define-generic (js-property-generic-set! o::Js-Object prop::bstring
					  new-value attributes)
   ;(print "set!: " prop " <- " new-value)
   (with-access::Js-Object o (props)
      (hashtable-update!
       props
       prop
       (lambda (entry)
	  (with-access::Property-entry entry (val attr)
	     (if attributes
		 (begin
		    (set! attr attributes)
		    (set! val new-value))
		 (with-access::Attributes attr (read-only)
		    (unless read-only
		       (set! val new-value)))))
	     entry) ;; put old entry back in.
       (instantiate::Property-entry
	  (val new-value)
	  (attr (or attributes (default-attributes)))))))

(define-generic (js-property-safe-delete!::bool o::Js-Object prop::bstring)
   (with-access::Js-Object o (props)
      (let ((entry (hashtable-get props prop)))
	 (if (not entry)
	     #t
	     (with-access::Property-entry entry (attr)
		(with-access::Attributes attr (deletable)
		   (if deletable
		       (begin
			  (hashtable-remove! props prop)
			  #t)
		       #f)))))))

(define-generic (js-class-name::bstring o::Js-Object)
   "Object")

(define-generic (add-enumerables o::Js-Object enumerables-ht shadowed-ht
				 go-into-prototypes?::bool)
   (with-access::Js-Object o (props proto)
      (hashtable-for-each
       props
       (lambda (key obj)
	  (unless (hashtable-get shadowed-ht key)
	     (hashtable-put! shadowed-ht key #t)
	     (with-access::Property-entry obj (attr val)
		(with-access::Attributes attr (enumerable)
		   (if enumerable
		       (hashtable-put! enumerables-ht key val)))))))
      (when go-into-prototypes?
	 ;; no need to test for null. null overloads add-enumerables
	 (add-enumerables proto enumerables-ht shadowed-ht #t))))

(define (js-for-in-properties-list::pair-nil o::Js-Object)
   (let ((enumerables-ht (make-string-hashtable))
	 (shadowed-ht (make-string-hashtable)))
      (add-enumerables o enumerables-ht shadowed-ht #t)
      (hashtable-key-list enumerables-ht)))
