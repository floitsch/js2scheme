(module jsre-base-object
   (import jsre-base-string)
   (use jsre-conversion)
   (export
    (inline mangle-false val)
    (inline unmangle-false val)
    (final-class Attributes
       (read-only::bool read-only)
       (deletable::bool read-only)
       (enumerable::bool read-only))
    (final-class Property-entry
       val
       attr::Attributes)
    ;; for-in must not visit properties that have been deleted.
    ;; using this wide-class we can flag the deleted ones.
    (wide-class Deleted-Property::Property-entry)

    (inline js-null::Js-Object)
    (inline js-null? v)
    (class Js-Object
       (props read-only) ;; hashtable
       (proto::Js-Object read-only)) ;; prototype
    (generic js-property-one-level-contains?::bool o::Js-Object
	     prop::Js-Base-String)
    (generic js-property-is-enumerable?::bool o::Js-Object
	     prop::Js-Base-String)
    (generic js-property-contains o::Js-Object prop::Js-Base-String)
    (generic js-property-generic-set!
	     o::Js-Object prop::Js-Base-String
	     new-val attributes)
    (generic js-property-safe-delete!::bool o::Js-Object prop::Js-Base-String)
    (js-property-direct-delete!::bool o::Js-Object prop::Js-Base-String)
    (generic js-class-name::Js-Base-String o::Js-Object)

    (generic js-property-one-level-for-each o::Js-Object f::procedure)

    (inline make-props-hashtable)

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
   (export (macro get-Attributes))
   )

(define-inline (js-null) (Js-Object-nil))
(define-inline (js-null? v) (eq? v (Js-Object-nil)))

(define-inline (mangle-false val)
   (or val 'false))
(define-inline (unmangle-false val)
   (if (eq? val 'false)
       #f
       val))

(define-inline (make-props-hashtable)
   (create-hashtable :size 10 :hash js-string-hash :eqtest js-string=?))

(define-macro (define-attributes)
   (define (concat-name n n2)
      (if (string-null? n)
	  n2
	  (string-append n "-" n2)))

   (define (build-name read-only? deletable? enumerable?)
      (let* ((ro (if read-only? "readOnly" ""))
	     (enum (if enumerable?
		       ro
		       (concat-name ro "dontEnum")))
	     (del (if deletable?
		      enum
		      (concat-name enum "dontDelete"))))
	 (string->symbol (concat-name del "attributes"))))
   (define (global-const n)
      (string->symbol (string-append "*" (symbol->string n) "*")))

   (let loop ((res '((define *no-attributes* (instantiate::Attributes
						(read-only #f)
						(deletable #t)
						(enumerable #t)))
		     (define (no-attributes) *no-attributes*)))
	      (i 1))
      (if (>=fx i 8)
	  (cons 'begin res)
	  (let* ((read-only? (>fx (bit-and i 4) 0))
		 (deletable? (not (>fx (bit-and i 2) 0)))
		 (enumerable? (not (>fx (bit-and i 1) 0)))
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


(define-generic (js-property-one-level-contains?::bool o::Js-Object prop)
   (with-access::Js-Object o (props)
      (and (hashtable-get props prop)
	   #t)))
(define-generic (js-property-is-enumerable? o::Js-Object prop)
   (with-access::Js-Object o (props)
      (let ((ht-entry (hashtable-get props prop)))
	 (if ht-entry
	     (with-access::Property-entry ht-entry (attr)
		(with-access::Attributes attr (enumerable)
		   enumerable))
	     #f))))
(define-generic (js-property-contains o::Js-Object prop)
   (with-access::Js-Object o (props proto)
      (let* ((ht-entry (hashtable-get props prop))
	     (entry (and ht-entry (Property-entry-val ht-entry))))
	 (cond
	    (entry entry)
	    ((js-null? proto) #f)
	    (else
	     (js-property-contains proto prop))))))

;; if attributes are given, then the original attributes are not used and
;; the original value is replaced (obviously the attributes too).
;; Subclasses might still forbid the replacement of properties (for instance
;; the length-property of an array)
;; if no attributes are given, but the value did not yet exist, then the
;; default-attributes are used.
(define-generic (js-property-generic-set! o::Js-Object prop
					  new-value attributes)
   ;(print "set!: " prop " <- " new-value)
   (with-access::Js-Object o (props)
      ;; hashtable-update! is evil! not only is its return value different,
      ;; when weak hashtables are used, it always creates the 'new' entry.
      ;; -> every set automatically creates a new entry. bad bad bad.
      (let ((entry (hashtable-get props prop)))
	 (if (not entry)
	     (let ((entry (instantiate::Property-entry
			     (val new-value)
			     (attr (or attributes (default-attributes))))))
		(hashtable-put! props prop entry))
	     (with-access::Property-entry entry (val attr)
		(if attributes
		    (begin
		       (set! attr attributes)
		       (set! val new-value))
		    (with-access::Attributes attr (read-only)
		       (unless read-only
			  (set! val new-value)))))))))

(define-generic (js-property-safe-delete!::bool o::Js-Object prop)
   ;; 11.4.1
   (js-property-direct-delete! o prop))

;; is not generic. Basically introduced for Arguments-object.
(define (js-property-direct-delete!::bool o::Js-Object prop::Js-Base-String)
   (with-access::Js-Object o (props proto)
      (let ((entry (hashtable-get props prop)))
	 (cond
	    (entry
	     (with-access::Property-entry entry (attr)
		(with-access::Attributes attr (deletable)
		   (if deletable
		       (begin
			  (widen!::Deleted-Property entry)
			  (hashtable-remove! props prop)
			  #t)
		       #f))))
	    ((js-null? proto)
	     #t) ;; if no element coulde be found return #t.
	    (else
	     (js-property-safe-delete! proto prop))))))

(define-generic (js-class-name::Js-Base-String o::Js-Object)
   (STR "Object"))

;; calls f with prop::string val read-only? deletable? enumerable?
;; guarantees that the property still exists when the procedure is called.
;; The fun can hence be used for the 'for-in' construct.
(define-generic (js-property-one-level-for-each o::Js-Object p::procedure)
   (with-access::Js-Object o (props)
      (hashtable-for-each
       props
       (lambda (key obj)
	  (unless (Deleted-Property? obj)
	     (with-access::Property-entry obj (attr val)
		(with-access::Attributes attr (read-only deletable enumerable)
		   (p key val read-only deletable enumerable))))))))
