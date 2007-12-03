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
    (generic js-property-contains o::Js-Object prop::bstring)
    (generic js-property-generic-set!
	     o::Js-Object prop::bstring
	     new-val attributes)
    (generic js-property-update! o::Js-Object prop::bstring new-val)
    (generic js-property-safe-delete!::bool o::Js-Object prop::bstring)
    (generic js-object->string::bstring o::Js-Object)

    (inline make-props-hashtable)
    (default-attributes)    ;; default attributes for common properties.
    (length-attributes)     ;; default attributes for "length" properties.
    (built-in-attributes)    ;; default attributes for "built-in" properties.
    (prototype-attributes)   ;; same as length-attribute
    (constructor-attributes)
    (declared-attributes)
    (dont-delete-attributes)
    (dont-enum-dont-delete-attributes)
    (runtime-attributes)
    (implicit-attributes)))
   
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

(define *default-attributes*
   (instantiate::Attributes
      (read-only #f)
      (deletable #t)
      (enumerable #t)))
(define (default-attributes)
   *default-attributes*)
(define (runtime-attributes)
   *default-attributes*)
(define (implicit-attributes)
   *default-attributes*)

;; ECMA 10.2.1 & 10.2.3
(define *declared-attributes*
   (instantiate::Attributes
      (read-only #f)
      (deletable #f)
      (enumerable #t)))
(define (declared-attributes)
   *declared-attributes*)

;; ECMA 13.2
(define *constructor-attributes*
   (instantiate::Attributes
      (read-only #f)
      (deletable #t)
      (enumerable #f)))
(define (constructor-attributes)
   *constructor-attributes*)

;; ECMA 15
(define *length-attributes*
   (instantiate::Attributes
      (read-only #t)
      (deletable #f)
      (enumerable #f)))
(define (prototype-attributes)
   *length-attributes*)
(define (length-attributes)
   *length-attributes*)

(define *built-in-attributes*
   (instantiate::Attributes
      (read-only #f)
      (deletable #t)
      (enumerable #f)))
(define (dont-delete-attributes)
   *built-in-attributes*)
(define (built-in-attributes)
   *built-in-attributes*)

(define *dont-enum-dont-delete-attributes*
   (instantiate::Attributes
      (read-only #f)
      (deletable #f)
      (enumerable #f)))
(define (dont-enum-dont-delete-attributes)
   *dont-enum-dont-delete-attributes*)

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

;; if no attributes are given, the default-attributes are used.
(define-generic (js-property-update! o::Js-Object prop::bstring new-value)
   (with-access::Js-Object o (props proto)
      (let* ((entry (hashtable-get props prop)))
	 (if entry
	     (with-access::Property-entry entry (attr val)
		(with-access::Attributes attr (read-only)
		   (when (not read-only)
		      (set! val new-value))
		   #t)) ;; return true: we knew how to handle this.
	     (js-property-update! proto prop new-value)))))

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

(define-generic (js-object->string::bstring o::Js-Object)
   ;; TODO
   "Object")

(define-generic (add-enumerables o::Js-Object ht)
   (with-access::Js-Object o (props proto)
      (hashtable-for-each props
			  (lambda (key obj)
			     (with-access::Attributes (cdr obj) (enumerable)
				(if enumerable
				    (hashtable-put! ht key #t)))))
      (unless (eq? proto 'null)
	 (add-enumerables proto ht))))

(define-inline (js-properties-list::pair-nil o::Js-Object)
   (let ((ht (make-string-hashtable)))
      (add-enumerables o ht)
      (hashtable-key-list ht)))
