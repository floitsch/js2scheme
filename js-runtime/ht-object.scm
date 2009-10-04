(module jsre-ht-object
   (import jsre-base-string
	   jsre-base-object
	   jsre-property-entry)
   (use jsre-conversion)
   (export
    (abstract-class Js-HT-Object::Js-Object
       (props read-only)) ;; hashtable
    (js-property-direct-delete!::bool o::Js-HT-Object prop::js-string)
    (inline make-props-hashtable)))

(define-inline (make-props-hashtable)
   (create-hashtable :size 10 :hash js-string-hash :eqtest js-string=?))

(define-method (js-property-one-level-contains?::bool o::Js-HT-Object prop)
   (with-access::Js-HT-Object o (props)
      (and (hashtable-get props prop)
	   #t)))

(define-method (js-property-is-enumerable? o::Js-HT-Object prop)
   (with-access::Js-HT-Object o (props)
      (let ((ht-entry (hashtable-get props prop)))
	 (if ht-entry
	     (with-access::Property-entry ht-entry (attr)
		(with-access::Attributes attr (enumerable)
		   enumerable))
	     #f))))

(define-method (js-property-contains o::Js-HT-Object prop)
   (with-access::Js-HT-Object o (props proto)
      (let* ((ht-entry (hashtable-get props prop))
	     (entry (and ht-entry (Property-entry-val ht-entry))))
	 (cond
	    (entry entry)
	    ((js-null? proto) #f)
	    (else
	     (js-property-contains proto prop))))))

(define-method (js-property-generic-set! o::Js-HT-Object prop
					  new-value attributes)
   ;(print "set!: " prop " <- " new-value)
   (with-access::Js-HT-Object o (props)
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

(define-method (js-property-safe-delete!::bool o::Js-HT-Object prop)
   ;; 11.4.1
   (js-property-direct-delete! o prop))

;; is not generic. Basically introduced for Arguments-object.
(define (js-property-direct-delete!::bool o::Js-HT-Object prop::js-string)
   (with-access::Js-HT-Object o (props proto)
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

(define-method (js-property-one-level-for-each o::Js-HT-Object p::procedure)
   (with-access::Js-HT-Object o (props)
      (hashtable-for-each
       props
       (lambda (key obj)
	  (unless (Deleted-Property? obj)
	     (with-access::Property-entry obj (attr val)
		(with-access::Attributes attr (read-only deletable enumerable)
		   (p key val read-only deletable enumerable))))))))