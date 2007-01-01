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
       (proto::Js-Object read-only) ;; prototype
       fun::proc   ;; when called as a function. by default raises an error.
       new::proc)  ;; when called as constructor. Usually same as 'fun'.
    (generic js-property-one-level-contains o::Js-Object prop::bstring)
    (generic js-property-contains o::Js-Object prop::bstring)
    (generic js-property-generic-set! o::Js-Object prop::bstring new-val)
    (generic js-property-direct-set!
	     o::Js-Object
	     prop::bstring
	     new-entry::Property-entry)
    (generic js-object->primitive o::Js-Object)
    (generic js-object->string::bstring o::Js-Object)

    (inline safe-new-fun::proc o::Js-Object)
    (inline make-props-hashtable)
    (default-attribute)    ;; default attributes for common properties.
    (length-attribute)     ;; default attributes for "length" properties.
    (built-in-attribute)   ;; default attributes for "built-in" properties.
    (inline js-property-safe-get o::Js-Object prop::bstring)
    (inline js-property-safe-set! o::Js-Object prop::bstring new-val)))
   
(define-inline (mangle-false val)
   (or val 'false))
(define-inline (unmangle-false val)
   (if (eq? val 'false)
       #f
       val))

(define-inline (safe-new-fun o::Js-Object)
   (Js-Object-new o))

(define-inline (make-props-hashtable)
   (make-hashtable))

(define *default-attributes*
   (instantiate::Attributes
      (read-only #f)
      (deletable #t)
      (enumerable #t)))
(define (default-attribute)
   *default-attributes*)

(define *length-attributes*
   (instantiate::Attributes
      (read-only #t)
      (deletable #f)
      (enumerable #f)))
(define (length-attribute)
   *length-attributes*)

(define *built-in-attributes*
   (instantiate::Attributes
      (read-only #f)
      (deletable #t)
      (enumerable #f)))
(define (built-in-attribute)
   *built-in-attributes*)

(define-generic (js-property-one-level-contains o::Js-Object prop::bstring)
   (with-access::Js-Object o (props)
      (let ((entry (hashtable-get props prop)))
	 (and entry (Property-entry-val entry)))))

(define-generic (js-property-contains o::Js-Object prop::bstring)
   (with-access::Js-Object o (props proto)
      (let* ((ht-entry (hashtable-get props prop))
	     (entry (and entry (Property-entry-val entry))))
	 (or entry
	     (js-property-contains proto prop)))))

;; non-generic. but js-property-contains is.
(define-inline (js-property-safe-get o::Js-Object prop::bstring)
   (let ((res (js-property-contains o prop)))
      (if res
	  (unmangle-false res)
	  'undefined)))

;; non-generic. but js-property-generic-set! is.
(define-inline (js-property-safe-set! o::Js-Object prop::bstring new-value)
      (js-property-generic-set! o prop (mangle-false new-value))

(define-generic (js-property-generic-set! o::Js-Object prop::bstring new-value)
   ;(print "set!: " prop " <- " new-value)
   (with-access::Js-Object o (props)
      (hashtable-update!
       props
       prop
       (lambda (entry)
	  (with-access::Property-entry entry (attr)
	     (with-access::Attributes attr (read-only)
		(if (not read-only)
		    (Property-entry-val-set! entry new-value))
		entry))) ;; put old entry back in.
       (instantiate::Property-entry
	  (val new-value)
	  (attr *default-attributes*)))))

(define-generic (js-property-direct-set! o::Js-Object
					 prop::bstring
					 new-entry::Property-entry)
   (with-access::Js-Object o (props)
      (hashtable-put! prop new-entry)))

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

(define-generic (js-object->primitive o::Js-Object hint::symbol)
   "Object")

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
	 (add-enumerables proto hashtable))))

(define-inline (js-properties-list::pair-nil o::Js-Object)
   (let ((ht (make-string-hashtable)))
      (add-enumerables o ht)
      (hashtable-key-list ht)))
