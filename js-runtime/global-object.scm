(module jsre-global-object
   (import jsre-base-object
	   jsre-base-string)
   (use jsre-natives ;; undefined
	jsre-Error
	jsre-primitives
	jsre-Object
	jsre-Date
	jsre-String
	jsre-Bool
	jsre-Number
	jsre-Function
	jsre-conversion
	jsre-Eval-env)
   (static (final-class Js-Global::Js-Object))
   (export *js-global-object* ;::Js-Object
	   *js-global-this* ;::Js-Object ;; alias for js-global-object
	   *js-global-env*
	   ;; reuse the Property-entry
 	   (wide-class Js-Global-Box::Property-entry
 	      id::Js-Base-String
 	      declared?::bool)
	   (global-object-init)
 	   (macro create-declared-global)
 	   (macro create-implicit-global)
 	   (macro create-special-global)
 	   (macro create-runtime-global)
 	   (macro global-read)
 	   (macro global-typeof-read)
 	   (macro global-set!)
	   (macro global-declared?)
	   (create-global::Js-Global-Box id::Js-Base-String
					 attributes declared? . Linit-val)))

;; should be in global-object.scm
(define-macro (create-declared-global id . Linit-val)
   (if (null? Linit-val)
       `(create-global ,id (declared-attributes) #t)
       `(create-global ,id (declared-attributes) #t ,(car Linit-val))))

(define-macro (create-implicit-global id)
   `(create-global ,id (implicit-attributes) #f))

(define-macro (create-special-global id attributes . Linit-val)
   (if (null? Linit-val)
       `(create-global ,id ,attributes #t)
       `(create-global ,id ,attributes #t ,(car Linit-val))))

(define-macro (create-runtime-global id . Linit-val)
   (if (null? Linit-val)
       `(create-global ,id (runtime-attributes) #t)
       `(create-global ,id (runtime-attributes) #t ,(car Linit-val))))

(define-macro (global-read v)
   `(begin
       (when (not (Js-Global-Box-declared? ,v))
	  (undeclared-error (Js-Global-Box-id ,v)))
       (unmangle-false (Js-Global-Box-val ,v))))

(define-macro (global-declared? v)
   `(Js-Global-Box-declared? ,v))

(define-macro (global-typeof-read v)
   `(if (not (Js-Global-Box-declared? ,v))
	(js-undefined)
	(unmangle-false (Js-Global-Box-val ,v))))

;; always returns the set value.
(define-macro (global-set! v val)
   (let ((tmp-val (gensym 'tmp-val)))
      `(let ((,tmp-val ,val))
	  (if (not (Js-Global-Box-declared? ,v))
	      (js-property-generic-set! *js-global-this*
					(Js-Global-Box-id ,v)
					(mangle-false ,tmp-val) #f)
	      (Js-Global-Box-val-set! ,v (mangle-false ,tmp-val)))
	  ,tmp-val)))

(define-method (object-display o::Js-Global . Lport)
   (if (null? Lport)
       (display '*js-global-this*)
       (display '*js-global-this* (car Lport))))

(define-method (object-write o::Js-Global . Lport)
   (if (null? Lport)
       (write '*js-global-this*)
       (write '*js-global-this* (car Lport))))

;; all globals will have their box in here.
(define *global-boxes* (make-props-hashtable))

(define *js-global-env* #unspecified)
;; this will be replaced with the correct object. But we need an object pretty
;; soon so we can add the global variables.
(define *js-global-this*
      (co-instantiate ((tmp-global-object (instantiate::Js-Global
					     (props (make-props-hashtable))
					     (proto tmp-global-object))))
	 tmp-global-object))

(define *js-global-object* *js-global-this*) ;; alias for *js-global-this*

(define (create-global id attributes declared? . Lnewval)
   (let ((box (hashtable-get *global-boxes* id)))
      (if (not box)
	  (let ((new-box (instantiate::Property-entry
			    (val (if (null? Lnewval)
				     (js-undefined)
				     (car Lnewval)))
			    (attr attributes))))
	     (widen!::Js-Global-Box new-box
		(id id)
		(declared? declared?))
	     (hashtable-put! *global-boxes* id new-box)
	     (when declared?
		(with-access::Js-Global *js-global-this* (props)
		   (hashtable-put! props id new-box)))
	     new-box)
	  (let ((new-declared? declared?))
	     (with-access::Js-Global-Box box (declared? attr val)
		;; 10.1.3: If a property of the same name exists already then
		;; its attributes are used
		;; In our case we have one exception: only declared actually
		;; exist already.
		(when (not declared?)
		   (set! attr attributes)
		   (set! declared? new-declared?)
		   (when new-declared?
		      (with-access::Js-Global *js-global-this* (props)
			 (hashtable-put! props id box))))
		(unless (null? Lnewval)
		   (set! val (car Lnewval))))
	     box))))


(define (global-object-init)
   (let ((tmp-props-table (Js-Global-props *js-global-object*)))
      (set! *js-global-object* (instantiate::Js-Global
				  (props tmp-props-table)
				  (proto (js-object-prototype)))))
   (set! *js-global-this* *js-global-object*) ;; alias
   (set! *js-global-env* (instantiate::Js-Eval-env
			    (objs (list *js-global-object*))
			    (next-env #f)))
   (let ((globals-ht (Js-Global-props *js-global-object*)))
      (hashtable-for-each *global-boxes*
			  (lambda (key val)
			     (with-access::Js-Global-Box val (declared?)
				(when declared?
				   (hashtable-put! globals-ht key val)))))))

;; ======= implementation of Js-Global.

(define-method (js-class-name::Js-Base-String o::Js-Global)
   ;; not defined in ECMA.
   (STR "global"))

(define-method (js-property-one-level-contains? o::Js-Global prop)
   (with-access::Js-Global o (props)
      (and (hashtable-get props prop)
	   #t)))
(define-method (js-property-is-enumerable? o::Js-Global prop)
   (with-access::Js-Global o (props)
      (let ((ht-entry (hashtable-get props prop)))
	 (if ht-entry
	     (with-access::Property-entry ht-entry (attr)
		(with-access::Attributes attr (enumerable)
		   enumerable))
	     #f))))
(define-method (js-property-contains o::Js-Global prop)
   (with-access::Js-Global o (props proto)
      (let* ((ht-entry (hashtable-get props prop))
	     (entry (and ht-entry (Property-entry-val ht-entry))))
	 (cond
	    (entry            entry)
	    ((js-null? proto) #f)
	    (else             (js-property-contains proto prop))))))

(define-method (js-property-generic-set! o::Js-Global prop
					 new-value attributes)
   ;(tprint "set!: " prop " <- " new-value)
   (with-access::Js-Global o (props)
      ;; hashtable-update! is evil! not only is its return value different,
      ;; when weak hashtables are used, it always creates the 'new' entry.
      ;; -> every set automatically creates a new entry. bad bad bad.
      (let ((entry (hashtable-get props prop)))
	 (cond
	    (entry
	     (with-access::Property-entry entry (val attr)
		(if attributes
		    (begin
		       (set! attr attributes)
		       (set! val new-value))
		    (with-access::Attributes attr (read-only)
		       (unless read-only
			  (set! val new-value))))))
	    ((hashtable-get *global-boxes* prop)
	     ;; an implicit variable, or one, that had been deleted.
	     ;; in either case use it.
	     => (lambda (box)
		   (with-access::Js-Global-Box box (val declared? attr)
		      (set! declared? #t)
		      (set! val new-value)
		      ;; implicit-attributes == default-attributes.
		      (set! attr (implicit-attributes)))
		   (hashtable-put! props prop box)))
	    (else
	     (let ((entry (instantiate::Property-entry
			     (val new-value)
			     (attr (or attributes (default-attributes))))))
		(hashtable-put! props prop entry)))))))

(define-method (js-property-safe-delete! o::Js-Global prop)
   (with-access::Js-Global o (props)
      (let ((entry (hashtable-get props prop)))
	 (if (not entry)
	     #t
	     (with-access::Property-entry entry (attr)
		(with-access::Attributes attr (deletable)
		   (if deletable
		       (begin
			  (hashtable-remove! props prop)
			  (if (Js-Global-Box? entry)
			      (with-access::Js-Global-Box entry (declared? val)
				 (set! val #f) ;; release memory
				 (set! declared? #f))
			      (widen!::Deleted-Property entry))
			  #t)
		       #f)))))))

(define-method (js-property-one-level-for-each o::Js-Global p)
   (with-access::Js-Global o (props)
      (hashtable-for-each
       props
       (lambda (key obj)
	  (cond
	     ((Deleted-Property? obj) 'do-nothing)
	     ((and (Js-Global-Box? obj)
		   (not (Js-Global-Box-declared? obj)))
	      'do-nothing)
	     (else
	     (with-access::Property-entry obj (attr val)
		(with-access::Attributes attr (read-only deletable enumerable)
		   (p key val read-only deletable enumerable)))))))))
