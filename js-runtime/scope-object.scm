(module jsre-scope-object
   (import jsre-object)
   (use jsre-global-object
	jsre-natives ;; undefined
	jsre-Error
	jsre-primitives
	jsre-Object
	jsre-Date
	jsre-String
	jsre-Bool
	jsre-Number
	jsre-Function
	jsre-conversion)
   (export (class Js-Scope-Object::Js-Object)
	   (class Js-Activation-Object::Js-Scope-Object)
	   (class Ref
	      (getter::procedure read-only)
	      (setter::procedure read-only))
	   (js-create-scope-object::Js-Scope-Object . Lproto)
	   (js-create-activation-object::Js-Activation-Object)
	   *js-deleted-token*
	   (inline js-deleted?::bool v))
   (export (macro scope-var-add))
   (eval (class Ref)))

(define-macro (scope-var-add scope-object
			     id v attributes)
   (let ((str-id (gensym 'str-id))
	 (ref (gensym 'ref))
	 (new-val (gensym 'new-val)))
      `(let ((,str-id (if (symbol? ,id) (symbol->string ,id) ,id))
	     (,ref (instantiate::Ref
		      (getter (lambda () ,v))
		      (setter (lambda (,new-val) (set! ,v ,new-val))))))
	  (js-property-generic-set! ,scope-object
				    ,str-id
				    ,ref
				    ,attributes))))

(define *js-deleted-token* (cons 'deleted 'deleted))
(define-inline (js-deleted? v) (eq? v *js-deleted-token*))

(define (js-create-scope-object . Lproto)
   (instantiate::Js-Scope-Object
      (props (make-props-hashtable))
      (proto (if (null? Lproto)
		 (js-null)
		 (car Lproto)))))

(define (js-create-activation-object)
   (instantiate::Js-Activation-Object
      (props (make-props-hashtable))
      (proto (js-null))))

(define-method (js-class-name::bstring o::Js-Scope-Object)
   "scope-object should never be seen")


(define (js-scope-property-one-level-contains? scope-object id)
   (with-access::Js-Object scope-object (props proto)
      (let ((ht-entry (hashtable-get props id)))
	 (and ht-entry
	      (let ((val (Property-entry-val ht-entry)))
		 (if (Ref? val)
		     ;; dereference
		     (let ((ref-val ((Ref-getter val))))
			(not (js-deleted? ref-val)))
		     #t))))))
(define-method (js-property-one-level-contains? o::Js-Scope-Object
						prop::bstring)
   (js-scope-property-one-level-contains? o prop))
(define-method (js-property-is-enumerable? o::Js-Scope-Object
					   prop::bstring)
   (if (js-scope-property-one-level-contains? o prop)
       (with-access::Js-Object o (props)
	  (with-access::Property-entry (hashtable-get props prop) (attr)
	     (with-access::Attributes attr (enumerable)
		enumerable)))
       #f))
(define-method (js-property-contains o::Js-Scope-Object prop::bstring)
   (define (call-proto)
      (with-access::Js-Object o (proto)
	 (if (js-null? proto)
	     #f
	     (js-property-contains proto prop))))

   (with-access::Js-Object o (props proto)
      (let ((ht-entry (hashtable-get props prop)))
	 (if ht-entry
	     (let ((val (Property-entry-val ht-entry)))
		(if (Ref? val)
		    ;; dereference
		    (let ((ref-val ((Ref-getter val))))
		       (if (js-deleted? ref-val)
			   ;; as if the ht-entry didn't exist.
			   (call-proto)
			   (mangle-false ref-val)))
		    val))
	     (call-proto)))))

(define-method (js-property-one-level-for-each o::Js-Scope-Object p::procedure)
   (with-access::Js-Object o (props proto)
      (hashtable-for-each
       props
       (lambda (key obj)
	  (with-access::Property-entry obj (attr val)
	     (with-access::Attributes attr (read-only deletable enumerable)
		(cond
		   ((Deleted-Property? obj) 'do-nothing)
		   ((and (Ref? val) ;;ignore entries that are deleted
			 (js-deleted? ((Ref-getter val))))
		    'do-nothing)
		   (else
		    (let ((tmp (if (Ref? val)
				   ((Ref-getter val))
				   val)))
		       (p key tmp read-only deletable enumerable))))))))))
	  
(define-method (js-property-generic-set! o::Js-Scope-Object prop::bstring
					 new-val attributes)
;   (print "setting " prop)
   (with-access::Js-Object o (props)
      (hashtable-update!
       props
       prop
       (lambda (entry)
	  (with-access::Property-entry entry (val attr)
	     (with-access::Attributes attr (read-only)
		(cond
		   ((and (Ref? val)
			 (js-deleted? ((Ref-getter val))))
		    ;; does not exist yet, or has been deleted.
		    ;; we reuse the ref. (was probably an implicit
		    ;; global, or a runtime-var)
		    ((Ref-setter val) new-val)
		    (set! attr (or attributes (default-attributes)))
		    entry)
		   ((and (Ref? val)
			 attributes)
		    ;; attributes are given -> replace.
		    ((Ref-setter val) new-val)
		    (set! attr attributes)
		    entry)
		   (read-only
		    ;; do nothing
		    entry)
		   ((Ref? val)
		    ((Ref-setter val) new-val)
		    entry)
		   (else ;; simple entry
		    (set! val new-val)
		    entry)))))
       (instantiate::Property-entry
	  (val new-val)
	  (attr (or attributes (default-attributes)))))))
   
(define-method (js-property-safe-delete! o::Js-Scope-Object prop::bstring)
   (define (proto-delete!)
      (with-access::Js-Object o (proto)
	 (if (js-null? proto)
	     #t
	     (js-property-safe-delete! proto prop))))

   (with-access::Js-Object o (props)
      (let ((ht-entry (hashtable-get props prop)))
	 (if ht-entry
	     (with-access::Property-entry ht-entry (val attr)
		(with-access::Attributes attr (deletable)
		   (cond
		      ((and (Ref? val)
			    (js-deleted? ((Ref-getter val))))
		       ;; as if we weren't here.
		       (proto-delete!))
		      ((not deletable)
		       #f)
		      ((Ref? val)
		       ((Ref-setter val) *js-deleted-token*)
		       #t)
		      (else
		       (widen!::Deleted-Property ht-entry)
		       (hashtable-remove! props prop)
		       #t))))
	     (proto-delete!)))))
