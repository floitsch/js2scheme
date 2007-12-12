(module jsre-scope-object
   (include "macros.sch")
   (import jsre-object
	   jsre-global-object
	   jsre-natives ;; undefined, null, ...
	   jsre-Error
	   jsre-primitives
	   jsre-Object
	   jsre-Date
	   jsre-String
	   jsre-Bool
	   jsre-Number
	   jsre-Function
	   jsre-conversion
	   jsre-globals-tmp)
   (export (class Js-Scope-Object::Js-Object)
	   (class Ref
	      (getter::procedure read-only)
	      (setter::procedure read-only))
	   (js-scope-one-level-property-contains? scope-object::Js-Scope-Object
						  id::bstring)
	   (js-create-scope-object::Js-Scope-Object . Lproto))
   (eval (class Ref)))

(define (js-create-scope-object . Lproto)
   (instantiate::Js-Scope-Object
      (props (make-props-hashtable))
      (proto (if (null? Lproto)
		 (js-null)
		 (car Lproto)))))

(define-method (js-object->string::bstring o::Js-Scope-Object)
   "scope-object should never be seen")

(define-method (js-property-contains o::Js-Scope-Object prop::bstring)
   (with-access::Js-Object o (props proto)
      (let ((ht-entry (hashtable-get props prop)))
	 (if ht-entry
	     (let ((val (Property-entry-val ht-entry)))
		(if (Ref? val)
		    ;; dereference
		    (let ((ref-val ((Ref-getter val))))
		       (if (js-undeclared? ref-val)
			   ;; as if the ht-entry didn't exist.
			   (js-property-contains proto prop)
			   (mangle-false ref-val)))
		    val))
	     (js-property-contains proto prop)))))

(define (js-scope-one-level-property-contains? scope-object id)
   (with-access::Js-Object scope-object (props proto)
      (let ((ht-entry (hashtable-get props id)))
	 (and ht-entry
	      (let ((val (Property-entry-val ht-entry)))
		 (if (Ref? val)
		     ;; dereference
		     (let ((ref-val ((Ref-getter val))))
			(not (js-undeclared? ref-val)))
		     #t))))))

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
			 (js-undeclared? ((Ref-getter val))))
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
   (with-access::Js-Object o (props proto)
      (let ((ht-entry (hashtable-get props prop)))
	 (if ht-entry
	     (with-access::Property-entry ht-entry (val attr)
		(with-access::Attributes attr (deletable)
		   (cond
		      ((and (Ref? val)
			    (js-undeclared? ((Ref-getter val))))
		       ;; as if we weren't here.
		       (js-property-safe-delete! proto prop))
		      ((not deletable)
		       #f)
		      ((Ref? val)
		       ((Ref-setter val) (js-undeclared))
		       #t)
		      (else
		       (hashtable-remove! props prop)
		       #t))))
	     (js-property-safe-delete! proto prop)))))
