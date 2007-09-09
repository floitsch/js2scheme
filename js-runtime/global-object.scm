(module jsre-global-object
   (include "macros.sch")
   (import jsre-object
	   jsre-natives ;; undefined, null, ...
	   jsre-exceptions
	   jsre-primitives
	   jsre-Object
	   jsre-Date
	   jsre-String
	   jsre-Bool
	   jsre-Number
	   jsre-Function
	   jsre-conversion
	   jsre-globals-tmp)
   (export *js-global-object*::Js-Object
	   (class Js-Global::Js-Object)
	   (class Ref
	      (getter::procedure read-only)
	      (setter::procedure read-only))
	   (global-object-init)
	   (global-add! id getter::procedure setter::procedure attributes)))

(define *js-global-object* (tmp-js-object))

(define-method (js-object->string::bstring o::Js-Global)
   "global")

(define-method (js-property-contains o::Js-Global prop::bstring)
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

(define-method (js-property-generic-set! o::Js-Global prop::bstring
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
		    ;; we reuse the ref. (was an implicit
		    ;; global, or a runtime-var)
		    ((Ref-setter val) new-val)
		    (instantiate::Property-entry
		       (val val)
		       (attr (or attributes (default-attributes)))))
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
   
(define-method (js-property-safe-delete! o::Js-Global prop::bstring)
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

(define (global-add! id getter setter attributes)
   ;; TODO: we must ensure, that globals are really added only once.
   (let ((str-id (if (symbol? id) (symbol->string id) id))
	 (ref (instantiate::Ref
		 (getter getter)
		 (setter setter))))
      (js-property-generic-set! *js-global-object*
				str-id
				ref
				attributes)))

(define (global-object-init)
   (set! *js-global-object* (instantiate::Js-Global
			       (props (make-props-hashtable))
			       (proto (js-object-prototype))))
   (set! *js-global-this* *js-global-object*) ;; alias
   (for-each (lambda (f) (f)) *globals-init-tmp*))
