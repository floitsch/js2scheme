(module jsre-Arguments
   (include "macros.sch")
   (import jsre-object
	   jsre-scope-object
	   jsre-natives)
   (export (class Js-Arguments::Js-Scope-Object))
   (eval (class Js-Arguments)))

(define-method (js-object->string::bstring o::Js-Arguments)
   ;; TODO: verify that class-name of Arguments is really "Arguments"
   "Arguments")

(define-method (js-property-safe-delete!::bool o::Js-Arguments prop::bstring)
   ;; contrary to the scope-object we only have to delete the entry in
   ;; the hashtable. (do not modify the original variable)
   ;;
   ;; copied from object.scm. I do not know any easy way to shortcut the
   ;; scope-object.
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
