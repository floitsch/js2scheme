(module jsre-Arguments
   (include "macros.sch")
   (import jsre-object
	   jsre-scope-object
	   jsre-natives)
   (export (class Js-Arguments::Js-Scope-Object))
   (eval (class Js-Arguments)))

;; 10.1.8
(define-method (js-class-name::bstring o::Js-Arguments)
   ;; The actual class-name is not specified.
   ;; Some interpreters use "Arguments" others "Object".
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
