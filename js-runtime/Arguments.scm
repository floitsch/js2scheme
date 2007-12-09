(module jsre-Arguments
   (include "macros.sch")
   (import jsre-object
	   jsre-scope-object
	   jsre-natives)
   (export (class Js-Arguments::Js-Scope-Object)
	   (macro make-arguments)))

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
   

(define-macro (make-arguments nb-named-params
			      callee nb-args param-vars par-vec)
   (let ((arguments (gensym 'arguments))
	 (counter (gensym 'counter))
	 (new-val (gensym 'new-val)))
      `(let ((,arguments (instantiate::Js-Arguments
			    (props (make-props-hashtable))
			    (proto (js-object-prototype)))))
	  (scope-var-add ,arguments "callee" ,callee
			 ; 'don-enum
			 (built-in-attributes))
	  (scope-var-add ,arguments "length" ,nb-args
			 ; 'don-enum
			 (built-in-attributes))
	  ,@(map (lambda (id c)
		    `(when (< ,c ,nb-args)
			(scope-var-add ,arguments
				       ,(number->string c)
				       ,id
				       ; 'don-enum
				       (built-in-attributes))))
		 param-vars
		 (iota (length param-vars)))
	  (for-each (lambda (,counter)
		       (js-property-generic-set!
			,arguments
			(number->string ,counter)
			(instantiate::Ref
			   (getter (lambda ()
				      (vector-ref ,par-vec
						  (- ,counter
						     ,nb-named-params))))
			   (setter (lambda (,new-val)
				      (vector-set! ,par-vec
						   (- ,counter
						      ,nb-named-params)
						   ,new-val))))
			; 'don't-enum
			(built-in-attributes)))
		    (iota (- ,nb-args ,(length param-vars))))
	  ,arguments)))
