(module jsre-Arguments
   (import jsre-scope-object)
   (use jsre-base-object
	jsre-natives)
   (export (class Js-Arguments::Js-Scope-Object))
   (export (macro make-arguments))
   (eval (class Js-Arguments)))

;; 10.1.8
(define-macro (make-arguments nb-named-params
			      callee nb-args param-vars par-vec)
   (let ((arguments (gensym 'arguments))
	 (counter (gensym 'counter))
	 (new-val (gensym 'new-val))
	 (nb-named (length param-vars)))
      `(let ((,arguments (instantiate::Js-Arguments
			    (props (make-props-hashtable))
			    (proto (js-object-prototype)))))
	  (js-property-generic-set! ,arguments
				    "callee"
				    ,callee
				    (get-Attributes dont-enum))
	  (js-property-generic-set! ,arguments
				    "length"
				    (fixnum->flonum ,nb-args)
				    (get-Attributes dont-enum))
	  ;; named vars are added as scope-vars
	  ,@(map (lambda (id c)
		    `(when (< ,c ,nb-args)
			(scope-var-add ,arguments
				       ,(number->string c)
				       ,id
				       (get-Attributes dont-enum))))
		 param-vars
		 (iota nb-named))
	  ;; remaining ones are added as vector-refs
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
			(get-Attributes dont-enum)))
		    (iota (- ,nb-args ,nb-named) ,nb-named))
	  ,arguments)))

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
