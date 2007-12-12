(module jsre-Eval-env
   (include "macros.sch")
   (import jsre-object
	   jsre-global-object
	   jsre-scope-object
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
   (export (class Js-Eval-env
	      (objs::pair-nil read-only)
	      (next-env read-only))
	   (env-get env::Js-Eval-env id::bstring)
	   (env-typeof-get env::Js-Eval-env id::bstring)
	   ;; returns the given new-val
	   (env-set! env::Js-Eval-env id::bstring new-val)
	   (env-delete! env::Js-Eval-env id::bstring)))

(define (env-get env id)
   (with-access::Js-Eval-env env (objs next-env)
      (let ((entry (any (lambda (obj)
			   (js-property-contains obj id))
			objs)))
	 (cond
	    (entry
	     (unmangle-false entry))
	    (next-env
	     (env-get next-env id))
	    (else
	     (undeclared-error id))))))

(define (env-typeof-get env id)
   (with-access::Js-Eval-env env (objs next-env)
      (let ((entry (any (lambda (obj)
			   (js-property-contains obj id))
			objs)))
	 (cond
	    (entry
	     (unmangle-false entry))
	    (next-env
	     (env-get next-env id))
	    (js-undeclared)))))

(define (env-set! env id new-val)
   (with-access::Js-Eval-env env (objs next-env)
      (let ((mangled (mangle-false new-val)))
	 (cond
	    ((any? (lambda (obj)
		      (js-property-update! obj id mangled))
		   objs)
	     new-val)
	    (next-env
	     (env-set! next-env id new-val))
	    (else
	     (js-property-safe-set! *js-global-object* id new-val)))))
   new-val)
   
(define (env-delete! env id)
   (with-access::Js-Eval-env env (objs next-env)
      (let ((o (any (lambda (obj)
		       (and (js-property-contains obj id)
			    obj))
		    objs)))
	 (cond
	    (o
	     (js-property-safe-delete! o id))
	    (next-env
	     (env-delete! next-env id))
	    (else
	     (delete-error id))))))
