(module jsre-Eval-env
   (import jsre-base-string)
   (use jsre-base-object
	jsre-global-object
	jsre-scope-object
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
   (export (class Js-Eval-env
	      (objs::pair-nil read-only)
	      (next-env read-only))
	   (env-get env::Js-Eval-env id::Js-Base-String)
	   (env-get+object env::Js-Eval-env id::Js-Base-String)
	   (env-typeof-get env::Js-Eval-env id::Js-Base-String)
	   ;; returns the given new-val
	   (env-set! env::Js-Eval-env id::Js-Base-String new-val)
	   (env-delete! env::Js-Eval-env id::Js-Base-String)))

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

;; same as env-get but returns containing obj too.
;; necessary for fun-calls (see 11.2.3-7)
(define (env-get+object env id)
   (with-access::Js-Eval-env env (objs next-env)
      (let loop ((objs objs))
	 (cond
	    ((and (null? objs)
		  next-env)
	     (env-get+object next-env id))
	    ((null? objs)
	     (undeclared-error id))
	    ((js-property-contains (car objs) id)
	     =>
	     (lambda (entry)
		(values (unmangle-false entry)
			(car objs))))
	    (else
	     (loop (cdr objs)))))))

(define (env-typeof-get env id)
   (with-access::Js-Eval-env env (objs next-env)
      (let ((entry (any (lambda (obj)
			   (js-property-contains obj id))
			objs)))
	 (cond
	    (entry
	     (unmangle-false entry))
	    (next-env
	     (env-typeof-get next-env id))
	    (else
	     (js-undefined))))))

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
	     (js-property-set! *js-global-object* id new-val)))))
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
