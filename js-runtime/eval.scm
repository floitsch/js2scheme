(module jsre-eval
;   (library js2scheme-comp)
   (import jsre-Eval-env
	   jsre-exceptions)
   (export (js-eval prog top-level-obj top-level-this next-env . Lenvs)
	   (js-Function-eval src::bstring)))

;; js-eval needs several things:
;; - the program prog (obvious)
;; - top-level-obj: the object which will receive new var-decls. Usually the
;;   scope-object of the surrounding function.
;; - top-level-this: usually the 'this' of the surrounding fun.
;; - next-env: the eval-env before the call to 'eval'.
;; - envs: a list of all environments that need to be searched for variables.
;;   note the top-level-obj is in this list too.
(define (js-eval prog top-level-obj top-level-this next-env . Lenvs)
   ;; TODO error handling
   (let* ((config (let ((ht (make-hashtable)))
		     (hashtable-put! ht 'verbose #t)
		     ht))
	  (p (open-input-string prog))
	  (scm-prog (js2scheme-eval p
				    config
				    top-level-obj
				    (instantiate::Js-Eval-env
				       (objs Lenvs)
				       (next-env next-env))
				    top-level-this))
	  (res (eval scm-prog)))
      (print scm-prog)
      (close-input-port p)
      res))

(define (js-Function-eval src)
   (with-handler
      (lambda (e) (syntax-error e))
      (let* ((config (let ((ht (make-hashtable)))
			(hashtable-put! ht 'verbose #t)
			ht))
	     (p (open-input-string src))
	     (scm-prog (js2scheme-eval p config
				       '*js-global-object*
				       '*js-global-env*
				       ;; we could use *js-global-this* too
				       '*js-global-object*))
	     (res (eval scm-prog)))
	 (print scm-prog)
	 (close-input-port p)
	 res)))
