(module jsre-eval
   (library js2scheme-comp)
   (use jsre-Eval-env
	jsre-global-object
	jsre-scope-object
	jsre-object
	jsre-Error)
   (export (js-eval prog top-level-obj top-level-this next-env . Lenvs)
	   (js-Function-eval src::bstring)))

;; TODO: currently we just include the macros. That's clearly not the best
;; solution. but at least it works, and is simple.

(define-macro (read-macros)
   (with-input-from-file "macros.sch"
      (lambda ()
	 (let loop ((ms '()))
	    (let ((e (read)))
	       (if (eof-object? e)
		   (list 'quote ms)
		   (loop (cons e ms))))))))

(define *macros* (read-macros))
(define *eval-init-done?* #f)

;; js-eval needs several things:
;; - the program prog (obvious)
;; - top-level-obj: the object which will receive new var-decls. Usually the
;;   scope-object of the surrounding function.
;; - top-level-this: usually the 'this' of the surrounding fun.
;; - next-env: the eval-env before the call to 'eval'.
;; - envs: a list of all environments that need to be searched for variables.
;;   note the top-level-obj is in this list too.
(define (js-eval prog top-level-obj top-level-this next-env . Lenvs)
   ;(tprint "**" prog)
   ;; TODO error handling
   (if (not (string? prog))
       prog
       (let* ((config (let ((ht (make-hashtable)))
			 ;; evaled-string is there anyways.
			 ;; no need to hide the function-string
			 (hashtable-put! ht 'function-strings #t)
			 ;(hashtable-put! ht 'verbose #t)
			 ht))
	      (p (open-input-string prog)))
	  (unwind-protect
	     (let ((scm-prog (js2scheme-eval p
					     config
					     top-level-obj
					     (instantiate::Js-Eval-env
						(objs Lenvs)
						(next-env next-env))
					     top-level-this)))
;		(eval `(module ,(gensym 'eval)
;			  (library js2scheme-runtime)))
		(unless *eval-init-done?*
		   (eval `(begin ,@*macros*))
		   (set! *eval-init-done?* #t))
		(eval scm-prog))
	     (close-input-port p)))))

(define (js-Function-eval src)
   (with-handler
      (lambda (e) (syntax-error e))
      (let* ((config (let ((ht (make-hashtable)))
			;; evaled-string is there anyways.
			;; no need to hide the function-string
			(hashtable-put! ht 'function-strings #t)
			;(hashtable-put! ht 'verbose #t)
			ht))
	     (p (open-input-string src)))
	 (unwind-protect
	    (let ((scm-prog (js2scheme-eval p
					    config
					    *js-global-object*
					    *js-global-env*
					    ;; we could use *js-global-this* too
					    *js-global-object*)))
;	       (eval `(module ,(gensym 'fun-eval)
;			 (library js2scheme-runtime)))
	       (unless *eval-init-done?*
		  (eval `(begin ,@*macros*))
		  (set! *eval-init-done?* #t))
	       (eval scm-prog))
	    (close-input-port p)))))
