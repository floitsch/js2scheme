(module js2scheme-comp
   (import config
	   verbose
	   nodes
	   parser
	   expand1
	   stmt-result
	   fun-bindings
	   symbol
	   with
	   ewal
	   label
	   label-resolution
	   simplify-labels
	   bind-exit
	   escape
	   simplify
	   liveness
	   let
	   arguments
	   scm-out
	   js-out)
   (export (js2scheme in-p config)
	   (js2scheme-eval in-p config top-level-obj env top-level-this))
   (from verbose))

(define (js2scheme-eval in-p config top-level-obj env top-level-this)
   (js2scheme-compil in-p config #t top-level-obj env top-level-this))

(define (js2scheme in-p config)
   (js2scheme-compil in-p config
		     #f                   ;; not eval
		     '*js-global-object*
		     '*js-global-env*
		     '*js-global-object*))

(define (js2scheme-compil in-p config
			  eval? top-level-obj env top-level-this)
   (thread-parameter-set! 'top-level-object top-level-obj)
   (thread-parameter-set! 'eval-env env)
   (thread-parameter-set! 'top-level-this top-level-this)
   (thread-parameter-set! 'eval? eval?)
   (config-init! config)
   (let ((ast (parse in-p)))
      (js-out ast)
      (fun-bindings! ast)
      (symbol-resolution! ast '())
      (stmt-result ast)
      (label-resolution ast)
      (simplify-labels! ast)
      (expand1! ast)
      (ewal ast)
      (with! ast)
      (bind-exit! ast)
      (escape ast)
      (simplify ast)
      ;; nice optimization would split the vars, and remove unnecessary
      ;; undefined var-inits.
      (liveness ast)
      (let-intro! ast)
      (arguments ast)
      (scm-out ast)
      ))
