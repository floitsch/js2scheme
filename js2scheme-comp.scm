(module js2scheme-comp
   (include "protobject.sch")
   (include "nodes.sch")
   (option (loadq "protobject-eval.sch"))
   (import protobject
	   config
	   verbose
	   nodes
	   var
	   parser
	   expand1
	   fun-bindings
	   symbol
	   with
	   ewal
	   label
	   label-resolution
	   expand4
	   simplify-labels
	   bind-exit
	   escape
	   simplify
	   liveness
	   let
	   statements
	   arguments
	   scm-out)
   (export (js2scheme in-p config)
	   (js2scheme-eval in-p config top-level-obj env top-level-this))
   (from verbose))

(define (dot-out tree)
   (pobject-dot-out tree (lambda (id)
			    (not (memq id '(imported
					    traverse
					    traverse0
					    traverse1
					    traverse2
					    traverse0!
					    traverse1!
					    traverse2!
					    clone
					    deep-clone
					    already-defined?
					    single-value
					    ))))))

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
   (nodes-init!)
   (var-nodes-init!)
   (label-nodes-init!)
   (let ((ast (parse in-p)))
      (set! ast.top-level-obj top-level-obj)
      (set! ast.env env)
      (set! ast.top-level-this top-level-this)
      (fun-bindings! ast)
      (expand4! ast)
      (symbol-resolution! ast '())
      (label-resolution ast)
      (simplify-labels! ast)
      (expand1! ast)
      (ewal! ast)
      ;(dot-out ast)
      (with! ast)
      (bind-exit! ast)
      (escape ast)
      (simplify! ast)
      ;; nice optimization would split the vars, and remove unnecessary
      ;; undefined var-inits.
      (liveness ast)
      ;(dot-out ast)
      (let-intro! ast)
      ;(dot-out ast)
      (arguments ast)
      (scm-out ast)
      ))
