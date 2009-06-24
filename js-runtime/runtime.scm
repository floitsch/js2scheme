(module jsre-runtime
   (import jsre-base-object
	   jsre-base-string
	   jsre-natives
	   jsre-Error
	   jsre-primitives
	   jsre-global-object
	   jsre-scope-object
	   jsre-globals
	   jsre-eval
	   jsre-Object
	   jsre-Array
	   jsre-Number
	   jsre-String
	   jsre-Function
	   jsre-Bool
	   jsre-Date
	   jsre-Math
	   jsre-RegExp
	   jsre-Eval-env
	   jsre-operators
	   jsre-conversion
	   jsre-Arguments)
   (from jsre-base-object
	 jsre-base-string
	 jsre-natives
	 jsre-Error
	 jsre-primitives
	 jsre-global-object
	 jsre-scope-object
	 jsre-globals
	 jsre-eval
	 jsre-Object
	 jsre-Array
	 jsre-Function
	 jsre-String
	 jsre-Bool
	 jsre-Date
	 jsre-Math
	 jsre-RegExp
	 jsre-Eval-env
	 jsre-Number
	 jsre-operators
	 jsre-conversion
	 jsre-Arguments)
   (eval (export-all)))

;; Object-init must be before Function-init.
;; in fact: Object-init will first initialize the global Object var, which will
;; then be used by create-function-object.
(Object-init)
(Function-init)
(Array-init)
(Number-init)
(Bool-init)
(String-init)
(Date-init)
(Math-init)
(Error-init)
(RegExp-init)
(globals-init)
(global-object-init) ;; can be last.
