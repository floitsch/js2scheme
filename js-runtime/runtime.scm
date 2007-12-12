(module jsre-runtime
   (include "macros.sch")
   (import jsre-object
	   jsre-natives
	   jsre-Error
	   jsre-primitives
	   jsre-global-object
	   jsre-scope-object
	   jsre-globals
	   jsre-eval
	   jsre-Object
	   jsre-Number
	   jsre-String
	   jsre-Function
	   jsre-Bool
	   jsre-Date
	   jsre-Math
	   jsre-Eval-env
	   jsre-operators
	   jsre-conversion
	   jsre-Arguments)
   (from jsre-object
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
	 jsre-Eval-env
	 jsre-Number
	 jsre-operators
	 jsre-conversion
	 jsre-Arguments)
   (eval (export-all)))

(Object-init)
(Function-init)
(Array-init)
(Number-init)
(Bool-init)
(String-init)
(Date-init)
(Math-init)
(Error-init)
(global-object-init)
;(eval '(print (Js-Undefined? (*js-Undefined*_fun))))
