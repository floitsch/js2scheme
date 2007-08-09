(module jsre-runtime
   (include "macros.sch")
   (import jsre-object
	   jsre-natives
	   jsre-exceptions
	   jsre-primitives
	   jsre-global-object
	   jsre-globals
	   jsre-Object
	   jsre-Number
	   jsre-String
	   jsre-Function
	   jsre-Bool
	   jsre-Date
	   jsre-Math
	   jsre-operators
	   jsre-conversion)
   (from jsre-object
	 jsre-natives
	 jsre-exceptions
	 jsre-primitives
	 jsre-global-object
	 jsre-globals
	 jsre-Object
	 jsre-Array
	 jsre-Function
	 jsre-String
	 jsre-Bool
	 jsre-Date
	 jsre-Math
	 jsre-Number
	 jsre-operators
	 jsre-conversion)
   (eval (export-all)))

(Object-init)
(Function-init)
(Array-init)
(Number-init)
(Bool-init)
(String-init)
(Date-init)
(Math-init)
(global-object-init)
;(eval '(print (Js-Undefined? (*js-Undefined*_fun))))
