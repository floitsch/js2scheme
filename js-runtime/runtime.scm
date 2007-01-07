(module jsre-runtime
   (include "macros.sch")
   (import jsre-object
	   jsre-natives
	   jsre-exceptions
	   jsre-primitives
	   jsre-primitive-functions
	   jsre-Object
	   jsre-Number
	   jsre-String
	   jsre-Function
	   jsre-Bool
	   jsre-Date
	   jsre-Math)
   (from jsre-object
	 jsre-natives
	 jsre-exceptions
	 jsre-primitives
	 jsre-primitive-functions))

(Object-init)
(Number-init)
(Function-init)
(Bool-init)
(String-init)
(Date-init)
(Math-init)
(init-js-print)
