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
	   jsre-Math
	   jsre-conversion)
   (from jsre-object
	 jsre-natives
	 jsre-exceptions
	 jsre-primitives
	 jsre-primitive-functions
	 jsre-Object
	 jsre-Function
	 jsre-String
	 jsre-Bool
	 jsre-Date
	 jsre-Number
	 jsre-conversion))

(Object-init)
(Function-init)
(Number-init)
(Bool-init)
(String-init)
(Date-init)
(Math-init)
(init-js-print)
