(module jsre-RegExp
   (include "macros.sch")
   (import jsre-RegExp-parse
	   jsre-object
	   jsre-Object
	   jsre-Function
	   jsre-String
	   jsre-global-object
	   jsre-scope-object
	   jsre-natives
	   jsre-primitives
	   jsre-conversion
	   jsre-globals-tmp)
   (export
    *js-RegExp* ;; can be modified by user -> can't be ::procedure
    (class Js-RegExp::Js-Object
       pattern)
    (RegExp-init)))

(define *js-RegExp* #unspecified)
(define *js-RegExp-prototype*::Js-Object (js-undeclared))

(define-method (js-object->string::bstring o::Js-RegExp)
   "RegExp")

(define (RegExp-init)
   (set! *js-RegExp* (RegExp-lambda))
   (globals-tmp-add! (lambda () (global-runtime-add! 'RegExp *js-RegExp*)))
   (let* ((text-repr "function(p, f) { /* native RegExp */ throw 'native'; }")
	  (regexp-object (create-function-object *js-RegExp*
						 (RegExp-new)
						 RegExp-construct
						 text-repr))
	  (prototype (instantiate::Js-Object           ;; 15.10.6
			(props (make-props-hashtable))
			(proto (js-object-prototype)))))
      
      (set! *js-RegExp-prototype* prototype)

      (js-property-generic-set! regexp-object ;; 15.10.5
				"length"
				2.0
				(length-attributes))
      (js-property-generic-set! regexp-object ;; 15.10.5.1
				"prototype"
				prototype
				(prototype-attributes))

      (js-property-generic-set! prototype                ;; 15.10.6.1
				"constructor"
				*js-RegExp*
				(constructor-attributes))
      (js-property-generic-set! prototype                ;; 15.10.6.2
				"exec"
				(exec)
				(built-in-attributes))
      (js-property-generic-set! prototype                ;; 15.10.6.3
				"test"
				(test)
				(built-in-attributes))
      (js-property-generic-set! prototype                ;; 15.10.6.4
				"toString"
				(toString)
				(built-in-attributes))))

(define (RegExp-lambda)
   (lambda L 'TODO))
(define (RegExp-new)
   (lambda L 'TODO))
(define (RegExp-construct)
   (lambda L 'TODO))

(define (exec)
   (lambda L 'TODO))
(define (test)
   (lambda L 'TODO))
(define (toString)
   (lambda L 'TODO))

