(module jsre-global-object
   (include "macros.sch")
   (import jsre-object
	   jsre-natives ;; undefined, null, ...
	   jsre-exceptions
	   jsre-primitives
	   jsre-Object
	   jsre-Date
	   jsre-String
	   jsre-Bool
	   jsre-Number
	   jsre-Function
	   jsre-conversion
	   jsre-globals-tmp)
   (export *js-global-object*::Js-Object
	   (global-object-init)
	   (global-add! id val)))

(define *js-global-object* (tmp-js-object))

(define (global-add! id val)
   ;; TODO: global-add! is only one direction
   (let ((str-id (if (symbol? id) (symbol->string id) id)))
      (js-property-safe-set! *js-global-object* str-id val)))

(define (global-object-init)
   (set! *js-global-object* (js-new *js-Object*))
   (set! *js-global-this* *js-global-object*) ;; alias
   (for-each (lambda (f) (f)) *globals-init-tmp*))
