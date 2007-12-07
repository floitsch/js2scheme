(module jsre-global-object
   (include "macros.sch")
   (import jsre-object
	   jsre-scope-object
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
	   jsre-globals-tmp
	   jsre-Eval-env)
   (export *js-global-object*::Js-Object
	   *js-global-this*::Js-Object ;; alias for js-global-object
	   *js-global-env*
	   (class Js-Global::Js-Scope-Object)
	   (global-object-init)
	   (macro global-declared-add!)
	   (macro global-implicit-add!)
	   (macro global-runtime-add!)
	   (macro global-special-add!)
	   (macro define-runtime-globals)
	   (global-add! id getter::procedure setter::procedure attributes)))


(define-macro (global-declared-add! id v)
   `(scope-var-add *js-global-object* ,id ,v (declared-attributes)))
(define-macro (global-implicit-add! id v)
   `(scope-var-add *js-global-object* ,id ,v (implicit-attributes)))
(define-macro (global-runtime-add! id v)
   `(scope-var-add *js-global-object* ,id ,v (runtime-attributes)))
(define-macro (global-special-add! id v attributes)
   `(scope-var-add *js-global-object* ,id ,v ,attributes))
   
(define-macro (define-runtime-globals . L)
   (let* ((ids (map (lambda (def)
		       (if (pair? (cadr def))
			   (caadr def)
			   (cadr def)))
		    L))
	  (exported-ids (map (lambda (id) (symbol-append 'jsg- id)) ids))
	  (vals (map (lambda (def)
			(if (pair? (cadr def))
			    `(js-fun #f #f #f ,(cdr (cadr def)) ,@(cddr def))
			    (caddr def)))
			L))
	  (defines (map (lambda (id) `(define ,id #f)) exported-ids))
	  (global-adds (map (lambda (id exported-id)
			       `(global-runtime-add! ',id ,exported-id))
			    ids
			    exported-ids))
	  (sets (map (lambda (id val) `(set! ,id ,val)) exported-ids vals)))
      `(begin
;	  (pp ',sets)
	  ,@defines
	  (globals-tmp-add! (lambda () ,@sets ,@global-adds)))))


(define *js-global-env* #unspecified)
(define *js-global-this* (tmp-js-object))
(define *js-global-object* (tmp-js-object))

(define-method (js-object->string::bstring o::Js-Global)
   "global")

(define (global-add! id getter setter attributes)
   ;; TODO: we must ensure, that globals are really added only once.
   (let ((str-id (if (symbol? id) (symbol->string id) id))
	 (ref (instantiate::Ref
		 (getter getter)
		 (setter setter))))
      (js-property-generic-set! *js-global-object*
				str-id
				ref
				attributes)))

(define (global-object-init)
   (set! *js-global-object* (instantiate::Js-Global
			       (props (make-props-hashtable))
			       (proto (js-object-prototype))))
   (set! *js-global-this* *js-global-object*) ;; alias
   (set! *js-global-env* (instantiate::Js-Eval-env
			    (objs (list *js-global-object*))
			    (next-env #f)))
   (for-each (lambda (f) (f)) *globals-init-tmp*))
