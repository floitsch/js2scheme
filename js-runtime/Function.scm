(module jsre-Function
   (include "macros.sch")
   (import jsre-object
	   jsre-eval
	   jsre-Object ;; recursive dependency :(
	   jsre-Date
	   jsre-String
	   jsre-Number
	   jsre-Bool
	   jsre-natives
	   jsre-Error
	   jsre-primitives
	   jsre-conversion
	   jsre-global-object
	   jsre-scope-object
	   jsre-globals-tmp
	   )
   (export
    *js-Function* ;; can be modified by user -> can't be ::procedure
    (class Js-Function::Js-Object
       new::procedure       ;; when called as a function. by default raises an error.
       construct::procedure ;; when called as constructor. Usually same as 'fun'.
       text-repr)
    (create-function::procedure js-lambda::procedure
				length text-repr)
    (create-function-object js-lambda::procedure
			    new::procedure
			    construct::procedure
			    text-repr)
    (procedure-object::Js-Object p::procedure)
    (Function-init)
    (inline create-empty-object-lambda::Js-Object f-o::Js-Function)))

(define *js-Function* (tmp-js-object))
(define *js-Function-prototype* #unspecified)
(define *js-Function-prototype-object* #unspecified)

(define-method (js-object->string::bstring o::Js-Function)
   "Function")

(define *function-prototype-initialized?* #f)

(define (js-function-prototype)
   ;; 15.3.4
   (unless *function-prototype-initialized?*
       (let* ((f (lambda L (js-undefined))) ;; simply returns 'undefined'
	      (fun-obj (instantiate::Js-Function
			  (props (make-props-hashtable))
			  ;; The only function with object-prototype!
			  (proto (js-object-prototype))
			  (new f)
			  (construct create-empty-object-lambda)
			  (text-repr "function() {/*Function.prototype*/}"))))
	  (hashtable-put! *js-function-objects-ht* f fun-obj)
	  (set! *js-Function-prototype* f)
	  (set! *js-Function-prototype-object* fun-obj)
	  (set! *function-prototype-initialized?* #t)))
   *js-Function-prototype*)

(define (js-function-prototype-object)
   (unless *function-prototype-initialized?*
      (js-function-prototype))
   *js-Function-prototype-object*)

(define (Function-init)
   (set! *js-Function* (Function-lambda))
   (globals-tmp-add! (lambda () (global-runtime-add! 'Function *js-Function*)))
   (let* ((fun-text "function(b) {/* native Function */ throw 'native'; }")
	  (fun-obj (create-function-object *js-Function*
					   *js-Function* ;; new == lambda
					   (lambda (ignored) 'ignored)
					   fun-text))
	  (prototype (js-function-prototype))
	  (prototype-obj (js-function-prototype-object)))
      
      (js-property-generic-set! fun-obj       ;; 15.3.3
				"length"
				1.0
				(length-attributes))
      (js-property-generic-set! fun-obj       ;; 15.3.3.1
				"prototype"
				prototype
				(prototype-attributes))

      ;; prototype is itself a function -> it should have constructor, length
      ;; and prototype properties. These are not mentioned in the spec, but we
      ;; simply assume, that they follow default rules.
      (js-property-generic-set! prototype-obj ;; assumed in 15.3.4
				"constructor"
				prototype
				(constructor-attributes))
      (js-property-generic-set! prototype-obj ;; assumed in 15.3.4
				"length"
				0.0
				(length-attributes))
      (js-property-generic-set! prototype-obj ;; assumed in 15.3.4
				"prototype"
				(js-new *js-Object*)
				(dont-delete-attributes))
				
      (js-property-generic-set! prototype-obj ;; 15.3.4.1
				"constructor"
				*js-Function*
				(built-in-attributes))
      (js-property-generic-set! prototype-obj ;; 15.3.4.2
				"toString"
				(toString)
				(built-in-attributes))))
				  
(define-inline (create-empty-object-lambda::Js-Object f-o::Js-Function)
   (let ((proto (or (js-object (js-property-safe-get f-o "prototype"))
		    (js-object-prototype))))
      (instantiate::Js-Object
	 (props (make-props-hashtable))
	 (proto proto))))

(define *js-function-objects-ht* (make-hashtable #unspecified #unspecified eq?))

;; implements default function creation (as in ECMA 13.2)
;; more sophisticated functions (Array, Bool, ...) might want to do most of
;; this by hand.
;; length can be either floating-point or bint.
;; text-repr should either be a bstring or a list of form: (str start end) such
;; that (substring start end) gives a correct representation.
(define (create-function js-lambda length text-repr)
   ;; 13.2
   (let* ((fun-prototype (js-new *js-Object*))
	  (fun-obj (create-function-object js-lambda ;; lambda
					   js-lambda ;; new
					   create-empty-object-lambda
					   text-repr)))
      
      (js-property-generic-set! fun-prototype
			       "constructor"
			       js-lambda
			       (constructor-attributes))
      
      (js-property-generic-set! fun-obj
			       "length"
			       (if (exact? length)
				   (exact->inexact length)
				   length)
			       (length-attributes))
      
      (js-property-generic-set! fun-obj
			       "prototype"
			       (js-new *js-Object*)
			       ;; ECMA 15.3.5.2
			       (dont-delete-attributes))
      js-lambda))

;; creates a Function-object and registers it in hashtable.
;; Automatically uses function-prototype as prototype.
;; text-repr should either be a bstring or a list of form: (str start end) such
;; that (substring start end) gives a correct representation.
(define (create-function-object js-lambda
				new
				construct
				text-repr)
   (let ((fun-obj (instantiate::Js-Function
		     (props (make-props-hashtable))
		     (proto (js-function-prototype-object))
		     (new new)
		     (construct construct)
		     (text-repr text-repr))))
      (hashtable-put! *js-function-objects-ht* js-lambda fun-obj)
      fun-obj))

(define (procedure-object::Js-Object p::procedure)
   (hashtable-get *js-function-objects-ht* p))

(define (Function-lambda)
   (js-fun-lambda #f #f (nb-args get-arg)
    ()
    (let loop ((i 0)
	       (args "")
	       (body ""))
       (cond
	  ((=fx i nb-args)
	   (js-Function-eval (string-append "(function (" args ") {"
					    body
					    "})")))
	  ((=fx i (- nb-args 1))
	   (loop (+ i 1) args (any->string (get-arg i))))
	  ((=fx i 0)
	   (loop (+ i 1) (any->string (get-arg i)) body))
	  (else
	   (loop (+ i 1)
		 (string-append args "," (any->string (get-arg i)))
		 body))))))

(define (toString)
   (js-fun this #f #f "Function.toString"
	   ()
	   (if (not (Js-Function? this))
	       (type-error "Function-toString applied to" this)
	       (let ((str (Js-Function-text-repr this)))
		  (if (string? str)
		      str
		      (substring (car str) (cadr str) (caddr str)))))))
