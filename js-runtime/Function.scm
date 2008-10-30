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
	   jsre-Arguments
	   jsre-Array
	   jsre-primitives
	   jsre-conversion
	   jsre-global-object
	   jsre-scope-object
	   jsre-globals-tmp
	   )
   (export
    *js-Function* ;; can be modified by user -> can't be ::procedure
    (class Js-Function::Js-Object
       fun::procedure      ;;the procedure (a js-fun-lambda)
       new::procedure      ;;when called as constructor. usually same as 'fun'.

       ;; used to construct the object when called as new.
       ;; usually create-empty-object-lambda for non-special procedures.
       ;; exceptions are Array, Number, ....
       construct::procedure
       
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

(define *js-Function* #unspecified)
(define *js-Function-prototype* ;; ::procedure
   (lambda L (js-undefined))) ;; 15.3.4
(define *js-Function-prototype-object*::Js-Object (js-undeclared))

(define-method (js-class-name::bstring o::Js-Function)
   "Function")

(define *function-prototype-initialized?* #f)

(define (js-function-prototype)
   ;; 15.3.4
   (unless *function-prototype-initialized?*
       (let* ((f *js-Function-prototype*)
	      (fun-obj (instantiate::Js-Function
			  (props (make-props-hashtable))
			  ;; The only function with object-prototype!
			  (proto (js-object-prototype))
			  (fun f)
			  (new f)
			  (construct create-empty-object-lambda)
			  (text-repr "function() {/*Function.prototype*/}"))))
	  (hashtable-put! *js-function-objects-ht* f fun-obj)
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
				(get-Attributes dont-enum
						dont-delete read-only))

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
				(get-Attributes dont-enum
						dont-delete read-only))
				
      (js-property-generic-set! prototype-obj ;; 15.3.4.1
				"constructor"
				*js-Function*
				(constructor-attributes))
      (js-property-generic-set! prototype-obj ;; 15.3.4.2
				"toString"
				(toString)
				(built-in-attributes))
      (js-property-generic-set! prototype-obj ;; 15.3.4.3
				"apply"
				(fun-apply)
				(built-in-attributes))
      (js-property-generic-set! prototype-obj ;; 15.3.4.4
				"call"
				(call)
				(built-in-attributes))))
				  
(define-inline (create-empty-object-lambda::Js-Object f-o::Js-Function)
   (let ((proto (or (js-object (js-property-get f-o "prototype"))
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
					   ;;constructed object will be ignored
					   create-empty-object-lambda
					   text-repr)))

      (js-property-generic-set! fun-prototype                 ;; 13.2 - 10
			       "constructor"
			       js-lambda
			       (get-Attributes dont-enum))
      
      (js-property-generic-set! fun-obj                       ;; 15.3.5.1
			       "length"
			       (if (exact? length)
				   (exact->inexact length)
				   length)
			       (get-Attributes dont-delete
					       read-only dont-enum))
      
      (js-property-generic-set! fun-obj                       ;; 15.3.5.2
			       "prototype"
			       fun-prototype
			       (get-Attributes dont-delete))
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
		     (fun js-lambda)
		     (new new)
		     (construct construct)
		     (text-repr text-repr))))
      (hashtable-put! *js-function-objects-ht* js-lambda fun-obj)
      fun-obj))

(define (procedure-object::Js-Object p::procedure)
   (hashtable-get *js-function-objects-ht* p))

(define (Function-lambda)
   ;; 15.3.2.1
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
   ;; 15.3.4.2
   (js-fun this #f #f "Function.toString"
	   ()
	   (if (not (Js-Function? this))
	       (type-error "Function-toString applied to" this)
	       (let ((str (Js-Function-text-repr this)))
		  (if (string? str)
		      str
		      (substring (car str) (cadr str) (caddr str)))))))

(define (fun-apply)
   ;; 15.3.4.3
   ;; some redundancy with js-call. :(

   (define *nb-named-params* 3)
   
   (js-fun this #f #f "Function.apply"
	   (thisArg argArray)
	   (cond
	      ((not (Js-Function? this))
	       (type-error "Function.apply applied to" this))
	      ((not (or (Js-Array? argArray)
			(Js-Arguments? argArray)))
	       (type-error "argArray is neither Array nor Arguments object"
			   argArray))
	      (else
	       (let* ((f (Js-Function-fun this))
		      (call-this (if (or (js-undefined? thisArg)
					 (js-null? thisArg))
				     *js-global-this*
				     (any->object thisArg)))
		      ;; HACK: we only allow bint args.
		      ;;  IIRC bigloo vectors can't have more elements anyways.
		      (len (flonum->fixnum
			    (any->uint32
			     (js-property-get argArray "length"))))
		      (vec (make-vector (maxfx (-fx len *nb-named-params*)
					       0))))
		  ;; start by filling the vector
		  (let loop ((i *nb-named-params*))
		     (unless (>=fx i len)
			(let ((str-i (integer->string i)))
			   (vector-set! vec (-fx i *nb-named-params*)
					(js-property-get argArray str-i))
			   (loop (+fx i 1)))))
		  ;; now get the named params in reverse order
		  (let loop ((args (list vec))
			     (i (-fx *nb-named-params* 1)))
		     (cond
			((<fx i 0)
			 (apply f (cons* call-this
					 f
					 len
					 args)))
			((<fx i len)
			 (let ((str-i (integer->string i)))
			    (loop (cons (js-property-get argArray str-i)
					args)
				  (-fx i 1))))
			(else
			 (loop (cons (js-undefined) args)
			       (-fx i 1))))))))))

(define (call)
   ;; 15.3.4.3
   ;; some redundancy with js-call and with apply :(
   
   (define *nb-named-params* 3)

   (js-fun this #f
	   (nb-args get-arg)
	   "Function.call"
	   (thisArg)
	   (if (not (Js-Function? this))
	       (type-error "Function.apply applied to" this)
	       (let* ((f (Js-Function-fun this))
		      (call-this (if (or (js-undefined? thisArg)
					 (js-null? thisArg))
				     *js-global-this*
				     (any->object thisArg)))
		      (vec-len (- nb-args *nb-named-params* 1)) ;;rm 1 for this
		      (vec (make-vector (maxfx vec-len 0))))
		  ;; start by filling the vector
		  (let loop ((i 0))
		     (when (<fx i vec-len)
			(vector-set! vec i (get-arg (+ i *nb-named-params* 1)))
			(loop (+fx i 1))))
		  ;; now get the named params in reverse order
		  (let loop ((args (list vec))
			     (i *nb-named-params*)) ;; do not remove 1
		     (cond
			((<fx i 1) ;; don't take 0th element.
			 (apply f (cons* call-this
					 f
					 (-fx nb-args 1)
					 args)))
			((<fx i nb-args)
			 (loop (cons (get-arg i) args)
			       (-fx i 1)))
			(else
			 (loop (cons (js-undefined) args)
			       (-fx i 1)))))))))
