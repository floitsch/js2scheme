(module jsre-Function
   (import jsre-base-object
	   jsre-base-string)
   (use jsre-natives
	jsre-eval
	jsre-Object
	jsre-Date
	jsre-String
	jsre-Number
	jsre-Bool
	jsre-Error
	jsre-Arguments
	jsre-Array
	jsre-primitives
	jsre-conversion
	jsre-global-object
	jsre-scope-object
	)
   (export
    (macro js-call)
    (macro js-method-call)
    (macro js-fun-lambda)
    (macro js-fun)
    (macro js-new))
   (include "call-convention.sch")
   (export
    *jsg-Function*
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

(define *jsg-Function* #unspecified)
(define *js-Function-prototype* ;; ::procedure
   (lambda L (js-undefined))) ;; 15.3.4
(define *js-Function-prototype-object*::Js-Object (js-null))

(define-method (js-class-name::Js-Base-String o::Js-Function)
   (STR "Function"))

(define *function-prototype-initialized?* #f)

(define (js-function-prototype)
   ;; 15.3.4
   (unless *function-prototype-initialized?*
       (let* ((f *js-Function-prototype*)
	      (text-repr (STR "function() { /* Function.prototype */ }"))
	      (fun-obj (instantiate::Js-Function
			  (props (make-props-hashtable))
			  ;; The only function with object-prototype!
			  (proto (js-object-prototype))
			  (fun f)
			  (new f)
			  (construct create-empty-object-lambda)
			  (text-repr text-repr))))
	  (hashtable-put! *js-function-objects-ht* f fun-obj)
	  (set! *js-Function-prototype-object* fun-obj)
	  (set! *function-prototype-initialized?* #t)))
   *js-Function-prototype*)

(define (js-function-prototype-object)
   (unless *function-prototype-initialized?*
      (js-function-prototype))
   *js-Function-prototype-object*)

(define (Function-init)
   (let* ((fun (Function-lambda))
	  (fun-text (STR "function() {/*native Function*/ throw 'native'; }"))
	  (fun-obj (create-function-object fun
					   fun ;; new == lambda
					   (lambda (ignored) 'ignored)
					   fun-text))
	  (prototype (js-function-prototype))
	  (prototype-obj (js-function-prototype-object)))

      (set! *jsg-Function* (create-runtime-global (STR "Function") fun))
      
      (js-property-generic-set! fun-obj       ;; 15.3.3
				(STR "length")
				1.0
				(length-attributes))
      (js-property-generic-set! fun-obj       ;; 15.3.3.1
				(STR "prototype")
				prototype
				(get-Attributes dont-enum
						dont-delete read-only))

      ;; prototype is itself a function -> it should have constructor, length
      ;; and prototype properties. These are not mentioned in the spec, but we
      ;; simply assume, that they follow default rules.
      (js-property-generic-set! prototype-obj ;; assumed in 15.3.4
				(STR "constructor")
				prototype
				(constructor-attributes))
      (js-property-generic-set! prototype-obj ;; assumed in 15.3.4
				(STR "length")
				0.0
				(length-attributes))
      (js-property-generic-set! prototype-obj ;; assumed in 15.3.4
				(STR "prototype")
				(js-new (global-read *jsg-Object*))
				(get-Attributes dont-enum
						dont-delete read-only))
				
      (js-property-generic-set! prototype-obj ;; 15.3.4.1
				(STR "constructor")
				fun
				(constructor-attributes))
      (js-property-generic-set! prototype-obj ;; 15.3.4.2
				(STR "toString")
				(toString)
				(built-in-attributes))
      (js-property-generic-set! prototype-obj ;; 15.3.4.3
				(STR "apply")
				(fun-apply)
				(built-in-attributes))
      (js-property-generic-set! prototype-obj ;; 15.3.4.4
				(STR "call")
				(call)
				(built-in-attributes))))
				  
(define-inline (create-empty-object-lambda::Js-Object f-o::Js-Function)
   (let ((proto (or (js-object (js-property-get f-o (STR "prototype")))
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
   (let* ((fun-prototype (js-new (global-read *jsg-Object*)))
	  (fun-obj (create-function-object js-lambda ;; lambda
					   js-lambda ;; new
					   ;;constructed object will be ignored
					   create-empty-object-lambda
					   text-repr)))

      (js-property-generic-set! fun-prototype                 ;; 13.2 - 10
				(STR "constructor")
				js-lambda
				(get-Attributes dont-enum))
      
      (js-property-generic-set! fun-obj                       ;; 15.3.5.1
				(STR "length")
				(if (exact? length)
				    (exact->inexact length)
				    length)
				(get-Attributes dont-delete
						read-only dont-enum))
      
      (js-property-generic-set! fun-obj                       ;; 15.3.5.2
				(STR "prototype")
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
    (if (zerofx? nb-args)
	(js-Function-eval (STR "(function() {})"))
	(let ((body (any->js-string (get-arg (-fx nb-args 1)))))
	   ;; now add the args in front.
	   (let loop ((i (-fx nb-args 2))
		      (str-parts (list (STR ") {") body (STR "})"))))
	      (cond
		 ((<fx i 0)
		  (js-Function-eval
		   (apply js-string-append (cons (STR "(function (")
						 str-parts))))
		 ((=fx i 0)
		  ;; the first arg
		  (loop (-fx i 1)
			(cons (any->js-string (get-arg 0)) str-parts)))
		 (else
		  ;; an arg, but not the first
		  (loop (-fx i 1)
			(cons* (STR ", ") (any->js-string (get-arg i))
			       str-parts)))))))))

(define (toString)
   ;; 15.3.4.2
   (js-fun this #f #f (STR "Function.prototype.toString")
	   ()
	   (if (not (Js-Function? this))
	       (type-error (STR "Function-toString applied to") this)
	       (let ((str (Js-Function-text-repr this)))
		  (if (js-string? str)
		      str
		      (js-substring (car str) (cadr str) (caddr str)))))))

(define (fun-apply)
   ;; 15.3.4.3
   ;; some redundancy with js-call. :(

   (define *nb-named-params* 3)
   
   (js-fun this #f #f (STR "Function.prototype.apply")
	   (thisArg argArray)
	   (cond
	      ((not (Js-Function? this))
	       (type-error (STR "Function.apply applied to") this))
	      ((not (or (Js-Array? argArray)
			(Js-Arguments? argArray)))
	       (type-error (STR "argArray is neither Array nor Arguments object")
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
			     (js-property-get argArray (STR "length")))))
		      (vec (make-vector (maxfx (-fx len *nb-named-params*)
					       0))))
		  ;; start by filling the vector
		  (let loop ((i *nb-named-params*))
		     (unless (>=fx i len)
			(let ((str-i (integer->js-string i)))
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
			 (let ((str-i (integer->js-string i)))
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
	   (STR "Function.prototype.call")
	   (thisArg)
	   (if (not (Js-Function? this))
	       (type-error (STR "Function.apply applied to") this)
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
