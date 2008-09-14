(module jsre-RegExp
   (include "macros.sch")
   (import jsre-RegExp-parse
	   jsre-object
	   jsre-Object
	   jsre-Function
	   jsre-String
	   jsre-Array
	   jsre-Error
	   jsre-global-object
	   jsre-scope-object
	   jsre-natives
	   jsre-primitives
	   jsre-conversion
	   jsre-globals-tmp)
   (import jsre-RegExp-classes
	   jsre-RegExp-fsm
	   jsre-RegExp-parse
	   jsre-RegExp-dot
	   jsre-RegExp-match)
   (export
    *js-RegExp* ;; can be modified by user -> can't be ::procedure
    *js-RegExp-exec*
    (class Js-RegExp::Js-Object
       re)
    (RegExp-init)))

(define *js-RegExp* #unspecified)
(define *js-RegExp-orig* #unspecified)
(define *js-RegExp-prototype*::Js-Object (js-undeclared))
(define *js-RegExp-exec* #unspecified)

(define-method (js-object->string::bstring o::Js-RegExp)
   "RegExp")

(define (RegExp-init)
   (set! *js-RegExp* (RegExp-lambda))
   (set! *js-RegExp-orig* *js-RegExp*)
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

      (set! *js-RegExp-exec* (exec))

      (js-property-generic-set! prototype                ;; 15.10.6.2
				"exec"
				*js-RegExp-exec*
				(built-in-attributes))
      (js-property-generic-set! prototype                ;; 15.10.6.3
				"test"
				(test)
				(built-in-attributes))
      (js-property-generic-set! prototype                ;; 15.10.6.4
				"toString"
				(toString)
				(built-in-attributes))))

(define (RegExp-lambda)                  ;; 15.10.3.1
   (js-fun-lambda
    #f #f #f
    (pattern flags)
    (if (and (Js-RegExp? pattern)
	     (js-undefined? flags))
	pattern
	(js-new *js-RegExp-orig* pattern flags))))

(define (RegExp-new)                     ;; 15.10.4.1
   (js-fun-lambda
    this #f #f
    (pattern flags)
    (cond
       ((and (Js-RegExp? pattern)
	     (js-undefined? flags))
	pattern)
       ((Js-RegExp? pattern)
	(type-error "new RegExp with RegExp as pattern and not undefined flags"
		    flags))
       (else
	(let* ((norm-pattern (if (js-undefined? pattern)
				 ""
				 (any->string pattern)))
	       (norm-flags (if (js-undefined? flags)
			       ""
			       (any->string flags)))
	       (len (string-length norm-flags)))
	   (let loop ((i 0)
		      (global? #f)
		      (ignore-case? #f)
		      (multiline? #f))
	      (cond
		 ((>=fx i len) ;; done
		  (js-property-generic-set! this "source" ;; 15.10.7.1
					    norm-pattern
					    ;; TODO: do not name 'length-attr'
					    (length-attributes))
		  (js-property-generic-set! this "global" ;; 15.10.7.2
					    (mangle-false global?)
					    ;; TODO: do not name 'length-attr'
					    (length-attributes))
		  (js-property-generic-set! this "ignoreCase" ;; 15.10.7.3
					    (mangle-false ignore-case?)
					    ;; TODO: do not name 'length-attr'
					    (length-attributes))
		  (js-property-generic-set! this "multiline"  ;; 15.10.7.4
					    (mangle-false multiline?)
					    ;; TODO: do not name 'length-attr'
					    (length-attributes))
		  (js-property-generic-set! this "lastIndex"  ;; 15.10.7.5
					    0.0
					    ;; TODO: do not name 'length-attr'
					    (dont-enum-dont-delete-attributes))
		  (with-access::Js-RegExp this (re)
		     (let ((scm-re (js-regexp->scm-regexp pattern)))
			(when (not scm-re)
			   (syntax-error "Bad RegExp pattern"))
			(set! re (scm-regexp->fsm scm-re)))))
		 ((or (>fx i 3)
		      (and (char=? (string-ref norm-flags i) #\g) global?)
		      (and (char=? (string-ref norm-flags i) #\i) ignore-case?)
		      (and (char=? (string-ref norm-flags i) #\m) multiline?))
		  (syntax-error "Bad RegExp flags"))
		 ((char=? #\g (string-ref norm-flags i))
		  (loop (+fx i 1) #t ignore-case? multiline?))
		 ((char=? #\i (string-ref norm-flags i))
		  (loop (+fx i 1) global? #t multiline?))
		 ((char=? #\m (string-ref norm-flags i))
		  (loop (+fx i 1) global? ignore-case? #t))
		 (else
		  (syntax-error "Bad RegExp flags")))))))
    this))

(define (RegExp-construct::Js-RegExp c)
   (instantiate::Js-RegExp
      (props (make-props-hashtable))
      (proto *js-RegExp-prototype*)
      (re #f)))

(define (exec)                               ;; 15.10.6.2
   (js-fun
    this #f #f
    "RegExp.exec"
    (string)
    (when (not (Js-RegExp? this))
       (type-error "RegExp-exec applied to " this))
    (let* ((s (any->string string))
	   (len (string-length s))
	   (lastIndex (js-property-safe-get this "lastIndex"))
	   (lastIndex-int (flonum->fixnum (any->integer lastIndex)))
	   (global? (js-property-safe-get this "global"))
	   (i (if global?
		  lastIndex-int
		  0)))
       (cond
	  ((or (<fx i 0)
	       (>fx i len))
	   (js-property-safe-set! this "lastIndex" 0.0)
	   (js-null))
	  ((regexp-match (Js-RegExp-re this) s i)
	   =>
	   (lambda (match)
	      (let ((start-index (car match))
		    (final-index (cadr match))
		    (clusters (caddr match))
		    (a (empty-js-Array)))
		 (when global?
		    (js-property-safe-set! this "lastIndex" final-index))
		 (js-property-safe-set! a "index" (fixnum->flonum start-index))
		 (js-property-safe-set! a "input" s)
		 (let loop ((i 0))
		    (cond
		       ((>fx (*fx i 2) (vector-length clusters))
			a)
		       ((=fx i 0)
			(js-property-safe-set!
			 a (integer->string i)
			 (substring s start-index final-index))
			(loop (+fx i 1)))
		       ((not (vector-ref clusters (*fx (-fx i 1) 2)))
			(js-property-safe-set! a (integer->string i)
					       (js-undefined))
			(loop (+fx i 1)))
		       (else
			(js-property-safe-set!
			 a (integer->string i)
			 (substring s
				    (vector-ref clusters (*fx (-fx i 1) 2))
				    (vector-ref clusters
						(+fx (*fx (-fx i 1) 2) 1))))
			(loop (+fx i 1)))))
		 )))
	  (else
	   (js-property-safe-set! this "lastIndex" 0.0)
	   (js-null))))))

(define (test)
   (js-fun
    this #f #f
    "RegExp.test"
    (string)
    (when (not (Js-RegExp? this))
       (type-error "RegExp-test applied to " this))
    (let* ((s (any->string string))
	   (len (string-length s))
	   (lastIndex (js-property-safe-get this "lastIndex"))
	   (lastIndex-int (flonum->fixnum (any->integer lastIndex)))
	   (global? (js-property-safe-get this "global"))
	   (i (if global?
		  lastIndex-int
		  0)))
       (cond
	  ((or (<fx i 0)
	       (>fx i len))
	   (js-property-safe-set! this "lastIndex" 0.0)
	   #f)
	  (global?
	   (let ((match (regexp-match (Js-RegExp-re this) s i)))
	      (if match
		  (begin
		     (js-property-safe-set! this "lastIndex"
					    (fixnum->flonum (cadr match)))
		     #t)
		  (begin
		     (js-property-safe-set! this "lastIndex" 0.0)
		     #f))))
	  ((regexp-test (Js-RegExp-re this) s i)
	   #t)
	  (else
	   (js-property-safe-set! this "lastIndex" 0.0)
	   #f)))))

(define (toString)
   (js-fun
    this #f #f
    "RegExp.toString"
    ()
    (when (not (Js-RegExp? this))
       (type-error "RegExp-toString applied to " this))
    ;; currently implement the "cheap" variant.
    ;; TODO: return a RegularExpressionLiteral
    (let ((src (js-property-safe-get this "source"))
	  (global? (js-property-safe-get this "global"))
	  (ignore-case? (js-property-safe-get this "ignoreCase"))
	  (multiline? (js-property-safe-get this "multiline")))
       (string-append "/" src "/"
		      (if global? "g" "")
		      (if ignore-case? "i" "")
		      (if multiline? "m" "")))))
