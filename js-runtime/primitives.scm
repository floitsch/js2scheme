(module jsre-primitives
   (include "macros.sch")
   (import jsre-object
	   jsre-natives ;; undefined, null, ...
	   jsre-exceptions)
   (export *js-global-this*::Js-Object
	   *js-Object* ;; can be modified by user -> can't be ::Js-Object
	   *js-Object-prototype*::Js-Object
	   *js-Function* ;; can be modified by user -> can't be ::Js-Object
	   *js-Function-prototype*::Js-Object
	   *js-Array* ;; can be modified by user -> can't be ::Js-Object
	   *js-Array-prototype*::Js-Object
	   *js-Number* ;; can be modified by user -> can't be ::Js-Object
	   *js-Number-prototype*::Js-Object
	   *js-Bool* ;; can be modified by user -> can't be ::Js-Object
	   *js-Bool-prototype*::Js-Object
	   *js-String* ;; can be modified by user -> can't be ::Js-Object
	   *js-String-prototype*::Js-Object
	   *js-Date* ;; can be modified by user -> can't be ::Js-Object
	   *js-Date-prototype*::Js-Object
	   *js-Math* ;; can be modified by user -> can't be ::Js-Object
	   (inline js-boolify::bool any)
	   (inline any->bool::bool any)
	   (inline any->number any)
	   (inline any->primitive any hint)
	   (inline any->integer any)
	   (inline any->int32 any)
	   (inline any->uint32 any)
	   (inline any->uint16 any)
	   (inline any->string::bstring any)
	   (inline any->object::Js-Object any)
	   *+infinity* ;; TODO type it
	   *-infinity* ;; TODO type it
	   *NaN*))

(define *tmp-object* #f)
(define (tmp-js-object)
   (or *tmp-object*
       (co-instantiate ((tmp (instantiate::Js-Object
				(props (make-props-hashtable))
				(proto tmp)
				(fun (error-fun "must not invoke"))
				(new (error-fun "must not new")))))
	  (set! *tmp-object* tmp)
	  tmp)))

(define *js-global-this* (tmp-js-object))
(define *js-Object* (tmp-js-object))
(define *js-Object-prototype* (tmp-js-object))
(define *js-Function* (tmp-js-object))
(define *js-Function-prototype* (tmp-js-object))
(define *js-Array* (tmp-js-object))
(define *js-Array-prototype* (tmp-js-object))
(define *js-Number* (tmp-js-object))
(define *js-Number-prototype* (tmp-js-object))
(define *js-Bool* (tmp-js-object))
(define *js-Bool-prototype* (tmp-js-object))
(define *js-String* (tmp-js-object))
(define *js-String-prototype* (tmp-js-object))
(define *js-Date* (tmp-js-object))
(define *js-Date-prototype* (tmp-js-object))
(define *js-Math* (tmp-js-object))

(define *+infinity* (/fl 1.0 0.0))
(define *-infinity* (/fl -1.0 0.0))
(define *NaN* 0.0) ;; TODO

(define-inline (js-boolify::bool any)
   (cond
      ((boolean? any) any)
      ((eq? any *js-Undefined*) #f)
      ((eq? any *js-Null*) #f)
      ((string? any) (not (string=? any "")))
      ((number? any) ;; TODO
       (and (not (=fl any 0.0))
	    (not (=fl any *NaN*))))
      (else #t)))

(define-inline (any->bool::bool any)
   (js-boolify any))

(define-inline (any->number any)
   (cond
      ((number? any) any)
      ((boolean? any) (if any 1.0 0.0)) ;; TODO +0.0
      ((eq? any *js-Undefined*) *NaN*)
      ((eq? any *js-Null*) 0.0)
      ((string? any) (string->number any)) ;; TODO
      (else (any->number (any->primitive any 'number)))))

(define-inline (any->primitive any hint)
   (cond
      ((Js-Object? any) (js-object->primitive any hint))
      (else any)))

(define-inline (any->integer any)
   (define (sign n)
      (if (>=fl n 0.0)
	  1.0
	  -1.0))
   
   (let ((nb (any->number any)))
      (cond
	 ((=fl *NaN* nb) 0.0)
	 ((or (=fl *+infinity* nb)
	      (=fl *-infinity* nb)
	      (=fl 0.0 nb))
	  nb)
	 (else
	  (*fl (sign nb) (floor nb))))))

(define-inline (any->int32 any)
   (let ((nb (any->number any)))
      (cond
	 ((or (=fl *NaN* nb)
	      (=fl *+infinity* nb)
	      (=fl *-infinity* nb)
	      (=fl 0.0 nb))
	  0)
	 (else
	  ;; TODO
	  0))))

(define-inline (any->uint32 any)
   (let ((nb (any->number any)))
      (cond
	 ((or (=fl *NaN* nb)
	      (=fl *+infinity* nb)
	      (=fl *-infinity* nb)
	      (=fl 0.0 nb))
	  0)
	 (else
	  ;; TODO
	  0))))
   
(define-inline (any->uint16 any)
   (let ((nb (any->number any)))
      (cond
	 ((or (=fl *NaN* nb)
	      (=fl *+infinity* nb)
	      (=fl *-infinity* nb)
	      (=fl 0.0 nb))
	  0)
	 (else
	  ;; TODO
	  0))))

(define-inline (any->string::bstring any)
   (cond
      ((string? any) any)
      ((eq? any *js-Null*) "null")
      ((eq? any *js-Undefined*) "undefined")
      ((boolean? any) (if any
		       "true"
		       "false"))
      (else
       (any->string (any->primitive any 'string)))))

(define-inline (any->object::Js-Object any)
   (define (procedure-object p)
      ;; TODO
      (co-instantiate ((tmp (instantiate::Js-Object
			       (props (make-props-hashtable))
			       (proto tmp)
			       (fun (error-fun "must not invoke"))
			       (new (error-fun "must not new")))))
	 tmp))
   (cond
      ((or (eq? any *js-Null*)
	   (eq? any *js-Undefined*))
       (type-error any))
      ((Js-Object? any) any)
      ((string? any) (js-new *js-String* any))
      ((number? any) (js-new *js-Number* any))
      ((procedure? any) (procedure-object any))
      ((boolean? any) (js-new *js-Bool* any))
      (else
       (type-error any))))

(define-inline (js-property-get o prop)
   (let ((o-typed (any->object o))
	 (prop-typed (any->string prop)))
      (js-property-safe-get o-typed prop-typed)))

(define-inline (js-property-set! o prop new-val)
   (let ((o-typed (any->object o))
	 (prop-typed (any->string prop)))
      (js-property-safe-set! o-typed prop-typed new-val)))
