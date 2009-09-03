(module jsre-property-entry
   (export
    (final-class Attributes
       (read-only::bool read-only)
       (deletable::bool read-only)
       (enumerable::bool read-only))
    (final-class Property-entry
       val
       attr::Attributes)
    ;; for-in must not visit properties that have been deleted.
    ;; using this wide-class we can flag the deleted ones.
    (wide-class Deleted-Property::Property-entry)

    (length-attributes::Attributes)
    (built-in-attributes::Attributes)
    (constructor-attributes::Attributes)
    (declared-attributes::Attributes)
    (implicit-attributes::Attributes)
    (default-attributes::Attributes)
    (runtime-attributes::Attributes)
    (readOnly-dontEnum-dontDelete-attributes::Attributes)
    (dontEnum-dontDelete-attributes::Attributes)
    (readOnly-dontDelete-attributes::Attributes)
    (readOnly-dontEnum-attributes::Attributes)
    (readOnly-attributes::Attributes)
    (dontEnum-attributes::Attributes)
    (dontDelete-attributes::Attributes)
    (no-attributes::Attributes))
   (export (macro get-Attributes)))

(define-macro (define-attributes)
   (define (concat-name n n2)
      (if (string-null? n)
	  n2
	  (string-append n "-" n2)))

   (define (build-name read-only? deletable? enumerable?)
      (let* ((ro (if read-only? "readOnly" ""))
	     (enum (if enumerable?
		       ro
		       (concat-name ro "dontEnum")))
	     (del (if deletable?
		      enum
		      (concat-name enum "dontDelete"))))
	 (string->symbol (concat-name del "attributes"))))
   (define (global-const n)
      (string->symbol (string-append "*" (symbol->string n) "*")))

   (let loop ((res '((define *no-attributes* (instantiate::Attributes
						(read-only #f)
						(deletable #t)
						(enumerable #t)))
		     (define (no-attributes) *no-attributes*)))
	      (i 1))
      (if (>=fx i 8)
	  (cons 'begin res)
	  (let* ((read-only? (>fx (bit-and i 4) 0))
		 (deletable? (not (>fx (bit-and i 2) 0)))
		 (enumerable? (not (>fx (bit-and i 1) 0)))
		 (name (build-name read-only? deletable? enumerable?)))
	     (loop (cons* `(define ,(global-const name)
			      (instantiate::Attributes
				 (read-only ,read-only?)
				 (deletable ,deletable?)
				 (enumerable ,enumerable?)))
			  `(define (,name)
			      ,(global-const name))
			  res)
		   (+fx i 1))))))

(define-attributes)

(define-macro (get-Attributes . Lattrs)
   (cond
      ((and (memq 'read-only Lattrs)
	    (memq 'dont-enum Lattrs)
	    (memq 'dont-delete Lattrs))
       '(readOnly-dontEnum-dontDelete-attributes))
      ((and (memq 'read-only Lattrs)
	    (memq 'dont-enum Lattrs))
       '(readOnly-dontEnum-attributes))
      ((and (memq 'read-only Lattrs)
	    (memq 'dont-delete Lattrs))
       '(readOnly-dontDelete-attributes))
      ((memq 'read-only Lattrs)
       '(readOnly-attributes))
      ((and (memq 'dont-enum Lattrs)
	    (memq 'dont-delete Lattrs))
       '(dontEnum-dontDelete-attributes))
      ((memq 'dont-enum Lattrs)
       '(dontEnum-attributes))
      ((memq 'dont-delete Lattrs)
       '(dontDelete-attributes))
      (else
       '(no-attributes))))

;; ECMA 15
(define (length-attributes)   (get-Attributes read-only dont-delete dont-enum))
(define (built-in-attributes) (get-Attributes dont-enum))

;; ECMA 13.2
(define (constructor-attributes) (get-Attributes dont-enum))
	 
;; ECMA 10.2.1 & 10.2.3
(define (declared-attributes) (get-Attributes dont-delete))

;; ECMA 8.6.2.2
(define (default-attributes)  (get-Attributes empty))
;; ECMA 11.13.1->8.7.2->8.6.2.2
(define (implicit-attributes) (default-attributes))
;; runtime-functions are properties of global-object. -> built-in-attributes
;; apply.
(define (runtime-attributes)  (built-in-attributes))
