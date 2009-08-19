(module obfuscate-ids
   (include "protobject.sch")
   (include "nodes.sch")
   (option (loadq "protobject-eval.sch"))
   (import protobject
	   verbose
	   nodes
	   var)
   (export (obfuscate-ids! tree::pobject)
	   *obfuscate-globals*
	   *obfuscation-mapping-p*
	   *imported-global-mapping*))

(define *obfuscation-mapping-p* #f)

(define *obfuscate-globals* #f)

(define *global-mappings* '())
(define *local-mappings* '())

(define *imported-global-mapping* '())
(define *used-ids* (make-hashtable))

(define (obfuscate-ids! tree)
   (verbose "obfuscate-ids!")
   (for-each (lambda (p)
		(hashtable-put! *used-ids* (cadr p) #t))
	     *imported-global-mapping*)
   (overload traverse obf (Node
			   Var-ref)
	     (overload obf obf (Var
				Runtime-var)
		       (tree.traverse)))
   (if *obfuscation-mapping-p*
       (with-output-to-port *obfuscation-mapping-p*
	  (lambda ()
	     (print *global-mappings*)
	     (print *local-mappings*)))))

(define-pmethod (Node-obf)
   (this.traverse0))

(define-pmethod (Var-ref-obf)
   (when (not this.var.obf)
      (tprint this.id)
      (tprint this.var.id))
   (this.var.obf))

(define-pmethod (Var-obf)
   (unless this.generated
      (cond
	 ((eq? this.id 'this)
	  (set! this.generated 'this))
	 ((and this.global?
	       (assq this.id *imported-global-mapping*))
	  =>
	  (lambda (p)
	     (let ((imported-generated (cadr p)))
		(set! this.generated imported-generated))))
	 ((and this.global?
	       (not *obfuscate-globals*))
	  (set! this.generated this.id))
	 (else
	  (set! this.generated (generate-obfuscated-id this.id))))
      (unless (eq? this.id 'this)
	 (if this.global?
	     (set! *global-mappings* (cons (list this.id this.generated)
					   *global-mappings*))
	     (set! *local-mappings* (cons (list this.id this.generated)
					  *local-mappings*))))))

(define-pmethod (Runtime-var-obf)
   (set! this.generated this.id))

(define *counter* 0)
(define (generate-obfuscated-id id)
   (set! *counter* (+ *counter* 1))
   (let ((generated (string->symbol
		     (string-append "v" (number->string *counter*)))))
      (if (hashtable-get *used-ids* generated)
	  (generate-obfuscated-id id)
	  generated)))
