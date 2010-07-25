(module obfuscate-ids
   (include "protobject.sch")
   (include "nodes.sch")
   (option (loadq "protobject-eval.sch"))
   (import protobject
	   verbose
	   nodes
	   var)
   (export (obfuscate-ids! tree::pobject)
	   (add-obfuscation-mapping! js-id::bstring obfuscation-id::bstring)
	   *obfuscate-globals?*
	   *obfuscate-properties?*
	   *obfuscation-mapping-p*))

(define *obfuscation-mapping-p* #f)

(define *obfuscate-globals?* #f)
(define *obfuscate-properties?* #f)

;; Properties are global and must be inside the global mapping.
(define *obfuscation-mapping* (make-hashtable))
(define *used-ids* (make-hashtable))

(define (hashtable->alist ht)
   (map (lambda (from)
	   (list from (hashtable-get ht from)))
	(hashtable-key-list ht)))

(define (obfuscate-ids! tree)
   (verbose "obfuscate-ids!")
   (overload traverse obf (Node
			   Access
			   Property-init
			   Var-ref)
	     (overload obf obf (Var
				Runtime-var)
		       (tree.traverse)))
   (when *obfuscation-mapping-p*
      (with-output-to-port *obfuscation-mapping-p*
	 (lambda () (write (hashtable->alist *obfuscation-mapping*))))))

(define-pmethod (Node-obf)
   (this.traverse0))

(define-pmethod (Var-ref-obf)
   (when (not this.var.obf)
      (tprint this.id)
      (tprint this.var.id))
   (this.var.obf))

(define-pmethod (Access-obf)
   (this.traverse0)
   (when (inherits-from? this.field (node 'String))
      (set! this.field.val (obfuscate-property this.field.val))))

(define-pmethod (Property-init-obf)
   (this.traverse0)
   (when (inherits-from? this.name (node 'String))
      (set! this.name.val (obfuscate-property this.name.val))))

(define-pmethod (Var-obf)
   (unless this.generated
      (cond
	 ((eq? this.id 'this)
	  (set! this.generated 'this))
	 (this.arguments?
	  (set! this.generated 'arguments))
	 ((and this.global?
	       (not *obfuscate-globals?*))
	  ;; do not obfuscate
	  (set! this.generated this.id))
	 (else
	  (set! this.generated (obfuscate-id this.id))))))

(define-pmethod (Runtime-var-obf)
   (set! this.generated this.id))

(define *counter* 0)
(define (generate-obfuscated-id)
   (set! *counter* (+ *counter* 1))
   (string-append "v" (number->string *counter*)))

(define (obfuscate-id id)
   (let* ((str (symbol->string id))
	  (obfuscated-id (hashtable-get *obfuscation-mapping* str)))
      (if obfuscated-id
	  (string->symbol obfuscated-id)
	  (let loop ()
	     (let ((obfuscated-id (generate-obfuscated-id)))
		(if (hashtable-get *used-ids* obfuscated-id)
		    (loop)
		    (begin
		       (add-obfuscation-mapping! str obfuscated-id)
		       (string->symbol obfuscated-id))))))))

(define (obfuscate-property property-str)
   (if (not *obfuscate-properties?*)
       property-str
       (let* ((len (string-length property-str))
	      (without-quotes (substring property-str 1 (- len 1)))
	      (without-quotes-symbol (string->symbol without-quotes))
	      (obfuscated-id (obfuscate-id without-quotes-symbol))
	      (obfuscated-id-str (symbol->string obfuscated-id)))
	  (if (string=? without-quotes obfuscated-id-str)
	      property-str ;; return unobfuscated string.
	      ;; obfuscated ids don't contain quotes.
	      (string-append "'" obfuscated-id-str "'")))))

(define (add-obfuscation-mapping! from::bstring to::bstring)
   (hashtable-put! *obfuscation-mapping* from to)
   (hashtable-put! *used-ids* to #t))
