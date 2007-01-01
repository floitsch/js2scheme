(module escape
   (include "protobject.sch")
   (include "tools.sch")
   (include "nodes.sch")
   (option (loadq "protobject-eval.sch"))
   (import protobject
	   nodes
	   verbose)
   (export (escape tree::pobject)))

;; marks escaping variables and stores them in their scope

(define (escape tree::pobject)
   (verbose " escape")
   (overload traverse escape (Node
			      Program
			      Scope
			      Decl
			      Var-ref)
	     (tree.traverse #f #f)))

(define-pmethod (Node-escape decls-ht refs-ht)
   (this.traverse2 decls-ht refs-ht))

(define-pmethod (Program-escape decls-ht refs-ht)
   ;; in theory we should fill the new decls-ht with the imported und runtime
   ;; variables. But we are not going to use the result anyways (as they must
   ;; escape). So just two empty hashtables.
   (pcall this Scope-escape (make-eq-hashtable) (make-eq-hashtable)))

(define-pmethod (Scope-escape decls-ht refs-ht)
   (let ((scope-decls-ht (make-eq-hashtable))
	 (scope-refs-ht (make-eq-hashtable)))
      (this.traverse2 scope-decls-ht scope-refs-ht)
      (hashtable-for-each scope-decls-ht
			  (lambda (key val)
			     (hashtable-remove! scope-refs-ht key)))
      (hashtable-for-each scope-refs-ht
			  (lambda (key val)
			     (set! key.escapes? #t)))
      (set! this.escaping-vars (filter! (lambda (var) var.escapes?)
					(hashtable-key-list scope-decls-ht)))
      (set! this.free-vars (hashtable-key-list scope-refs-ht))
      (hashtable-for-each scope-refs-ht
			  (lambda (key val)
			     (hashtable-put! refs-ht key #t)))))

(define-pmethod (Decl-escape decls-ht refs-ht)
   (this.traverse2 decls-ht refs-ht)
   (hashtable-put! decls-ht this.var #t))

(define-pmethod (Var-ref-escape decls-ht refs-ht)
   (this.traverse2 decls-ht refs-ht)
   (if (not this.var)
       (verbose "FUCK FUCK" this.id))
   (hashtable-put! refs-ht this.var #t))
