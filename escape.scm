(module escape
   (include "tools.sch")
   (import walk
	   nodes
	   verbose)
   (export (escape tree::Program)))

;; marks escaping variables and stores them in their scope

(define (escape tree)
   (verbose " escape")
   (esc tree #f (make-eq-hashtable) (make-eq-hashtable)))

(define-nmethod (Node.esc decls-ht refs-ht)
   (default-walk this decls-ht refs-ht))

;; Program:
;; in theory we should fill the new decls-ht with the imported und runtime
;; variables. But we are not going to use the result anyways (as they must
;; escape).

(define-nmethod (Scope.esc decls-ht refs-ht)
   (let ((scope-decls-ht (make-eq-hashtable))
	 (scope-refs-ht (make-eq-hashtable)))
      (default-walk this scope-decls-ht scope-refs-ht)
      (hashtable-for-each scope-decls-ht
			  (lambda (key val)
			     (hashtable-remove! scope-refs-ht key)))
      (hashtable-for-each scope-refs-ht
			  (lambda (key val)
			     (with-access::Var key (escapes?)
				(set! escapes? #t))))
      (hashtable-for-each scope-refs-ht
			  (lambda (key val)
			     (hashtable-put! refs-ht key #t)))))

(define-nmethod (Decl.esc decls-ht refs-ht)
   (default-walk this decls-ht refs-ht)
   (hashtable-put! decls-ht (Decl-var this) #t))

(define-nmethod (Ref.esc decls-ht refs-ht)
   (default-walk this decls-ht refs-ht)
   (with-access::Ref this (var id)
      (when (not var)
	 (error #f "Internal Error." id))
      (hashtable-put! refs-ht var #t)))
