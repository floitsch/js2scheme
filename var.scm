(module var
   (include "protobject.sch")
   (include "nodes.sch")
   (option (loadq "protobject-eval.sch"))
   (import protobject
           nodes)
   (export (var-nodes-init!)
           (Decl-of-new-Var id)))

(define (var-nodes-init!)
(define nodes (thread-parameter '*nodes*))
(define-macro (define-node signature . Lrest)
   `(begin
       (define-pclass ,signature ,@Lrest)
       (hashtable-put! nodes ',(car signature) ,(car signature))))

(define-node (Var id)
   (set! this.id id))

(define-node (This-var)
   (set! this.id 'this))

(define-node (With-var id with intercepted-var)
   (set! this.id id)
   (set! this.with with)
   (set! this.intercepted intercepted-var))
(set! With-var.proto (empty-pobject Var))

(define-node (Runtime-var id scm-id)
   (set! this.external? #t)
   (set! this.runtime? #t)
   (set! this.id id)
   (set! this.scm-id scm-id))
(set! Runtime-var.proto (empty-pobject Var))
(set! Runtime-var.proto.runtime? #t)

(define-node (Imported-var id scm-id)
   (set! this.external? #t)
   (set! this.imported? #t)
   (set! this.id id)
   (set! this.scm-id scm-id))
(set! Imported-var.proto (empty-pobject Var))
(set! Imported-var.proto.runtime? #t)

(define-pmethod (Var-reference)
   (let ((var-ref (new-node Var-ref this.id)))
      (set! var-ref.var this)
      var-ref))
(set! Var.proto.reference Var-reference)

(define-pmethod (Var-assig val)
   (let ((var-ref (pcall this Var-reference)))
      (new-node Vassig var-ref val)))
(set! Var.proto.assig Var-assig)
)

(define (Decl-of-new-Var id)
   (let ((decl (new-node Decl id))
         (var (new-node Var id)))
      (set! decl.var var)
      decl))
