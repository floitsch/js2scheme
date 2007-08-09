(module var
   (include "protobject.sch")
   (include "nodes.sch")
   (option (loadq "protobject-eval.sch"))
   (import protobject
           nodes)
   (export Var
	   This-var
	   With-var
	   Runtime-var
	   Imported-var
           (Decl-of-new-Var id)))

(define-pclass (Var id)
   (set! this.id id))

(define-pclass (This-var)
   (set! this.id 'this))

(define-pclass (With-var id with intercepted-var)
   (set! this.id id)
   (set! this.with with)
   (set! this.intercepted intercepted-var))
(set! With-var.proto (empty-pobject Var))

(define-pclass (Runtime-var id scm-id)
   (set! this.external? #t)
   (set! this.runtime? #t)
   (set! this.id id)
   (set! this.scm-id scm-id))
(set! Runtime-var.proto (empty-pobject Var))
(set! Runtime-var.proto.runtime? #t)

(define-pclass (Imported-var id scm-id)
   (set! this.external? #t)
   (set! this.imported? #t)
   (set! this.id id)
   (set! this.scm-id scm-id))
(set! Imported-var.proto (empty-pobject Var))
(set! Imported-var.proto.runtime? #t)

(define-pmethod (Var-reference)
   (let ((var-ref (new Var-ref this.id)))
      (set! var-ref.var this)
      var-ref))
(set! Var.proto.reference Var-reference)

(define-pmethod (Var-assig val)
   (let ((var-ref (pcall this Var-reference)))
      (new Vassig var-ref val)))
(set! Var.proto.assig Var-assig)

(define (Decl-of-new-Var id)
   (let ((decl (new Decl id))
         (var (new Var id)))
      (set! decl.var var)
      decl))
