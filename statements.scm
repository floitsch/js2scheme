(module statements
   (include "protobject.sch")
   (include "nodes.sch")
   (option (loadq "protobject-eval.sch"))
   (import parser
	   protobject
	   nodes
	   verbose)
   (export (statements tree::pobject)))

(define (statements tree)
   (verbose "statements")
   (overload traverse stmts (Node
			     Block
			     If
			     For
			     While
			     Do
			     For-in
			     With
			     Fun
			     Cond)
	     (tree.traverse #f)))

(define-pmethod (Node-stmts stmt?)
   (if stmt?
       (set! this.stmt? #t))
   (this.traverse1 #f))

(define-pmethod (Block-stmts stmt?)
   (this.traverse1 #t))

(define-pmethod (If-stmts stmt?)
   (this.test.traverse #f)
   (this.then.traverse #t)
   (this.else.traverse #t))

(define-pmethod (For-stmts stmt?)
   (this.init.traverse #f)
   (this.test.traverse #f)
   (this.incr.traverse #f)
   (this.body.traverse #t))

(define-pmethod (While-stmts stmt?)
   (this.test.traverse #f)
   (this.body.traverse #t))

(define-pmethod (Do-stmts stmt?)
   (this.body.traverse #t)
   (this.test.traverse #f))

(define-pmethod (For-in-stmts stmt?)
   (this.lhs.traverse #f)
   (this.obj.traverse #f)
   (this.body.traverse #t))

(define-pmethod (With-stmts stmt?)
   (this.obj.traverse #f)
   (this.body.traverse #t))

(define-pmethod (Fun-stmts stmt?)
   (if stmt?
       (set! this.stmt? #t))
   (this.body.traverse #t))

(define-pmethod (Cond-stmts stmt?)
   (if stmt? (set! this.stmt? #t))
   (this.traverse1 #f))
