(module nodes
   (import verbose)
   (export
    (class Var
       id::symbol

       (generated (default #f))

       (fun?::bool (default #f))
       (named-fun?::bool (default #f))
       (no-let?::bool (default #f))
       (catch?::bool (default #f))
       (param?::bool (default #f))
       (arguments?::bool (default #f))

       (operator?::bool (default #f))

       (eval?::bool (default #f))
       (eval-next-var (default #f))
       (local-eval?::bool (default #f))
       (eval-obj-id (default #f))

       (scm-id (default #f))

       (global?::bool (default #f))
       (runtime?::bool (default #f))
       (external?::bool (default #f))
       (internal?::bool (default #f))

       (escapes?::bool (default #f))

       (live-begin-stack (default #f))
       (live-end-stack (default #f))

       (arguments-used?::bool (default #f))
       )
    (final-class Internal-Var::Var
       ;; internal? <- true.
       )
    (final-class This-Var::Var
       ;; id <- 'this.
       )
    (final-class Intercepted-Var::Var
       obj-id::symbol
       intercepted::Var)
    (final-class Runtime-Var::Var
       ;; external? <- true.
       ;; runtime? <- true.
       ;; global? <- true.
       )
    (final-class Imported-Var::Var
       ;; external? <- true.
       ;; imported? <- true.
       ;; global? <- true.
       )

    (abstract-class Node
       (live-begins (default #f))
       (live-ends (default #f)))
    (class Ref::Node
       id::symbol
       (var::Var (default (Var-nil))))
    ;; When a decl is unnecessary (because another one existed already) we
    ;; demote it to a Demoted-Decl (wich almost always acts like a Ref).
    (final-class Demoted-Decl::Ref)
    (class Decl::Ref)
    (class Param::Decl)
    (final-class This-Decl::Param)
    (final-class Arguments-Decl::Param)
    (class Scope::Node
       body::Node

       (locals-table (default #f)) ;; a Scope-table.

       (eval?::bool (default #f))
       (local-eval?::bool (default #f))
       )

    (final-class Program::Scope
       (this-decl::Node (default (instantiate::This-Decl
				    (id 'this))))
       (imported::pair-nil (default '()))
       (runtime::pair-nil (default '()))

       (imported-table (default #f))
       (runtime-table (default #f))
       (globals-table (default #f))

       ;; The globals-table contains the union of declared-globals and
       ;; implicit-globals.
       ;; The following lists only contain the Var-nodes (and not the ids).
       (declared-globals::pair-nil (default '())) ;; of Var.
       (implicit-globals::pair-nil (default '())) ;; of Var.

       (function-str-ids-ht (default #f))
       )
    (class Begin::Node
       els::pair-nil)
    (final-class Block::Begin)
    (final-class Sequence::Begin)
    (final-class Var-Decl-List::Begin)
    (final-class NOP::Node)
    (class If::Node
       test::Node
       then::Node
       else::Node)
    (class Loop::Node
       body::Node
       (continue-label (default #f))
       (break-label (default #f)))
    (final-class For::Loop
       init ;; Node or #f
       test ;; Node or #f
       incr) ;; Node or #f
    (final-class While::Loop
       test::Node)
    (final-class Do::Loop
       test::Node)
    (final-class For-In::Loop
       lhs::Node
       obj::Node)
    (class Flow-Interruption::Node)
    (final-class Bind-Exit::Flow-Interruption
       label
       body::Node)
    (final-class Bind-Exit-Invoc::Flow-Interruption
       label
       expr::Node)
    (final-class Continue::Flow-Interruption
       id  ;; symbol or #f
       (label (default #f)))
    (final-class Break::Flow-Interruption
       id  ;; symbol or #f
       (label (default #f)))
    (final-class Return::Flow-Interruption
       expr::Node
       (label (default #f)))
    (class Intercept::Node
       obj-id::symbol
       body::Node

       (eval?::bool (default #f)))
    (final-class With::Intercept
       obj::Node)
    ;; From a symbol-resolution point of view Catch und Named-fun both are nearly
    ;; equivalent to Withs. In addition to pushing an object onto the stack, they
    ;; declare however a new variable. The new variable has to be inside the
    ;; object. For efficiency reasons (especially for Named-fun) a Scope object is
    ;; used. The original obj will be the prototype of the Scope-object. This is
    ;; possible as neither the Named-Fun nor the exception can be
    ;; deleted. (Otherwise a delete would remove the scope-object element and then
    ;; 'show' the object-element.).
    (class Decl-Intercept::Intercept
       (locals-table (default #f))
       decl::Node)
    (final-class Obj-Init::Node
       props::pair-nil)
    (final-class Switch::Node
       key::Node
       cases::pair-nil)
    (final-class Fall-Through::Node)
    (class Switch-Clause::Node
       body::Node
       (break-label (default #f)))
    (final-class Case::Switch-Clause
       expr::Node)
    (final-class Default::Switch-Clause)
    (final-class Throw::Flow-Interruption
       expr::Node)
    (final-class Try::Node
       body::Node
       catch  ;; Node or #f
       finally) ;; Node or #f
    (final-class Catch::Decl-Intercept)
    (final-class Labelled::Node
       id
       (label (default #f))
       body::Node)
    (class Assig::Node
       lhs::Node
       val::Node)
    (class Vassig::Assig)
    (final-class Vassig-Op::Vassig
       op::Node)
    (final-class Init::Vassig)
    (class Accsig::Assig)
    (final-class Accsig-Op::Accsig
       op::Node)
    (final-class Fun-Binding::Vassig)
    (final-class Named-Fun::Decl-Intercept)
    (final-class Fun::Scope
       params::pair-nil
       (this-decl::Node (default (instantiate::This-Decl
				    (id 'this))))
       (arguments-decl::Node (default (instantiate::Arguments-Decl
					 (id 'arguments))))

       (eval-obj-id (default #f))
       (str (default #f))

       (return-label (default #f)))
    (final-class Cond::If)
    (class Call::Node
       op::Node
       args::pair-nil)
    (final-class Eval-Call::Call
       eval-scm-id::symbol
       top-level-object
       env-vars::pair-nil)
    (final-class Method-Call::Call)
    (class Binary::Call)
    (class Unary::Call)
    (final-class Postfix::Call)
    (final-class Delete-Property-Call::Binary)
    (final-class Delete-Call::Unary)
    (final-class New::Node
       class::Node
       args::pair-nil)
    (final-class Access::Node
       obj::Node
       field::Node)
    ;; we consider This to be a var-ref and not to be
    ;; literal.
    (final-class This::Ref)
    (class Literal::Node
       val)
    (final-class Undefined::Literal)
    (final-class Null::Literal)
    (final-class Bool::Literal)
    (final-class Number::Literal)
    (final-class String::Literal)
    (final-class Array::Node
       els::pair-nil
       length::int)
    (final-class Array-Element::Node
       index::int
       expr::Node)
    (final-class Property-Init::Node
       name::String
       val::Node)
    (final-class Reg-Exp::Node
       pattern)

    ;;
    ;; Scheme nodes
    ;;
    (final-class Let*::Node
       vassigs::pair-nil
       body::Node)

    (Decl-of-new-Var::Decl id::symbol)
    (var-reference::Ref v::Var)
    (var-assig::Vassig v::Var val::Node)
    (new-undefined::Undefined)
    (new-null::Null)))

(define (Decl-of-new-Var id)
   (let ((var (instantiate::Internal-Var
		 (internal? #t)
		 (id id))))
      (instantiate::Decl
	 (id id)
	 (var var))))

(define (var-reference v::Var)
   (instantiate::Ref
      (id (Var-id v))
      (var v)))

(define (var-assig v::Var val::Node)
   (let ((var-ref (var-reference v)))
      (instantiate::Vassig
	 (lhs var-ref)
	 (val val))))

(define (new-undefined)
   (instantiate::Undefined (val 'undefined)))

(define (new-null)
   (instantiate::Null (val 'null)))
