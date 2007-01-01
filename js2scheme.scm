; parse
; expand1
; fun-bindings
; symbol
; label-resolution
; simplify-labels
; bind-exit
; escape
; dead-code removal (important, otherwise liveness sees non-existing assigs)
; nice optimization would be "var splitting".
; liveness
; code-gen
(module js2scheme
   (include "protobject.sch")
   (include "nodes.sch")
   (option (loadq "protobject-eval.sch"))
   (import protobject
	   verbose
	   nodes
	   parser
	   expand1
	   fun-bindings
	   symbol
	   expand2
	   label
	   label-resolution
	   expand3
	   simplify-labels
	   bind-exit
	   escape
	   simplify
	   liveness
	   let
	   statements
	   scm-out)
   (main js2scheme))

(define (dot-out tree)
   (pobject-dot-out tree (lambda (id)
			    (not (memq id '(imported
					    traverse
					    traverse0
					    traverse1
					    traverse2
					    traverse0!
					    traverse1!
					    traverse2!
					    clone
					    deep-clone
					    already-defined?
					    single-value
					    ))))))

(define (js2scheme args)
   (set! *verbose* #t)
   (let ((in-p (current-input-port)))
      (let ((ast (parse in-p)))
	 (fun-bindings! ast)
	 (symbol-resolution ast)
	 (expand2! ast)
	 (label-resolution ast)
	 (expand1! ast)
;	 (expand3! ast)
	 (simplify-labels! ast)
	 (bind-exit! ast)
	 (escape ast)
	 (simplify! ast)
	 ;; nice optimization would split the vars, and remove unnecessary
	 ;; undefined var-inits.
	 (liveness ast)
	 (let-intro! ast)
;	 (dot-out ast)
	 (pp (scm-out ast))
	 )
      'done))
