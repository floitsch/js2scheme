(module scm-out
   (library js2scheme-runtime)
   (library utf)
   (import parser
	   symbol
	   config
	   nodes
	   label
	   verbose
	   walk)
   (export (scm-out tree::Program)))

(define (scm-out tree)
   (verbose "scm-out")
   (out tree))

(define *regexps* '())
(define (regexps-reset!) (set! *regexps* '()))
(define (regexps-alist) *regexps*)
(define (add-regexp! pattern flags)
   (let ((reg-id (symbol-append 'jsreg- (gensym))))
      (set! *regexps* (cons (list reg-id pattern flags) *regexps*))
      reg-id))

;; not thread-safe: we have a global variable here.
(define *strs* '())
(define (strs-reset!) (set! *strs* '()))
(define (strs-alist) *strs*)
(define (add-str! str)
   (let ((t (assoc str *strs*)))
      (if t
	  (cdr t)
	  (let ((str-id (symbol-append 'jsstr- (gensym))))
	     (set! *strs* (cons (cons str str-id) *strs*))
	     str-id))))
(define (string-var? var::symbol)
   (string-prefix? "jsstr-" (symbol->string var)))

(define (comp:any->js-string any)
   (cond
      ((and (symbol? any) (string-var? any))
       any)
      ((flonum? any)
       (add-str! (js-string->utf8 (any->js-string any))))
      ((eq? any '(js-undefined))
       (add-str! "undefined"))
      ((equal? any '(js-null))
       (add-str! "null"))
      ((boolean? any)
       (add-str! (if any "true" "false")))
      (else
       #f)))

(define (comp:any->js-object any)
   (if (and (symbol? any)
	    (eq? any 'this))
       'this
       #f))

(define (get/assign-scm-id! var::Var)
   (with-access::Var var (scm-id id)
      (or scm-id
	  (let ((id (gensym (symbol-append 'jsv- id))))
	     (set! scm-id id)
	     id))))

(define-generic (ref-out this::Var)
   (with-access::Var this (global? eval-next-var id local-eval?)
      (let ((scm-id (get/assign-scm-id! this)))
	 (cond
	    ;; don't even try to do fancy stuff when we are in an eval. just
	    ;; access the eval object
	    ((and (thread-parameter 'eval?)
		  global?)
	     (let ((str-id (add-str! (symbol->string id))))
		`(env-get ,(thread-parameter 'eval-env) ,str-id)))
	    (local-eval?
	     ;; stupid thing: evals might delete local variables. If there's a
	     ;; local eval, we need to verify first, if the variable actually
	     ;; still exists.
	     `(if (not (js-deleted? ,scm-id))
		  ,scm-id
		  ,(ref-out eval-next-var)))
	    (global?
	     `(global-read ,scm-id))
	    (else
	     scm-id)))))

(define-method (ref-out this::Intercepted-Var)
   (with-access::Intercepted-Var this (id intercepted obj-id)
      (let ((str-id (add-str! (symbol->string id)))
	    (tmp (gensym 'tmp)))
	 `(let ((,tmp (js-property-contains ,obj-id ,str-id)))
	     (if ,tmp
		 (unmangle-false ,tmp)
		 ,(ref-out intercepted))))))

(define-method (ref-out this::This-Var)
   'this)

(define-method (ref-out this::Imported-Var)
   (with-access::Imported-Var this (id)
      (error "scm-out"
	     "Internal Error. Encountered imported var"
	     id)))

(define-generic (typeof-out this::Var)
   (with-access::Var this (global? id local-eval? eval-next-var)
      (let ((scm-id (get/assign-scm-id! this)))
	 (cond
	    ;; don't even try to do fancy stuff, when we are in an eval. just
	    ;; access the eval object
	    ((and (thread-parameter 'eval?)
		  global?)
	     (let ((str-id (add-str! (symbol->string id))))
		`(env-typeof-get ,(thread-parameter 'eval-env) ,str-id)))
	    (local-eval?
	     ;; stupid thing: evals might delete local variables. If there's a
	     ;; local eval, we need to verify first, if the variable actually
	     ;; still exists.
	     `(if (not (js-deleted? ,scm-id))
		  ,scm-id
		  ,(typeof-out eval-next-var)))
	    (global? `(global-typeof-read ,scm-id))
	    (else scm-id)))))

(define-method (typeof-out this::Intercepted-Var)
   (with-access::Intercepted-Var this (id intercepted obj-id)
      (let ((str-id (add-str! (symbol->string id)))
	    (tmp (gensym 'tmp)))
	 `(let ((,tmp (js-property-contains ,obj-id ,str-id)))
	     (if ,tmp
		 (unmange-false ,tmp)
		 ,(typeof-out intercepted))))))

(define-method (typeof-out this::This-Var)
   'this)

(define-method (typeof-out this::Imported-Var)
   (with-access::Imported-Var this (id)
      (error "scm-out"
	     "Internal Error. Encountered imported var"
	     id)))

;; Local variables can be deleted if they contain an 'eval'. However during the
;; initial assignment of functions these variables just can't be deleted. ->
;; optimize this case.
;; Partially copied from 'Var-set!'.
(define (fun-binding-set! var::Var val)
   (with-access::Var var (global? local-eval?)
      (cond
	 ((and (thread-parameter 'eval?)
	       global?)
	  (set!-out var val))
	 (local-eval?
	  (let ((scm-id (get/assign-scm-id! var)))
	     `(begin (set! ,scm-id ,val) ,scm-id)))
	 (else
	  (set!-out var val)))))

(define-generic (set!-out this::Var val)
   (with-access::Var this (global? id local-eval? eval-next-var named-fun?)
      (let ((scm-id (get/assign-scm-id! this)))
	 (cond
	    ;; don't even try to do fancy stuff when we are in an eval. just
	    ;; set! the id in the eval object
	    ((and (thread-parameter 'eval?)
		  global?)
	     (let ((str-id (add-str! (symbol->string id))))
		`(env-set! ,(thread-parameter 'eval-env) ,str-id ,val)))
	    (local-eval?
	     ;; stupid thing: evals might delete local variables. If there's a
	     ;; local eval, we need to verify first, if the variable actually
	     ;; still exists.
	     `(if (not (js-deleted? ,scm-id))
		  (begin (set! ,scm-id ,val) ,scm-id)
		  ,(set!-out eval-next-var val)))
	    (global?
	     `(global-set! ,scm-id ,val))
	    (named-fun? ;; named funs are read-only
	     `(begin ,val #f))
	    (else
	     `(begin (set! ,scm-id ,val) ,scm-id))))))

(define-method (set!-out this::Intercepted-Var val)
   (with-access::Intercepted-Var this (id obj-id intercepted)
      (let ((str-id (add-str! (symbol->string id))))
	 `(if (js-property-contains ,obj-id ,str-id)
	      ;; do not use generic-set! here
	      (js-property-update! ,obj-id ,str-id ,val)
	      ,(set!-out intercepted val)))))

(define-method (set!-out this::This-Var val)
   (error "Var-set!"
	  "Can't assign to 'this'"
	  #f))

(define-method (set!-out this::Imported-Var val)
   (with-access::Imported-Var this (id)
      (error "scm-out"
	     "Internal Error. Encountered imported var"
	     id)))

(define-generic (delete-out this::Var)
   (with-access::Var this (global? id local-eval? eval-obj-id eval-next-var)
      (let ((scm-id (get/assign-scm-id! this)))
	 (cond
	    (global?
	     (let ((str-id (add-str! (symbol->string id))))
		`(env-delete! ,(thread-parameter 'eval-env) ,str-id)))
	    (local-eval?
	     (let ((str-id (add-str! (symbol->string id))))
		`(if (not (js-deleted? ,scm-id))
		     (js-property-safe-delete! ,eval-obj-id ,str-id)
		     ,(delete-out eval-next-var))))
	    (else
	     #f)))))

(define-method (delete-out this::Intercepted-Var)
   (with-access::Intercepted-Var this (id obj-id intercepted)
      (let* ((str-id (add-str! (symbol->string id))))
	 `(if (js-property-contains ,obj-id ,str-id)
	      (js-property-safe-delete! ,obj-id ,str-id)
	      ,(delete-out intercepted)))))

(define-method (delete-out this::This-Var)
   (error "Var-delete"
	  "Can't delete 'this'"
	  #f))

(define-method (delete-out this::Imported-Var)
   (with-access::Imported-Var this (id)
      (error "scm-out"
	     "Encountered imported var"
	     id)))

;; slightly HACKish: if the referenced var is inside an object (different from
;; an activation object), then it updates (using 'set!') the given
;; base-variable.
;; access+base will return a lambda that should then be executed.
;; this is in response to 11.2.3 which states that the function is first
;; evaluated without taking the value.
(define-generic (access+base this::Var base)
   (with-access::Var this (global? id local-eval? eval-next-var)
      ;; mostly similar to ref-out.
      (let ((scm-id (get/assign-scm-id! this)))
	 (cond
	    ;; don't even try to do fancy stuff, when we are in an eval. just
	    ;; access the eval object
	    ((and (thread-parameter 'eval?)
		  global?)
	     (let ((v (gensym 'v))
		   (o (gensym 'o))
		   (str-id (add-str! (symbol->string id))))
		`(let ((,o (env-object ,(thread-parameter 'eval-env) ,str-id)))
		    (unless (Js-Activation-Object? ,o)
		       (set! ,base ,o))
		    (lambda () (js-property-get ,o ,str-id)))))
	    (local-eval?
	     ;; stupid thing: evals might delete local variables. If there's a
	     ;; local eval, we need to verify first, if the variable actually
	     ;; still exists.
	     `(if (not (js-deleted? ,scm-id))
		  (lambda () ,scm-id)
		  ,(access+base eval-next-var base)))
	    (global?
	     `(begin
		 ;; first read var. this will throw an error if the var is
		 ;; not declared.
		 (global-read ,scm-id)
		 ;; but then return a lambda that will reread again.
		 (lambda ()
		    (if (global-declared? ,scm-id)
			(global-read ,scm-id)
			(js-undefined)))))
	    (else
	     `(lambda () ,scm-id))))))

(define-method (access+base this::Intercepted-Var base)
   (with-access::Intercepted-Var this (id obj-id intercepted)
      (let ((str-id (add-str! (symbol->string id))))
	 `(if (js-property-contains ,obj-id ,str-id)
	      (begin
		 (set! ,base ,obj-id)
		 (lambda () (js-property-get ,obj-id ,str-id)))
	      ,(access+base intercepted base)))))

(define-method (access+base this::This-Var base) '(lambda () this))

(define-method (access+base this::Imported-Var base)
   (with-access::Imported-Var this (id)
      (error "scm-out"
	     "Internal Error. Encountered imported var"
	     id)))



(define-generic (compiled-id this::Var)
   (get/assign-scm-id! this))

(define-method (compiled-id this::Intercepted-Var)
   (with-access::Intercepted-Var this (id)
      (error "scm-out"
	     "Intercepted-vars don't have any id. (internal error)"
	     id)))

(define-method (compiled-id this::This-Var)
   'this)

(define-method (compiled-id this::Imported-Var)
   (with-access::Imported-Var this (id)
      (error "scm-out"
	     "Encountered imported var"
	     id)))

(define (label-out this::Label)
   (with-access::Label this (generated)
      (unless generated
	 (set! generated (gensym 'label-id)))
      generated))

(define-generic (out this::Node)
   (error "Node-out"
	  "Internal Error. Forgot node type: "
	  (class-name (object-class this))))

(define-method (out this::Program)
   (with-access::Program this (declared-globals implicit-globals body
						function-str-ids-ht)
      (strs-reset!)
      (regexps-reset!)
      (let* ((tlo (thread-parameter 'top-level-object))
	     (in-eval? (thread-parameter 'eval?))
	     (tl-this (thread-parameter 'top-level-this))
	     (declared-globals-w/o-this
	      (filter (lambda (var)
			 (not (This-Var? var)))
		      declared-globals))
	     (compiled-body (out body)))
	 (if (not in-eval?)
	     `(begin
		 ;; declare strings
		 ;; create 'this'
		 ;; declare global variables (declared and implicit)
		 ;; declare regexp-variables
		 ;; create fun-strings (when configured this way)
		 ;; create 'init-declared', 'init-implicit', 'init-regexps' and
		 ;;      'run-top-level
		 ;; if not in module execute these funs. (otherwise somebody
		 ;;   else needs to invoke them.)
		 ,@(map (lambda (p)
			   `(define ,(cdr p)
			       ,(utf8->js-string-literal (car p) #t)))
			(strs-alist))
		 (define this ,tl-this)
		 ,@(map (lambda (var)
			   `(define ,(compiled-id var) (js-undefined)))
			declared-globals-w/o-this)
		 ,@(map (lambda (decl)
			   (with-access::Decl decl (var)
			      `(define ,(compiled-id var) (js-undefined))))
			implicit-globals)
		 ,@(map (lambda (l)
			   `(define ,(car l) (js-null)))
			(regexps-alist))
		 ,@(if (config 'function-strings)
		       (hashtable-map function-str-ids-ht
				      (lambda (str-id str)
					 `(define ,str-id
					     (utf8->js-string-literal ,str))))
		       '())
		 (define (js-init-declared)
		    #unspecified ;; so the fun is never empty
		    ,@(map (lambda (var)
			      `(set! ,(compiled-id var)
				     (create-declared-global
				      ,(utf8->js-string-literal
					(symbol->string (Var-id var))))))
			   declared-globals-w/o-this))
		 (define (js-init-implicit)
		    #unspecified ;; so the fun is never empty
		    ,@(map (lambda (decl)
			      (let* ((var (Decl-var decl))
				     (id (Var-id var)))
				 `(set! ,(compiled-id var)
					(create-implicit-global
					 ,(utf8->js-string-literal
					   (symbol->string id))))))
			   implicit-globals))
		 (define (js-init-regexps)
		    #unspecified ;; so the fun is never empty
		    ,@(map (lambda (l)
			      (let ((id (car l))
				    (pattern (cadr l))
				    (flags (caddr l)))
				 `(set! ,id (regexp-literal ,pattern ,flags))))
			   (regexps-alist)))
		 (define (js-run-top-level)
		    ,compiled-body)
		 ,@(if (config 'module)
		       '()
		       '((js-init-declared)
			 (js-init-implicit)
			 (js-init-regexps)
			 (js-run-top-level))))

	     ;; eval does not do anything for implicit vars.
	     ;; declared globals: if the var is a declared function, then it
	     ;; must replace the original entry (including the attributes).
	     ;; otherwise just make sure there is an entry.
	     `(let* ((this ,tl-this)
		     ,@(map (lambda (p)
			       `(,(cdr p) ,(utf8->js-string-literal (car p))))
			    (strs-alist))
		     ,@(map (lambda (l)
			       `(,(car l) (regexp-literal ,(cadr l)
							  ,(caddr l))))
			    (regexps-alist))
		     ,@(if (config 'function-strings)
			   (hashtable-map
			    function-str-ids-ht
			    (lambda (id str)
			       `(,id (utf8->js-string-literal ,str))))
			   '()))
		 ,@(map (lambda (var)
			   (let ((id-str (symbol->string (Var-id var)))
				 (str-id (symbol-append 'jsstr- (gensym))))
			      (if (Var-fun? var)
				  ;; declared fun.
				  ;; always replace existing entry.
				  ;; initially set to undefined, but that
				  ;; changes soon.
				  ;; we need to use generic-set, otherwise
				  ;; we can't overwrite existing entries, if
				  ;; they have different attributes.
				  `(js-property-generic-set!
				    ,tlo
				    ,(utf8->js-string-literal id-str)
				    (js-undefined)
				    (default-attributes))
				  `(let ((,str-id
					  ,(utf8->js-string-literal id-str)))
				      (unless (js-property-one-level-contains?
					       ,tlo ,str-id)
					 (js-property-set! ,tlo ,str-id
							   (js-undefined)))))))
			declared-globals-w/o-this)
	      ,compiled-body)))))

(define-method (out this::Begin)
   (with-access::Begin this (els)
      `(begin
	  ,@(map out els))))

(define-method (out this::Let*)
   (with-access::Let* this (vassigs body)
      `(let* (,@(map (lambda (vassig)
			(with-access::Vassig vassig (lhs val)
			   (with-access::Ref lhs (var)
			      `(,(compiled-id var) ,(out val)))))
		     vassigs))
	  ,(out body))))

(define-method (out this::Ref)
   (with-access::Ref this (var)
      (ref-out var)))

(define-method (out this::NOP)
   '(js-undefined))

(define-method (out this::If)
   (with-access::If this (test then else)
      `(if (js-boolify ,(out test))
	   ,(out then)
	   ,(out else))))

(define-method (out this::While)
   (with-access::While this (test body)
      (let ((loop (gensym 'loop)))
	 `(let ,loop ()
	       (if (js-boolify ,(out test))
		   (begin
		      ,(out body)
		      (,loop)))))))

(define-method (out this::Do)
   (with-access::Do this (body test)
      (let ((loop (gensym 'loop)))
	 `(let ,loop (())
	       ,(out body)
	       (if (js-boolify ,(out test))
		   (,loop))))))

(define-method (out this::For-In)
   (with-access::For-In this (obj lhs body)
      (let ((prop (gensym 'prop))
	    (val (gensym 'val))
	    (read-only? (gensym 'ro))
	    (deletable? (gensym 'del))
	    (enumerable? (gensym 'enum)))
	 `(js-property-for-each
	   (jsop-any->object ,(out obj))
	   (lambda (,prop ,val ,read-only? ,deletable? ,enumerable?)
	      (when ,enumerable?
		 ,(set!-out (Ref-var lhs) prop)
		 ,(out body)))))))

(define-method (out this::With)
   (with-access::With this (obj-id obj body)
      ;; the obj is not yet transformed to an object.
      `(let ((,obj-id (jsop-any->object ,(out obj))))
	  ,(out body))))

(define (make-clause default-clause? body-id expr body)
   (list default-clause? body-id expr body))

(define clause-default-clause? car)
(define clause-body-id cadr)
(define clause-expr caddr) ;; ignored in default-clause
(define clause-body cadddr)

(define-method (out this::Switch)
   (define (rev-bind-clauses compiled-clauses)
      (let loop ((clauses compiled-clauses)
		 (rev-result '()))
	 (cond
	    ((null? clauses) ;; should not happen
	     '())
	    ((null? (cdr clauses))
	     (cons (list (clause-body-id (car clauses))
			 `(lambda ()
			     (let ((fall-through (lambda () (js-undefined))))
				,(clause-body (car clauses)))))
		   rev-result))
	    (else
	     (loop (cdr clauses)
		   (cons (list (clause-body-id (car clauses))
			       `(lambda ()
				   (let ((fall-through ,(clause-body-id (cadr clauses))))
				      ,(clause-body (car clauses)))))
			 rev-result))))))
   
   (with-access::Switch this (key cases)
      (let* ((key (gensym 'key))
	     (compiled-key (out key))
	     (compiled-clauses (map out cases))
	     (default-body-id (let loop ((clauses compiled-clauses))
				 (cond
				    ((null? clauses) #f)
				    ((clause-default-clause? (car clauses))
				     (clause-body-id (car clauses)))
				    (else
				     (loop (cdr clauses))))))
	     (rev-clauses-bindings (rev-bind-clauses compiled-clauses)))
	 `(let* (,@(cons (list key compiled-key)
			 rev-clauses-bindings))
	     (cond
		,@(map (lambda (clause)
			  (if (clause-default-clause? clause)
			      `(#f 'default-clause-was-here)
			      `((jsop-=== ,(clause-expr clause) ,key)
				(,(clause-body-id clause)))))
		       compiled-clauses)
		,@(if default-body-id
		      `((else (,default-body-id)))
		      '()))))))

(define-method (out this::Fall-Through)
   '(fall-through))

(define-method (out this::Case)
   (with-access::Case this (expr body)
      (make-clause #f
		   (gensym 'clause-body)
		   (out expr)
		   (out body))))

(define-method (out this::Default)
   (with-access::Default this (body)
      (make-clause #t
		   (gensym 'default-body)
		   'ignored
		   (out body))))

(define-method (out this::Throw)
   (with-access::Throw this (expr)
      `(raise ,(out expr))))

(define-method (out this::Try)
   (with-access::Try this (body catch finally)
      ;; trampoline-style
      ;; with-handlers are executed before the surrounding unwind-protects.
      ;; -> wrap the result into a lambda.
      ;;    same goes for catch-clause.
      ;; the catch-clause is then executed after the body and all
      ;; unwind-protects.
      (let* ((tmp (gensym 'tmp))
	     (inner (if catch
			`((with-handler
			     ;; catch must return a trampoline
			     ,(out catch)
			     (let ((,tmp ,(out body)))
				(lambda ()
				   ,tmp))))
			(out body))))
	 (if finally
	     `(unwind-protect
		 ,inner
		 ,(out finally))
	     inner))))

(define-method (out this::Catch)
   (with-access::Catch this (decl obj-id body)
      (let* ((decl-var (Decl-var decl))
	     (exc-scm-id (compiled-id decl-var))
	     (exc-js-str (add-str! (symbol->string (Var-id decl-var))))
	     (compiled-body (out body)))
	 ;; the exc-scm-id is assigned in the Try-clause.
	 `(lambda (,exc-scm-id)
	     (lambda ()
		(let* ((,exc-scm-id (error->js-exception ,exc-scm-id))
		       (,obj-id (js-create-scope-object (new-Object))))
		   ;; 12.14
		   (scope-var-add ,obj-id
				  ,exc-js-str
				  ,exc-scm-id
				  (get-Attributes dont-delete))
		   ,compiled-body))))))

(define-method (out this::Named-Fun)
   (with-access::Named-Fun this (decl obj-id body)
      (let* ((decl-var (Decl-var decl))
	     (fun-scm-id (compiled-id decl-var))
	     (fun-js-str (add-str! (symbol->string (Var-id decl-var))))
	     (compiled-body (out body)))
	 `(let ((,obj-id (js-create-scope-object (new-Object))))
	     (letrec ((,fun-scm-id ,compiled-body))
		;; 13
		(scope-var-add ,obj-id
			       ,fun-js-str
			       ,fun-scm-id
			       (get-Attributes dont-delete read-only))
		,fun-scm-id)))))

(define-method (out this::Bind-Exit)
   (with-access::Bind-Exit this (label body)
      `(bind-exit (,(label-out label))
	  ,(out body))))

(define-method (out this::Bind-Exit-Invoc)
   (with-access::Bind-Exit-Invoc this (label expr)
      `(,(label-out label) ,(out expr))))

(define *nb-named-params* 3)
(define-method (out this::Fun)
   (with-access::Fun this (body this-decl arguments-decl params locals-table
				eval-obj-id eval? str)
      (define (add-eval-object body)
	 (let* ((fun-vars (filter! (lambda (var)
				      (not (This-Var? var)))
				   (hashtable->list locals-table)))
		(local-vars (filter (lambda (var)
				       (with-access::Var var (param?)
					  (not param?)))
				    fun-vars)))
	    `(let ((,eval-obj-id (js-create-activation-object))
		   ,@(map (lambda (var)
			     `(,(compiled-id var) (js-undefined)))
			  local-vars))
		,@(map (lambda (var)
			  `(scope-var-add ,eval-obj-id
					  ,(add-str! (symbol->string
						      (Var-id var)))
					  ,(compiled-id var)
					  (declared-attributes)))
		       fun-vars)
		,body)))


      ;; when adding conditional for arguments-var don't forget, that arguments
      ;; var might be added into eval-obj.
      ;; in theory only for function with local-eval, but as this one can be
      ;; deleted, the next one needs to expose its arguments object too. As we
      ;; have (not yet) any means to detect that, we have to add the arguments
      ;; object for all functions with '.eval?'.
      (let ((compiled-arguments (if (Var-arguments-used? (Decl-var
							  arguments-decl))
				    (compiled-id (Decl-var arguments-decl))
				    #f))
	    (compiled-this (compiled-id (Decl-var this-decl)))
	    (compiled-params (map (lambda (decl)
				     (with-access::Decl decl (var)
					(compiled-id var)))
				  params))
	    (compiled-body (if eval?
			       (add-eval-object (out body))
			       (out body))))
	 `(js-fun ,compiled-this
		  #f ;; no this-fun (only accessible through arguments)
		  ,compiled-arguments
		  ,(if (config 'function-strings)
		       `(list ,@str) ;; something like fun-str-id, from, to.
		       `(add-str! ,(symbol->string (gensym "function"))))
		  (,@compiled-params)
		  ,compiled-body))))

(define-method (out this::Vassig)
   (with-access::Vassig this (lhs val)
      (set!-out (Ref-var lhs) (out val))))

(define-method (out this::Fun-Binding)
   (with-access::Fun-Binding this (lhs val)
      (if (and (thread-parameter 'eval?)
	       (Var-global? (Ref-var lhs)))
	  ;; assign fun-binding to top-level object and not to current
	  ;; environment. (Rhino 1.7 release 1 Pre 2007 11 25 has this bug)
	  `(js-property-set! ,(thread-parameter 'top-level-object)
			     ,(add-str! (symbol->string
					 (Var-id (Ref-var lhs))))
			     ,(out val))
	  (fun-binding-set! (Ref-var lhs) (out val)))))

(define-method (out this::Accsig)
   (with-access::Accsig this (lhs val)
      (with-access::Access lhs (obj field)
	 (let* ((tmp-o (gensym 'tmp-o))
		(tmp-field (gensym 'tmp-field))
		(tmp-object-o (gensym 'tmp-object-o))
		(tmp-string-field (gensym 'tmp-string-field))
		(traversed-obj (out obj))
		(traversed-obj-object (comp:any->js-object traversed-obj))
		(traversed-field (out field))
		(traversed-field-string (comp:any->js-string traversed-field))
		(traversed-val (out val)))
	    ;; we need all these tmp-variables, to ensure the correct order of
	    ;; evaluation.
	    `(let* ((,tmp-o ,(if traversed-obj-object
				 #unspecified
				 traversed-obj))
		    (,tmp-field ,(if traversed-field-string
				     #unspecified
				     traversed-field))
		    (,tmp-object-o ,(or traversed-obj-object
					`(jsop-any->object ,tmp-o)))
		    (,tmp-string-field ,(or traversed-field-string
					    `(any->js-string ,tmp-field))))
		(js-property-set! ,tmp-object-o
				  ,tmp-string-field
				  ,traversed-val))))))

(define (Operator-out this)
   (let* ((op (Call-op this))
	  (args (Call-args this))
	  (var (Ref-var op))
	  (op-id (Var-id var)))
      (cond
	 ((and (eq? op-id 'typeof)
	       (not (null? args))
	       (null? (cdr args))
	       (Ref? (car args)))
	  ;; we need to use .typeof to avoid undeclared error
	  `(,(compiled-id (Ref-var op)) ,(typeof-out (Ref-var (car args)))))
	 ((or (eq? op-id '&&)
	      (eq? op-id 'OR))
	  ;; not really operator calls, but macros.
	  ;; do not add the 'let'.
	  `(,(compiled-id var) ;; operator call.
	    ,@(map out args)))
	 ((and (eq? op-id 'unary--)
	       (Number? (car args)))
	  `(negfl ,(out (car args))))
	 ((and (eq? op-id 'unary-+)
	       (Number? (car args)))
	  (out (car args)))
	 (else
	  (let* ((compiled-args (map out args))
		 (tmp-ids (map (lambda (arg) (gensym 'tmp))
			       compiled-args))
		 (bindings (map (lambda (tmp-id arg)
				   (list tmp-id arg))
				tmp-ids
				compiled-args)))
	     `(let* ,bindings
		 (,(compiled-id var) ;; operator call.
		  ,@tmp-ids)))))))

;; we assume:
;;  - args expressions that need to be put into a let for explicit left to
;;   right evaluation.
(define (generate-call fun base args)
   (if (null? args)
       `(js-call ,fun ,base)
       (let ((arg-names (map (lambda (ign) (gensym 'arg)) args)))
	  `(let* ,(map list arg-names args)
	      (js-call ,fun ,base ,@arg-names)))))

(define-method (out this::Call)
   (with-access::Call this (op args)
      ;; 11.2.3
      (cond
	 ((and (Ref? op)
	       (Runtime-Var? (Ref-var op))
	       (Var-operator? (Ref-var op)))
	  (Operator-out this))
	 ((Ref? op)
	  (let ((base (gensym 'base))
		(f (gensym 'f)))
	     `(let* ((,base *js-global-this*)
		     ;; if the variable has a base, it will update the
		     ;; base-variable (during runtime). we then perform
		     ;; (implicitely) a method-call.
		     (,f ,(access+base (Ref-var op) base)))
		 ,(generate-call `(,f)
				 base ;; might have been updated by access+base.
				 (map out args)))))
	 (else
	  (let ((f (gensym 'f)))
	     `(let ((,f ,(out op)))
		 ,(generate-call f #f (map out args))))))))

; (define-pmethod (Binary-out)
;    `(,(this.op.traverse)
;      ,@(map-node-compile this.args)))

(define-method (out this::Delete-Call)
   (with-access::Delete-Call this (args)
      (delete-out (car args))))

(define-method (out this::Delete-Property-Call)
   (with-access::Delete-Property-Call this (args)
      `(jsop-delete ,(out (car args))
		    ,(out (cadr args)))))

(define-method (out this::Method-Call)
   (with-access::Method-Call this (op args)
      (with-access::Access op (obj field)
	 (let* ((tmp-o (gensym 'o))
		(tmp-o-object (gensym 'o-object))
		(tmp-this (gensym 'this))
		(tmp-field (gensym 'field))
		(tmp-field-string (gensym 'field-string))
		(traversed-o (out obj))
		(traversed-o-object (comp:any->js-object traversed-o))
		(traversed-field (out field))
		(traversed-field-string (comp:any->js-string traversed-field)))
	    ;; we need all these tmp-variables, to ensure the correct order of
	    ;; evaluation.
	    `(let* ((,tmp-o ,(if traversed-o-object #unspecified traversed-o))
		    (,tmp-field ,(if traversed-field-string
				     #unspecified
				     traversed-field))
		    (,tmp-this ,(or traversed-o-object
				    `(any->object ,tmp-o)))
		    (,tmp-o-object (safe-js-object ,tmp-this))
		    (,tmp-field-string ,(or traversed-field-string
					    `(any->js-string ,tmp-field))))
		,(generate-call `(js-property-get ,tmp-o-object
						  ,tmp-field-string)
				tmp-this
				(map out args)))))))

(define-method (out this::Eval-Call)
   (let* ((t (gensym 'tmp))
	  (op (Eval-Call-op this))
	  (eval-id (Eval-Call-eval-scm-id this))
	  (tlo (Eval-Call-top-level-object this))
	  (tlo-id/obj (if (Var? tlo)
			  (compiled-id tlo)
			  tlo))
	  (next-env (thread-parameter 'eval-env))
	  (env-vars (Eval-Call-env-vars this))
	  (args (Eval-Call-args this)))
      `(let ((,t ,(out op)))
	  (if (eq? ,t (js-eval-orig))
	      ;; we have a real eval here
	      ,(if (null? args)
		   `(js-undefined)
		   `(js-eval ,(out (car args))
			     ,tlo-id/obj this ,next-env
			     ,@env-vars))
	      (js-call ,t #f ,@(map out args))))))

(define-method (out this::New)
   (with-access::New this (class args)
      (let ((traversed-class (out class))
	    (traversed-args (map out args)))
	 (if (and (null? args)
		  (symbol? traversed-class)
		  (eq? traversed-class (id->runtime-var 'Object)))
	     '(new-Object)
	     (let ((f-id (gensym 'f))
		   (arg-ids (map (lambda (x) (gensym 'arg)) traversed-args)))
		`(let* ((,f-id ,traversed-class)
			,@(map list arg-ids traversed-args))
		    (js-new ,f-id ,@arg-ids)))))))

(define-method (out this::Access)
   (with-access::Access this (obj field)
      (let* ((tmp-o (gensym 'tmp-o))
	     (tmp-field (gensym 'tmp-field))
	     (tmp-object-o (gensym 'tmp-object-o))
	     (tmp-string-field (gensym 'tmp-string-field))
	     (traversed-o (out obj))
	     (traversed-o-object (comp:any->js-object traversed-o))
	     (traversed-field (out field))
	     (traversed-field-string (comp:any->js-string traversed-field)))
	 ;; we need all these tmp-variables, to ensure the correct order of
	 ;; evaluation.
	 `(let* ((,tmp-o ,(if traversed-o-object #unspecified traversed-o))
		 (,tmp-field ,(if traversed-field-string
				  #unspecified
				  traversed-field))
		 (,tmp-object-o ,(if traversed-o-object
				     `(safe-js-object ,traversed-o-object)
				     `(jsop-any->object ,tmp-o)))
		 (,tmp-string-field ,(or traversed-field-string
					 `(any->js-string ,tmp-field))))
	     (js-property-get ,tmp-object-o ,tmp-string-field)))))

(define-method (out this::This)
   'this)

(define-method (out this::Literal)
   (with-access::Literal this (val)
      (error "Literal-out"
	     "Internal Error. Forgot literal type"
	     val)))

(define-method (out this::Undefined)
   '(js-undefined))

(define-method (out this::Null)
   '(js-null))

(define-method (out this::Bool)
   (with-access::Bool this (val)
      (if val #t #f)))

(define-method (out this::Number)
   (with-access::Number this (val)
      (let* ((str val)
	     (hex? (or (string-prefix? "0x" str)
		       (string-prefix? "0X" str)))
	     (nb (if hex?
		     (bignum->flonum
		      (string->bignum
		       (substring str 2 (string-length str)) 16))
		     (string->real str))))
	 (cond
	    ((flonum? nb)
	     nb)
	    (else
	     (error "Number-out"
		    "could not transform to number:"
		    str))))))

(define (unescape str)
   (define (char-hex? c)
      (or (char-numeric? c)
	  (case c
	     ((#\a #\b #\c #\d #\e #\f) #t)
	     ((#\A #\B #\C #\D #\E #\F) #t)
	     (else #f))))
   (let ((char-l (string->list str)))
      (let loop ((char-l char-l)
		 (rev-res '()))
	 (if (null? char-l)
	     (list->string (reverse rev-res))
	     (cond
		((and (char=? #\\ (car char-l))
		      (char=? #\0 (cadr char-l)))
		 ;; only if the following char is not
		 ;; another digit count it as null-char.
		 ;; was initially octal escape
		 ;; sequence. (now deprecated)
		 (if (or (null? (cddr char-l))
			 (not (char-numeric? (caddr char-l))))
		     (loop (cddr char-l)
			   (cons #\null rev-res))
		     (loop (cddr char-l)
			   (cons* (cadr char-l)
				  (car char-l)
				  rev-res))))
		((and (char=? #\\ (car char-l))
		      (not (null? (cddr char-l)))
		      (not (null? (cdddr char-l)))
		      (char-hex? (caddr char-l))
		      (char-hex? (cadddr char-l))
		      (let ((char-l+4 (cddddr char-l)))
			 (or (char=? #\x (cadr char-l))
			     (and (char=? #\u (cadr char-l))
				  (not (null? char-l+4))
				  (not (null? (cdr char-l+4)))
				  (char-hex? (car char-l+4))
				  (char-hex? (cadr char-l+4))))))
		 ;; we can't really handle unicode chars now (depends on the
		 ;; configuration (utf8, utf16, etc). so we leave them
		 ;; in the string.
		 (loop (cddr char-l)
		       (cons* (cadr char-l)
			      (car char-l)
			      rev-res)))
		((char=? #\\ (car char-l))
		 (loop (cddr char-l)
		       ;; 7.8.4
		       (cons (case (cadr char-l)
				;; SingleEscapeCharacters vvv
				((#\b) #a008)
				((#\t) #\tab)
				((#\n) #\newline)
				((#\v) #a011)
				((#\f) #a012)
				((#\r) #\return)
				;; the else-part covers ', " and \
				;; SingleEscapeCharacters ^^^
				(else (cadr char-l)))
			     rev-res)))
		(else
		 (loop (cdr char-l)
		       (cons (car char-l) rev-res))))))))

(define (unescaped-minus-quotes s)
   ;; 7.8.4
   (unescape (substring s 1 (-fx (string-length s) 1))))

(define-method (out this::String)
   (with-access::String this (val)
      (add-str! (unescaped-minus-quotes val))))

(define-method (out this::Array)
   (with-access::Array this (length els)
      `(js-array-literal
	,length
	,(list 'quasiquote (map out els)))))

(define-method (out this::Array-Element)
   (with-access::Array-Element this (index expr)
      (list index
	    (list 'unquote (out expr)))))

(define-method (out this::Obj-Init)
   (with-access::Obj-Init this (props)
      `(natO-object-literal
	,(list 'quasiquote (map out props)))))

(define-method (out this::Property-Init)
   (with-access::Property-Init this (name val)
      `(,(list 'unquote (comp:any->js-string (out name)))
	,(list 'unquote (out val)))))

(define-method (out this::Reg-Exp)
   (let* ((pattern/flags (Reg-Exp-pattern this))
	  (last-/ (string-index-right pattern/flags #\/))
	  (pattern (substring pattern/flags 1 last-/))
	  (flags (substring pattern/flags
			    (+ last-/ 1)
			    (string-length pattern/flags))))
      (add-regexp! (add-str! pattern) (add-str! flags))))
