(module js-obfuscator
   (include "protobject.sch")
   (include "nodes.sch")
   (option (loadq "protobject-eval.sch"))
   (import parser
	   config
	   html
	   protobject
	   nodes
	   var
	   obfuscate-ids
	   fun-bindings
	   symbol
	   simplify
	   verbose
	   js-out)
   (main js-obfuscator))

(define *in-file* #f)
(define *out-file* #f)

(define *js-globals* '(Math String Number Object RegExp Function Array Date
		       Error Boolean))

(define *js-properties* '(apply))

(define *pre-mapping* `(,*js-globals*))

(define *obfuscation-mapping-file* #f)

(define *exclude-html?* #f)

(define *version* "20060606")
(define (handle-args args)
   (args-parse (cdr args)
      (section "Help")
      (("?")
       (args-parse-usage #f))
      ((("-h" "--help") (help "?,-h,--help" "This help message"))
       (args-parse-usage #f))
      (section "Misc")
      (("-o" ?file (help "The output file. '-' prints to stdout"))
       (set! *out-file* file))
      (("-i" ?file (help "The input file. '-' reads from stdin"))
       (set! *in-file* file))
      ((("--version") (help "Version number"))
       (print *version*))
      ((("-v" "--verbose") (help "Verbose output"))
       (config-set! 'verbose #t))
      (("--obfuscate-globals" (help "Obfuscate globals too"))
       (set! *obfuscate-globals?* #t))
      (("--obfuscate-properties" (help "Obfuscate properties too"))
       (set! *obfuscate-properties?* #t))
      ((("-c" "--compress") (help "Compress output"))
       (config-set! 'compress? #t))
      ((("--html") (help "Include HTML global variables"))
       (set! *exclude-html?* #t))
      (("--imported" ?file
	(help "file containing imported variables/properties and their mapping"))
       (set! *pre-mapping* (cons file *pre-mapping*)))
      (("--obfuscation-mapping"
	?file
	(help "output obfuscation mapping to file"))
       (set! *obfuscation-mapping-file* file))
      (else
       (error "handle-args"
	      "unknown argument: "
	      else))))

;; obfuscation is either a string, a symbol or a mapping.
;; If it is a string/symbol, then it maps to itself.
;; Properties must not have their surrounding quotes.
(define (add-pre-obfuscation! obfuscation)
   (cond
      ((symbol? obfuscation)
       (let ((str (symbol->string obfuscation)))
	  (add-obfuscation-mapping! str str)))
      ((string? obfuscation)
       (add-obfuscation-mapping! obfuscation obfuscation))
      ((pair? obfuscation)
       (let* ((from (car obfuscation))
	      (to (cadr obfuscation))
	      (from-str (if (symbol? from)
			    (symbol->string from)
			    from))
	      (to-str (if (symbol? to)
			  (symbol->string to)
			  to)))
	  (add-obfuscation-mapping! from to)))
      (else
       (error 'js-obfuscator
	      "Couldn't parse obfuscation-mapping"
	      obfuscation))))

(define (js-obfuscator args)
   (config-init!)
   (nodes-init!)
   (var-nodes-init!)
   
   (handle-args args)
   (when (not *in-file*)
      (error "js-obfuscator"
	     "missing input-file. Use --help to see usage."
	     #f))
   (when (not *out-file*)
      (error "js-obfuscator"
	     "missing output-file. Use --help to see usage."
	     #f))

   (when *exclude-html?*
      (set! *pre-mapping* (cons *HTML-globals* *pre-mapping*)) ;; small hack.
      (when *obfuscate-properties?*
	 (set! *pre-mapping* (cons *HTML-properties* *pre-mapping*))))

   (for-each (lambda (f)
		(let ((l (if (string? f)
			     (begin
				(verbose "reading imported variables/properties of file " f)
				(with-input-from-file f read))
			     f)))
		   (for-each (lambda (obfuscation)
				(add-pre-obfuscation! obfuscation))
			     l)))
	     *pre-mapping*)
   (let* ((in-p (if (string=? *in-file* "-")
		    (current-input-port)
		    (open-input-file *in-file*)))
	  (out-p (if (string=? *out-file* "-")
		     (current-output-port)
		     (open-output-file *out-file*))))
      (when (not in-p)
	 (error 'obfuscator
		"Could not open file"
		*in-file*))
      (when (not out-p)
	 (error 'obfuscator
		"Could not open file"
		*out-file*))
      (let ((ast (parse in-p)))
	 (when *obfuscation-mapping-file*
	     (set! *obfuscation-mapping-p*
		   (open-output-file *obfuscation-mapping-file*)))
	 (fun-bindings! ast)
	 (symbol-resolution! ast '()) ;*imported-global-mapping*)
	 (set! *integrate-Var-decl-lists* #f) ;; HACK.
	 (simplify! ast)
	 (obfuscate-ids! ast)
	 ;      (with-output-to-port out-p
	 ;	 (lambda () (pobject-dot-out ast)))
	 (js-out ast out-p)
	 (when *obfuscation-mapping-file*
	    (close-output-port *obfuscation-mapping-p*))
	 (when (not (string=? *in-file* "-"))
	    (close-input-port in-p))
	 (when (not (string=? *out-file* "-"))
	    (close-output-port out-p)))))
