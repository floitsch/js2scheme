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
(define *pre-property-mapping* `(,*js-properties*))

(define *obfuscation-mapping-file* #f)
(define *property-mapping-file* #f)

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
       (set! *obfuscate-globals* #t))
      (("--obfuscate-properties" (help "Obfuscate properties too"))
       (if (eq? *non-obfuscated-properties* #t)
	   (set! *non-obfuscated-properties* '())))
      ((("-c" "--compress") (help "Compress output"))
       (config-set! 'compress? #t))
      ((("--html") (help "Include HTML global variables"))
       (set! *pre-mapping* (cons *HTML-globals* *pre-mapping*)) ;; small hack.
       (set! *pre-property-mapping*
	     (cons *HTML-properties* *pre-property-mapping*)))
      (("--imported" ?file
	(help "file containing imported variables and their mapping"))
       (set! *pre-mapping* (cons file *pre-mapping*)))
      (("--imported-properties" ?file
	(help "file containing properties and their mapping"))
       (set! *pre-property-mapping* (cons file *pre-property-mapping*)))
      (("--obfuscation-mapping"
	?file
	(help "output obfuscation mapping to file"))
       (set! *obfuscation-mapping-file* file))
      (("--property-mapping"
	?file
	(help "output property obfuscation mapping to file"))
       (set! *property-mapping-file* file))
      (else
       (error "handle-args"
	      "unknown argument: "
	      else))))

(define (any->property-mapping any)
   (cond
      ((symbol? any)
       (any->property-mapping (symbol->string any)))
      ((and (string? any)
	    (not (or (char=? #\' (string-ref any 0))
		     (char=? #\" (string-ref any 0)))))
       (let ((str1 (string-append "\"" any "\""))
	     (str2 (string-append "'" any "'")))
	  (list `(,str1 ,str1)
		`(,str2 ,str2))))
      ((string? any)
       (list `(,any ,any)))
      ((pair? any)
       (let ((from (car any))
	     (to (cadr any)))
	  (let ((to-with-quotes (if (or (char=? #\' (string-ref to 0))
					(char=? #\" (string-ref to 0)))
				    to
				    (string-append "'" to "'"))))
	     (if (not (or (char=? #\' (string-ref from 0))
			  (char=? #\" (string-ref to 0))))
		 (let ((str1 (string-append "\"" from "\""))
		       (str2 (string-append "'" from "'")))
		    (list `(,str1 ,to-with-quotes)
			  `(,str2 ,to-with-quotes)))
		 (list `(,from ,to-with-quotes))))))
      (else
       (error 'js-obfuscator
	      "Couldn't parse property-mapping"
	      any))))

(define (js-obfuscator args)
   (config-init!)
   (nodes-init!)
   (var-nodes-init!)
   
   (handle-args args)
   (if (not *in-file*)
       (begin
	  (error "js-obfuscator"
		 "missing input-file. Use --help to see usage."
		 #f)))
   (if (not *out-file*)
       (error "js-obfuscator"
	      "missing output-file. Use --help to see usage."
	      #f))
   (for-each (lambda (f)
		(let ((l (if (string? f)
			     (begin
				(verbose "reading imported variables of file " f)
				(with-input-from-file f read))
			     f)))
		   (set! *imported-global-mapping*
			 (append! (map! (lambda (id/p)
					   (if (pair? id/p)
					       id/p
					       (list id/p id/p)))
					l)
				  *imported-global-mapping*))))
	     *pre-mapping*)
   (for-each (lambda (f)
		(let ((l (if (string? f)
			     (begin
				(verbose "reading imported property-mappings of file " f)
				(with-input-from-file f read))
			     f)))
		   (set! *property-mapping*
			 (append! (apply append (map! any->property-mapping l))
				  *property-mapping*))))
	     *pre-property-mapping*)
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
	 (when *property-mapping-file*
	     (set! *property-mapping-p*
		   (open-output-file *property-mapping-file*)))
	 (fun-bindings! ast)
	 (symbol-resolution! ast *imported-global-mapping*)
	 (set! *integrate-Var-decl-lists* #f) ;; HACK.
	 (simplify! ast)
	 (obfuscate-ids! ast)
	 ;      (with-output-to-port out-p
	 ;	 (lambda () (pobject-dot-out ast)))
	 (js-out ast out-p)
	 (when *obfuscation-mapping-file*
	    (close-output-port *obfuscation-mapping-p*))
	 (when *property-mapping-file*
	    (close-output-port *property-mapping-p*))
	 (when (not (string=? *in-file* "-"))
	    (close-input-port in-p))
	 (when (not (string=? *out-file* "-"))
	    (close-output-port out-p)))))
