(module js-pp
   (include "protobject.sch")
   (include "nodes.sch")
   (option (loadq "protobject-eval.sch"))
   (import parser
	   config
	   protobject
	   nodes
	   var
	   statements
	   verbose
	   js-out)
   (main js-pp))

(define *in-file* #f)
(define *out-file* #f)

(define *version* "20060606")
(define (handle-args args)
   (args-parse (cdr args)
      (section "Help")
      (("?")
       (args-parse-usage #f))
      ((("-h" "--help") (help "?,-h,--help" "This help message"))
       (args-parse-usage #f))
      (section "Misc")
      ((("--version") (help "Version number"))
       (print *version*))
      ((("-v" "--verbose") (help "Verbose output"))
       (config-set! 'verbose #t))
      (("-o" ?file (help "The output file. '-' prints to stdout."))
       (set! *out-file* file))
      (("-i" ?file (help "The input file. '-' reads from stdin."))
       (set! *in-file* file))
      ((("-c" "--compress") (help "Compress output"))
       (config-set! 'compress? #t))
      (else
       (error "handle-args"
	      "unknown argument: "
	      else))))

(define (js-pp args)
   (config-init!)
   (nodes-init!)
   (var-nodes-init!)
   (handle-args args)
   (if (not *in-file*)
       (begin
	  (error "js-pp"
		 "missing input-file. Use --help to see usage."
		 #f)))
   (if (not *out-file*)
       (error "js-pp"
	      "missing output-file. Use --help to see usage."
	      #f))
   (let* ((in-p (if (string=? *in-file* "-")
		    (current-input-port)
		    (open-input-file *in-file*)))
	  (out-p (if (string=? *out-file* "-")
		     (current-output-port)
		     (open-output-file *out-file*)))
	  (ast (parse in-p)))
      (statements ast)
      (js-out ast out-p)
      (if (not (string=? *in-file* "-"))
	  (close-input-port in-p))
      (if (not (string=? *out-file* "-"))
	  (close-output-port out-p))))
