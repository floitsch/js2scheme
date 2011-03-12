;; Copyright (c) 2007-2011, Florian Loitsch
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are met:
;;    * Redistributions of source code must retain the above copyright
;;      notice, this list of conditions and the following disclaimer.
;;    * Redistributions in binary form must reproduce the above copyright
;;      notice, this list of conditions and the following disclaimer in the
;;      documentation and/or other materials provided with the distribution.
;;    * Neither the name of the <organization> nor the
;;      names of its contributors may be used to endorse or promote products
;;      derived from this software without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;; DISCLAIMED. IN NO EVENT SHALL <COPYRIGHT HOLDER> BE LIABLE FOR ANY
;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
;; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
;; ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;; extracts exported macros from given files.
(module extract-macros
   (main my-main))

(define *output* #f)
(define *in-files* '())

(define (my-main args)
   (args-parse (cdr args)
      (("-o" ?file (help "Output file"))
       (set! *output* file))
      (else
       (set! *in-files* (cons else *in-files*))))

   (let ((out-p (if (not *output*)
		    (current-output-port)
		    (open-output-file *output*))))
      (with-handler
	 (lambda (e)
	    (when *output*
	       (delete-file *output*))
	    (raise e))
	 (unwind-protect
	    (begin
	       (display ";; GENERATED FILE -- DO NOT EDIT\n" out-p)
	       (display ";; GENERATED FILE -- DO NOT EDIT\n" out-p)
	       (display ";; GENERATED FILE -- DO NOT EDIT\n" out-p)
	       (display ";; GENERATED FILE -- DO NOT EDIT\n" out-p)
	       (display ";; GENERATED FILE -- DO NOT EDIT\n" out-p)
	       (for-each (lambda (f)
			    (extract-exported-macros f out-p))
			 *in-files*))
	    (close-output-port out-p)))))

(define (extract-exported-macros f out-p)
   (display (format "extracting macros from: ~a\n" f) (current-error-port))
   (with-input-from-file f
      (lambda ()
	 (let ((module-clause (read)))
	    (if (and (list? module-clause)
		     (or (eq? 'module (car module-clause))
			 (eq? 'directives (car module-clause))))
		(let ((exported-macros (search-exports module-clause))
		      (include-files (search-includes module-clause)))
		   (print-definitions exported-macros
				      (cons f include-files)
				      out-p))
		(warning "ignoring file: " f))))))

(define (extract-module-info m kind)
   ;; (module m
   ;;    (import ...)
   ;;    (export
   ;;       *a*
   ;;       (foo)
   ;;       (macro m))
   ;;    (main ...)
   ;;    (export
   ;;      (macro m2)))
   (let* ((a-list (filter pair? m))
	  (kind-list (filter-map (lambda (p)
				    (and (eq? (car p) kind)
					 (cdr p)))
				 a-list))
	  (merged-kinds (apply append kind-list)))
      merged-kinds))
   
(define (search-exports m)
   (let* ((exports (extract-module-info m 'export))
	  (macros (filter (lambda (e) (and (pair? e)
					   (eq? 'macro (car e))))
			  exports)))
      (map cadr macros)))

(define (search-includes m)
   (extract-module-info m 'include))

(define (print-definitions exported-macros
			   files
			   out-p)
   (cond
      ((null? exported-macros)
       'done)
      ((null? files)
       (error "extract-macros"
	      "could not find exported-macros"
	      exported-macros))
      (else
       (with-input-from-file (car files)
	  (lambda ()
	     (let loop ((exported-macros exported-macros))
		(if (null? exported-macros)
		    'done
		    (let ((e (read)))
		       (cond
			  ((eof-object? e)
			   (print-definitions exported-macros
					      (cdr files)
					      out-p))
			  ((and (pair? e)
				(eq? (car e) 'define-macro))
			   (let ((m-name (car (cadr e))))
			      (if (memq m-name exported-macros)
				  (begin
				     (pp e out-p)
				     (loop (filter (lambda (m)
						      (not (eq? m m-name)))
						   exported-macros)))
				  (loop exported-macros))))
			  (else
			   (loop exported-macros)))))))))))
