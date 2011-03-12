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

(module str-cache
   (main my-main))

(define *seen-str-include?* #f)
(define *strs* '())

(define *in-file* #f)
(define *out-file* #f)

(define (my-main args)
   (args-parse (cdr args)
      (("-o" ?file (help "Out-File file"))
       (set! *out-file* file))
      (else
       (when *in-file*
	  (error "str-cache"
		 "Only one in-file allowed"
		 else)
	  (args-parse-usage #f))
       (set! *in-file* else)))
   (when (or (not *in-file*)
	     (not *out-file*))
      (error "str-cache"
	     "in-file or out-file missing"
	     #f))
   (display (format "writing STR-caches for ~a\n" *in-file*) (current-error-port))

   (extract-and-verify-STRs!)
   (write-STR-cache!))


(define (write-STR-cache!)
   (let ((out-p (if (string=? *out-file* "-")
		    (current-output-port)
		    (open-output-file *out-file*))))
      (with-handler
	 (lambda (e)
	    (when (not (string=? *out-file* "-"))
	       (delete-file *out-file*))
	    (raise e))
	 (unwind-protect
	    (let ((cache-ids (map (lambda (str)
				     (gensym 'str))
				  *strs*)))
	       (display ";; GENERATED FILE -- DO NOT EDIT\n" out-p)
	       (display ";; GENERATED FILE -- DO NOT EDIT\n" out-p)
	       (display ";; GENERATED FILE -- DO NOT EDIT\n" out-p)
	       (display ";; GENERATED FILE -- DO NOT EDIT\n" out-p)
	       (display ";; GENERATED FILE -- DO NOT EDIT\n" out-p)
	       (unless (null? *strs*)
		  (pp `(define-macro (STR str)
			  (cdr (assoc str ',(map cons *strs* cache-ids))))
		      out-p)
		  (for-each (lambda (id str)
			       (pp `(define ,id (utf8->js-string-literal ,str))
				   out-p))
			    cache-ids
			    *strs*)))
	    (close-output-port out-p)))))

(define (extract-and-verify-STRs!)
   (let ((in-p (if (string=? *in-file* "-")
		   (current-input-port)
		   (open-input-file *in-file*))))
      (unwind-protect
	 (extract in-p)
	 (when (and (pair? *strs*)
		    (not *seen-str-include?*))
	    (warning "str-cache" "could not find str-include" *in-file*))
	 (when (and (null? *strs*)
		    *seen-str-include?*)
	    (warning "str-cache" "str-include without STRs" *in-file*))
	 (close-input-port in-p))))

(define (extract in-p)
   (let ((e (read in-p)))
      (unless (eof-object? e)
	 (extract-expr! e)
	 (extract in-p))))

(define (add-str! str)
   (unless (member str *strs*)
      (set! *strs* (cons str *strs*))))

;; we do not handle quotes, etc. Should not be necessary for now.
(define (extract-expr! e)
   (match-case e
      ((define (STR . ?-) . ?-) 'do-nothing)
      ((define-macro (STR . ?-) . ?-) 'do-nothing)
      ((define-inline (STR . ?-) . ?-) 'do-nothing)
      ((define-macro (define-runtime-globals . ?-) . ?-) 'do-nothing)
      ((define-runtime-globals . ?defs)
       (for-each (lambda (def)
		    (match-case def
		       ((define (?id . ?-) . ?body)
			(add-str! (symbol->string id))
			(for-each extract-expr! body))
		       (else
			(extract-expr! def))))
		 defs))
      ((STR (and ?str (? string?)))
       (add-str! str))
      ((STR . ?-)
       (warning "str-cache" "bad STR form" e))
      ((include ?f)
       (when (and *seen-str-include?*
		  (string=? (prefix f) "strs"))
	  (warning "str-cache" "already seen string-include" e))
       ;; for now just see if they are equal. we can implement more
       ;; sophisticated techniques later.
       (when (not (string=? f *out-file*))
	  (warning "str-cache" "string-include and out-file don't match"
		   f
		   *out-file*))
       (set! *seen-str-include?* #t))
      ((?- . ?-)
       (let loop ((e e))
	  (cond
	     ((null? e) 'done)
	     ((pair? e) (extract-expr! (car e))
			(loop (cdr e)))
	     (else (extract-expr! e)))))
      (else 'do-nothing)))
