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

(module walk
   (import nodes)
   (export
    (generic walk0 n::Node env p::procedure)
    (generic walk1 n::Node env p::procedure arg0)
    (generic walk2 n::Node env p::procedure arg0 arg1)
    (generic walk3 n::Node env p::procedure arg0 arg1 arg2)
    (generic walk4 n::Node env p::procedure arg0 arg1 arg2 arg3)
    (generic walk0! n::Node env p::procedure)
    (generic walk1! n::Node env p::procedure arg0)
    (generic walk2! n::Node env p::procedure arg0 arg1)
    (generic walk3! n::Node env p::procedure arg0 arg1 arg2)
    (generic walk4! n::Node env p::procedure arg0 arg1 arg2 arg3)
    (macro define-nmethod)
    (macro ncall)))

;; (define-nmethod (While.optim! x y) BODY)
;; =>
;; (define-method (optim! this::While env x y)
;;   (define (default-walk! n x y)
;;      (walk2! n env optim! x y))
;;   (define (walk! n x y)
;;      (optim! n env x y))
;;   BODY)
(define-macro (define-nmethod args . body)
   (define (without-type sym)
      (if (not (symbol? sym))
	  (scheme2js-error "bad define-nmethod (expected symbol)" args args)
	  (let* ((str (symbol->string sym))
		 (pos (string-contains str "::")))
	     (if pos
		 (string->symbol (substring str 0 pos))
		 sym))))

   (let* ((Type.name (car args))
	  (method-args (cdr args))
	  (str-type.name (symbol->string Type.name))
	  (str-len (string-length str-type.name))
	  (dot-pos (string-index str-type.name #\.))
	  (dummy (when (not dot-pos)
		    (scheme2js-error "bad define-nmethod name" Type.name Type.name)))
	  (type (string->symbol (substring str-type.name 0 dot-pos)))
	  (name (string->symbol (substring str-type.name
					   (+fx dot-pos 1)
					   str-len)))
	  (bang? (char=? #\! (string-ref str-type.name (- str-len 1))))
	  (nb-method-args (length method-args))
	  (short-walk (if bang? 'walk! 'walk))
	  (long-walk (if bang?
			 (string->symbol (format "walk~a!" nb-method-args))
			 (string->symbol (format "walk~a" nb-method-args))))
	  (define-gen/met (if (eq? type 'Node)
			      'define-generic
			      'define-method))
	  (default-walk (symbol-append 'default- short-walk))
	  (args-without-type (map without-type method-args))
	  (gensymed-method-args (map gensym args-without-type)))
      `(,define-gen/met (,name ,(symbol-append 'this:: type) env ,@method-args)
	  (define (,default-walk node ,@gensymed-method-args)
	     (,long-walk node env ,name ,@gensymed-method-args))
	  (define (,short-walk node ,@gensymed-method-args)
	     (,name node env ,@gensymed-method-args))
	  ,@body)))

(define-macro (ncall method-name node . args)
   `(,method-name ,node env ,@args))
    
(define-generic (walk0 n::Node env p::procedure)
   (error "walk0"
	  "Internal Error: forgot Node type"
	  (with-output-to-string (lambda () (write-circle n)))))
(define-generic (walk1 n::Node env p::procedure arg0)
   (error "walk1"
	  "Internal Error: forgot Node type"
	  (with-output-to-string (lambda () (write-circle n)))))
(define-generic (walk2 n::Node env p::procedure arg0 arg1)
   (error "walk2"
	  "Internal Error: forgot Node type"
	  (with-output-to-string (lambda () (write-circle n)))))
(define-generic (walk3 n::Node env p::procedure arg0 arg1 arg2)
   (error "walk3"
	  "Internal Error: forgot Node type"
	  (with-output-to-string (lambda () (write-circle n)))))
(define-generic (walk4 n::Node env p::procedure arg0 arg1 arg2 arg3)
   (error "walk4"
	  "Internal Error: forgot Node type"
	  (with-output-to-string (lambda () (write-circle n)))))

(define-generic (walk0! n::Node env p::procedure)
   (error "walk0!"
	  "Internal Error: forgot Node type"
	  (with-output-to-string (lambda () (write-circle n)))))
(define-generic (walk1! n::Node env p::procedure arg0)
   (error "walk1!"
	  "Internal Error: forgot Node type"
	  (with-output-to-string (lambda () (write-circle n)))))
(define-generic (walk2! n::Node env p::procedure arg0 arg1)
   (error "walk2!"
	  "Internal Error: forgot Node type"
	  (with-output-to-string (lambda () (write-circle n)))))
(define-generic (walk3! n::Node env p::procedure arg0 arg1 arg2)
   (error "walk3!"
	  "Internal Error: forgot Node type"
	  (with-output-to-string (lambda () (write-circle n)))))
(define-generic (walk4! n::Node env p::procedure arg0 arg1 arg2 arg3)
   (error "walk4!"
	  "Internal Error: forgot Node type"
	  (with-output-to-string (lambda () (write-circle n)))))

(define-macro (gen-walks class . fields)
   (define (field-name f)
      (if (pair? f)
	  (car f)
	  (without-? f)))

   (define (starts-with-? f)
      (char=? #\? (string-ref (symbol->string f) 0)))

   (define (without-? f)
      (if (starts-with-? f)
	  (let ((str (symbol->string f)))
	     (string->symbol (substring str 1 (string-length str))))
	  f))

   (define (visit f nb-args)
      (cond
	 ((pair? f)
	  `(for-each (lambda (f)
			(p f env
			   ,@(map (lambda (i)
				     (string->symbol (format "arg~a" i)))
				  (iota nb-args))))
		     ,(car f)))
	 ((starts-with-? f)
	  `(when ,(without-? f)
	      ,(visit (without-? f) nb-args)))
	 (else
	  `(p ,f env ,@(map (lambda (i)
			       (string->symbol (format "arg~a" i)))
			    (iota nb-args))))))

   (define (visit! f nb-args)
      (cond
	 ((pair? f)
	  `(let loop ((fields ,(car f)))
	      (unless (null? fields)
		 (set-car! fields
			   (p (car fields) env
			      ,@(map (lambda (i)
					(string->symbol (format "arg~a" i)))
				     (iota nb-args))))
		 (loop (cdr fields)))))
	 ((starts-with-? f)
	  `(when ,(without-? f)
	      ,(visit! (without-? f) nb-args)))
	 (else
	  `(set! ,f (p ,f env ,@(map (lambda (i)
					(string->symbol (format "arg~a" i)))
				     (iota nb-args)))))))
   
   (define (gen-method nb-args bang?)
      `(define-method (,(if bang?
			    (string->symbol (format "walk~a!" nb-args))
			    (string->symbol (format "walk~a" nb-args)))
		       ,(symbol-append 'n:: class)
		       env
		       p
		       ,@(map (lambda (i)
				 (string->symbol (format "arg~a" i)))
			      (iota nb-args)))
	  ,(if (null? fields)
	       #unspecified
	       `(,(symbol-append 'with-access:: class) n ,(map field-name fields)
	          ,@(map (lambda (f)
		      ((if bang? visit! visit) f nb-args))
		   fields)))
	  n))

   `(begin
       ,@(map (lambda (nb) (gen-method nb #f)) (iota 4))
       ,@(map (lambda (nb) (gen-method nb #t)) (iota 4))))

(gen-walks Ref)
(gen-walks Scope body)
(gen-walks Program this-decl (imported) (runtime) (implicit-globals) body)
(gen-walks Begin (els))
(gen-walks NOP)
(gen-walks If test then else)
(gen-walks For ?init ?test ?incr body)
(gen-walks While test body)
(gen-walks Do body test)
(gen-walks For-In lhs obj body)
(gen-walks Bind-Exit body)
(gen-walks Bind-Exit-Invoc expr)
(gen-walks Continue)
(gen-walks Break)
(gen-walks Return expr)
(gen-walks With obj body)
(gen-walks Obj-Init (props))
(gen-walks Switch key (cases))
(gen-walks Fall-Through)
(gen-walks Case expr body)
(gen-walks Default body)
(gen-walks Throw expr)
(gen-walks Try body ?catch ?finally)
(gen-walks Catch decl body)
(gen-walks Labelled body)
(gen-walks Assig lhs val)
(gen-walks Vassig-Op lhs op val)
(gen-walks Accsig-Op lhs op val)
(gen-walks Named-Fun decl body)
(gen-walks Fun this-decl arguments-decl (params) body)
(gen-walks Call op (args))
(gen-walks New class (args))
(gen-walks Access obj field)
(gen-walks Literal)
(gen-walks Array (els))
(gen-walks Array-Element expr)
(gen-walks Property-Init name val)
(gen-walks Reg-Exp)
(gen-walks Let* (vassigs) body)
