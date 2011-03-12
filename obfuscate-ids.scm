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

(module obfuscate-ids
   (import nodes
	   verbose
	   walk)
   (export (obfuscate-ids! tree::Program)
	   (add-obfuscation-mapping! js-id::bstring obfuscation-id::bstring)
	   *obfuscate-globals?*
	   *obfuscate-properties?*
	   *obfuscation-mapping-p*))

(define *obfuscation-mapping-p* #f)

(define *obfuscate-globals?* #f)
(define *obfuscate-properties?* #f)

;; Properties are global and must be inside the global mapping.
(define *obfuscation-mapping* (make-hashtable))
(define *used-ids* (make-hashtable))

(define (hashtable->alist ht)
   (map (lambda (from)
	   (list from (hashtable-get ht from)))
	(hashtable-key-list ht)))

(define (obfuscate-ids! tree)
   (verbose "obfuscate-ids!")
   (obfuscate tree #f)
   (when *obfuscation-mapping-p*
      (with-output-to-port *obfuscation-mapping-p*
	 (lambda () (write (hashtable->alist *obfuscation-mapping*))))))

(define-nmethod (Node.obfuscate)
   (default-walk this))

(define-nmethod (Ref.obfuscate)
   (obfuscate-var (Ref-var this)))

(define-nmethod (Access.obfuscate)
   (default-walk this)
   (with-access::Access this (field)
      (when (String? field)
	 (with-access::String field (val)
	    (set! val (obfuscate-property val))))))

(define-nmethod (Property-Init.obfuscate)
   (default-walk this)
   (with-access::Property-Init this (name)
      (when (String? name)
	 (with-access::String name (val)
	    (set! val (obfuscate-property val))))))

(define-generic (obfuscate-var this::Var)
   (with-access::Var this (generated id global? arguments?)
      (unless generated
	 (cond
	    ((eq? id 'this)
	     (set! generated 'this))
	    (arguments?
	     (set! generated 'arguments))
	    ((and global?
		  (not *obfuscate-globals?*))
	     ;; do not obfuscate
	     (set! generated id))
	    (else
	     (set! generated (obfuscate-id id)))))))

(define-method (obfuscate-var this::Runtime-Var)
   (with-access::Runtime-Var this (generated id)
      (set! generated id)))

(define *counter* 0)
(define (generate-obfuscated-id)
   (set! *counter* (+ *counter* 1))
   (string-append "v" (number->string *counter*)))

(define (obfuscate-id id)
   (let* ((str (symbol->string id))
	  (obfuscated-id (hashtable-get *obfuscation-mapping* str)))
      (if obfuscated-id
	  (string->symbol obfuscated-id)
	  (let loop ()
	     (let ((obfuscated-id (generate-obfuscated-id)))
		(if (hashtable-get *used-ids* obfuscated-id)
		    (loop)
		    (begin
		       (add-obfuscation-mapping! str obfuscated-id)
		       (string->symbol obfuscated-id))))))))

(define (obfuscate-property property-str)
   (if (not *obfuscate-properties?*)
       property-str
       (let* ((len (string-length property-str))
	      (without-quotes (substring property-str 1 (- len 1)))
	      (without-quotes-symbol (string->symbol without-quotes))
	      (obfuscated-id (obfuscate-id without-quotes-symbol))
	      (obfuscated-id-str (symbol->string obfuscated-id)))
	  (if (string=? without-quotes obfuscated-id-str)
	      property-str ;; return unobfuscated string.
	      ;; obfuscated ids don't contain quotes.
	      (string-append "'" obfuscated-id-str "'")))))

(define (add-obfuscation-mapping! from::bstring to::bstring)
   (hashtable-put! *obfuscation-mapping* from to)
   (hashtable-put! *used-ids* to #t))
