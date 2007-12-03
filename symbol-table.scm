(module symbol-table
   (export
    (inline make-symbol-table)
    (inline make-scope-table)
    (inline add-scope symbol-table scope-table)
    (inline symbol-var symbol-table symbol)
    (inline local-symbol-var symbol-table symbol)
    (inline symbol-var-set! symbol-table symbol var)
    (inline scope-symbol-var-set! scope symbol var)
    (inline scope-symbol-var scope symbol)))


(define-inline (make-symbol-table) '())
(define-inline (make-scope-table) (make-hashtable))

(define-inline (add-scope symbol-table scope-table)
   (cons scope-table symbol-table))

(define-inline (symbol-var symbol-table symbol)
   (any (lambda (ht)
	   (hashtable-get ht symbol))
	symbol-table))

(define-inline (local-symbol-var symbol-table symbol)
   (and (pair? symbol-table)
	(hashtable-get (car symbol-table) symbol)))

(define-inline (symbol-var-set! symbol-table symbol var)
   (scope-symbol-var-set! (car symbol-table) symbol var))

(define-inline (scope-symbol-var-set! scope symbol var)
   (hashtable-put! scope symbol var))

(define-inline (scope-symbol-var scope symbol)
   (hashtable-get scope symbol))

