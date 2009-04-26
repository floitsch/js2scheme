(module mset
   (export (make-mset #!key (eqtest eq?) (hash get-hashnumber))
	   (mset-put! mset val)
	   (mset-remove! mset val)
	   (mset-contains? mset val)
	   (mset->list mset)
	   (mset-for-each proc mset)
	   (mset-duplicate mset)
	   (mset-size mset)))

;; A mset is a mutable set. That is instead of returning a new set after each
;; operation it updates its state.
;; Currently based on hashtables. But should improve in the future.

(define (make-mset #!key (eqtest eq?) (hash get-hashnumber))
   (create-hashtable :size 5 :eqtest eqtest :hash hash))

(define (mset-put! mset val)
   (hashtable-put! mset val #t))

(define (mset-remove! mset val)
   (hashtable-remove! mset val))

(define (mset-contains? mset val)
   (hashtable-get mset val))

(define (mset->list mset)
   (hashtable-key-list mset))

(define (mset-for-each proc mset)
   (hashtable-for-each mset
		       (lambda (key b)
			  (proc key))))

(define (mset-duplicate mset)
   (let ((new-mset (make-mset)))
      (hashtable-for-each mset
			  (lambda (key b)
			     (hashtable-put! new-mset key #t)))))

(define (mset-size mset)
   (hashtable-size mset))
