(module label
   (export
    (class Label
       (ids::pair-nil (default '()))
       (used?::bool (default #f))

       (generated (default #f)))
    (final-class Break-Label::Label)
    (final-class Continue-Label::Label)

    (label-add-id! label::Label id)
       
    *default-break-label-id*
    *default-continue-label-id*
    *return-label-id*
    (make-label-table)
    (label-set! label-table id label)
    (label-get label-table id)
    (label-remove! label-table id)))

;; following labels can't clash with JS-labels, as they use a dash in their id.
(define *default-break-label-id* '<*default-break*>)
(define *default-continue-label-id* '<*default-continue*>)
(define *return-label-id* '<*return-label*>)

(define (make-label-table) (make-hashtable))

(define (label-set! label-table id label)
   (hashtable-put! label-table id label))

(define (label-get label-table id)
   (hashtable-get label-table id))

(define (label-remove! label-table id)
   (hashtable-remove! label-table id))

(define (label-add-id! label id)
   (with-access::Label label (ids)
      (set! ids (cons id ids))))
