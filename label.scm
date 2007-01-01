(module label
   (include "protobject.sch")
   (include "nodes.sch")
   (option (loadq "protobject-eval.sch"))
   (import protobject
	   verbose
	   nodes)
   (export *default-break-label-id*
	   *default-continue-label-id*
	   *return-label-id*
	   (make-label-table)
	   (label-set! label-table id label)
	   (label-get label-table id)
	   (label-remove! label-table id)
	   Label
	   Break-label
	   Continue-label))

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

(define-pclass (Label)
   (set! this.ids '()))
(define-pmethod (Label-add-id! id)
   (set! this.ids (cons id this.ids)))
(set! Label.proto.add-id! Label-add-id!)

(define-pclass (Break-label))
(set! Break-label.proto (empty-pobject Label))

(define-pclass (Continue-label))
(set! Continue-label.proto (empty-pobject Label))
