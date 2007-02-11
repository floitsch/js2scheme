(module jsre-globals-tmp
   (export (globals-tmp-add! f)
	   *globals-init-tmp*))

(define *globals-init-tmp* '())

(define (globals-tmp-add! f)
   (set! *globals-init-tmp* (cons f *globals-init-tmp*)))
