(module js2scheme
   (import js2scheme-comp)
   (main js2scheme-prog))

(define (js2scheme-prog args)
   ;(set! *verbose* #t)
   (let ((in-p (current-input-port)))
      (pp (js2scheme in-p))
      'done))
