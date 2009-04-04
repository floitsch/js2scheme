(module jsre-natives
   (export
    (inline js-undefined)
    (inline js-undefined? v)))

(define-inline (js-undefined) #unspecified)
(define-inline (js-undefined? v) (eq? v #unspecified))
