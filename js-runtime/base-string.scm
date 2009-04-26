(module jsre-base-string
   (include "js-string-impl.sch")
   (export (macro STR)))

(define-macro (STR str)
   `(utf8->js-string-literal ,str))
