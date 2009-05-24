(module jsre-base-string
   (include "js-string-impl.sch")
   (import jsre-base-char)
   (from jsre-base-char)
   (export (macro STR)))

(define-macro (STR str)
   `(utf8->js-string-literal ,str))
