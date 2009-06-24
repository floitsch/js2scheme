(module jsre-base-string
   (include "utf/string.sch")
   (include "utf/char.sch")
   (export (macro STR)))

(define-macro (STR str)
   `(ascii->js-string-literal ,str))
