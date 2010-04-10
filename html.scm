(module html
   (export *HTML-globals*)
   (export *HTML-properties*))

(define *HTML-globals* '(document
			 window
			 navigator
			 screen
			 HTMLElement
			 ActiveXObject
			 XMLHttpRequest
			 event
			 unescape
			 setTimeout))

(define *HTML-properties* '(innerHTML))
