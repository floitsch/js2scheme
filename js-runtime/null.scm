(module jsre-Null
   (import jsre-object
	   jsre-exception
	   jsre-natives))

(define-method (js-property-one-level-contains o::Js-Null
					       prop::bstring)
   #f)

(define-method (js-property-contains o::Js-Null
				     prop::bstring)
   #f)

(define-method (js-property-generic-set! o::Js-Null
					 prop::bstring
					 new-value)
   (error-fun))

(define-method (js-property-direct-set! o::Js-Null
					prop::bstring
					new-entry::Property-entry)
   (error-fun))
