(module jsre-String
   (include "macros.sch")
   (import jsre-natives)
   (use jsre-object
	jsre-Object
	jsre-Date
	jsre-Array
	jsre-Function
	jsre-Number
	jsre-Bool
	jsre-Error
	jsre-RegExp
	jsre-RegExp-match
	jsre-RegExp-fsm
	jsre-primitives
	jsre-conversion
	jsre-global-object
	jsre-scope-object
	)
   (export *jsg-String*
	   *js-String-orig*::procedure
	   (class Js-String::Js-Object
	      (str::bstring read-only))
	   (String-init)))

;; 15.5 String Objects

(define *jsg-String* #unspecified)
(define *js-String-orig* (lambda () 'to-be-replaced))
(define *js-String-prototype*::Js-Object (js-null))

(define-method (js-class-name::bstring o::Js-String)
   "String") ;; 15.5.2.1

(define (String-init)
   (set! *js-String-orig* (String-lambda))
   (set! *jsg-String* (create-runtime-global "String" *js-String-orig*))

   (let* ((text-repr "function(v) {/* native String */ throw 'native';}")
	  (string-object (create-function-object *js-String-orig*
						 (String-new)
						 String-construct
						 text-repr))
	  (prototype (instantiate::Js-String
			(props (make-props-hashtable))
			(proto (js-object-prototype))
			(str ""))))

      (set! *js-String-prototype* prototype)

      (js-property-generic-set! string-object  ;; 15.5.3
				"length"
				1.0
				(length-attributes))
      (js-property-generic-set! string-object  ;; 15.5.3.1
				"prototype"
				prototype
				(get-Attributes dont-enum
						dont-delete read-only))
      (js-property-generic-set! string-object  ;; 15.5.3.2
			        "fromCharCode"
				(fromCharCode)
				(built-in-attributes))

      ;; prototype is a String-object. -> set the length value
      (js-property-generic-set! prototype      ;; 15.5.5.1
				"length"
				0.0
				(get-Attributes dont-enum
						dont-delete read-only))
      (js-property-generic-set! prototype      ;; 15.5.4.1
				"constructor"
				*js-String-orig*
				(constructor-attributes))
      (js-property-generic-set! prototype      ;; 15.5.4.2
				"toString"
				(toString)
				(built-in-attributes))
      (js-property-generic-set! prototype      ;; 15.5.4.3
				"valueOf"
				(valueOf)
				(built-in-attributes))
      (js-property-generic-set! prototype      ;; 15.5.4.4
				"charAt"
				(charAt)
				(built-in-attributes))
      (js-property-generic-set! prototype      ;; 15.5.4.5
				"charCodeAt"
				(charCodeAt)
				(built-in-attributes))
      (js-property-generic-set! prototype      ;; 15.5.4.6
				"concat"
				(concat)
				(built-in-attributes))
      (js-property-generic-set! prototype      ;; 15.5.4.7
				"indexOf"
				(indexOf)
				(built-in-attributes))
      (js-property-generic-set! prototype      ;; 15.5.4.8
				"lastIndexOf"
				(lastIndexOf)
				(built-in-attributes))
      (js-property-generic-set! prototype      ;; 15.5.4.9
				"localeCompare"
				(localeCompare)
				(built-in-attributes))
      (js-property-generic-set! prototype      ;; 15.5.4.10
				"match"
				(match)
				(built-in-attributes))
      (js-property-generic-set! prototype      ;; 15.5.4.11
				"replace"
				(replace)
				(built-in-attributes))
      (js-property-generic-set! prototype      ;; 15.5.4.12
				"search"
				(search)
				(built-in-attributes))
      (js-property-generic-set! prototype      ;; 15.5.4.13
				"slice"
				(slice)
				(built-in-attributes))
      (js-property-generic-set! prototype      ;; 15.5.4.14
				"split"
				(split)
				(built-in-attributes))
      (js-property-generic-set! prototype      ;; 15.5.4.15
				"substring"
				(js-substring)
				(built-in-attributes))
      (js-property-generic-set! prototype      ;; 15.5.4.16
				"toLowerCase"
				(toLowerCase)
				(built-in-attributes))
      (js-property-generic-set! prototype      ;; 15.5.4.17
				"toLocaleLowerCase"
				(toLocaleLowerCase)
				(built-in-attributes))
      (js-property-generic-set! prototype      ;; 15.5.4.18
				"toUpperCase"
				(toUpperCase)
				(built-in-attributes))
      (js-property-generic-set! prototype      ;; 15.5.4.19
				"toLocaleUpperCase"
				(toLocaleUpperCase)
				(built-in-attributes))
      ))

(define (String-lambda) ;; 15.5.1.1
   (js-fun-lambda
    #f #f
    (nb-args get-arg)
    (first-arg)
    (if (zero? nb-args)
	""
	(any->string first-arg))))

(define (String-new)
   (js-fun-lambda ;; 15.5.2.1
    #f
    #f
    (nb-args get-arg)
    (value)
    (let* ((str (if (zero? nb-args)
		    ""
		    (any->string value)))
	   (str-len (string-length str))
	   (o (instantiate::Js-String
		 (props (make-props-hashtable))
		 (proto *js-String-prototype*)
		 (str str))))
       ;; 15.5.5.1
       (js-property-generic-set! o "length"
				 (fixnum->flonum str-len)
				 (get-Attributes dont-enum dont-delete
						 read-only))
       o)))

(define (String-construct f-o::Js-Function)
   #f)

(define (fromCharCode) ;; 15.5.3.2
   (js-fun
    this #f
    (nb-args get-arg) "String.fromCharCode"
    (c0) ;; length 1
    (list->string (map (lambda (i)
			  (let ((n (flonum->fixnum (any->uint16 (get-arg i)))))
			     (if (>= n 256)
				 #\- ;; TODO currently we only support ASCII
				 (integer->char n))))
		       (iota nb-args)))))

(define (toString)                       ;; 15.5.4.2
   (js-fun this #f #f "String.prototype.toString"
	   ()
	   (if (not (Js-String? this))
	       (type-error "String-toString applied to" this)
	       (Js-String-str this))))
	   
(define (valueOf)                        ;; 15.5.4.3
   (js-fun this #f #f "String.prototype.valueOf"
	   ()
	   (if (not (Js-String? this))
	       (type-error "String-valueOf applied to" this)
	       (Js-String-str this))))

(define (js-string-ref str pos-any)         ;; 15.5.4.4 && 15.5.4.5
   (let* ((str-len (string-length str))
	  (pos (any->integer pos-any)))
      (cond
	 ((<fl pos 0.0)
	  #f)
	 ((>= pos str-len)
	  #f)
	 (else
	   ;; CARE: we use bints here. as strings can't be longer anyways this
	   ;; should not matter.
	  (string-ref str (flonum->fixnum pos))))))
   
(define (charAt)                         ;; 15.5.4.4
   (js-fun
    this #f #f "String.prototype.charAt"
    (pos)
    (let* ((str (if (Js-String? this)
		    (Js-String-str this)
		    (any->string this)))
	   (c (js-string-ref str pos)))
       (if c
	   (string c)
	   ""))))

(define (charCodeAt)                    ;; 15.5.4.5
   (js-fun
    this #f #f "String.prototype.charCodeAt"
    (pos-any)
    (let* ((str (if (Js-String? this)
		    (Js-String-str this)
		    (any->string this)))
	   (c (js-string-ref str pos-any)))
       (if c
	   (fixnum->flonum (char->integer c))
	   +nan.0))))

(define (concat)                        ;; 15.5.4.6
   (js-fun
    this #f
    (nb-args get-arg)
    "String.prototype.concat"
    (str1)
    (let ((this-str (if (Js-String? this)
			(Js-String-str this)
			(any->string this))))
       (cond
	  ((=fx nb-args 0)
	   this-str)
	  ((=fx nb-args 1)
	   (string-append this-str (any->string str1)))
	  (else
	   (let loop ((i 0)
		      (rev-strs '()))
	      (cond
		 ((>= i nb-args)
		  (apply string-append (cons this-str (reverse! rev-strs))))
		 (else
		  (loop (+fx i 1)
			(cons (any->string (get-arg i)) rev-strs))))))))))


(define (string-contains-right str search-str pos)
   (let ((len1 (string-length str))
	 (len2 (string-length search-str)))
	 
      (let loop ((i (minfx pos (-fx len1 len2))))
	 (cond
	    ((<fx i 0) #f)
	    ((substring-at? str search-str i) i)
	    (else (loop (-fx i 1)))))))
      
;; 15.5.4.7 15.5.4.8
(define (first/last-index-of this search-str-any pos-any find-first?)
   (let* ((this-str (if (Js-String? this)
			(Js-String-str this)
			(any->string this)))
	  (len (string-length this-str))
	  (search-str (any->string search-str-any))
	  (pos-num (any->number pos-any))
	  (pos (if (nanfl? pos-num)
		   (if find-first? 0 len)
		   ;; CARE: we use bints here. as strings can't be longer
		   ;; anyways this should not matter.
		   (flonum->fixnum (min-2fl (max-2fl (any->integer pos-num)
						     0.0)
					  (fixnum->flonum len)))))
	  (search-fun (if find-first? string-contains string-contains-right)))

      (let ((tmp (search-fun this-str search-str pos)))
	 (if tmp
	     (fixnum->flonum tmp)
	     -1.0))))

(define (indexOf)                       ;; 15.5.4.7
   (let ((f (js-fun-lambda
	     this #f #f
	     (search-str pos)
	     (first/last-index-of this search-str pos #t))))
      (create-function f 1 "String.prototype.indexOf")
      f))


(define (lastIndexOf)                   ;; 15.5.4.8
   (let ((f (js-fun-lambda
	     this #f #f
	     (search-str pos)
	     (first/last-index-of this search-str pos #f))))
      (create-function f 1 "String.prototype.lastIndexOf")
      f))

(define (localeCompare)                 ;; 15.5.4.9
   (js-fun
    this #f #f "String.prototype.localeCompare"
    (that)
    (let* ((this-str (if (Js-String? this)
			 (Js-String-str this)
			 (any->string this)))
	   (that-str (any->string that)))
       (fixnum->flonum (string-compare3 this-str that-str)))))

(define (match)                         ;; 15.5.4.10
   (js-fun
    this #f #f "String.prototype.match"
    (regexp)
    (let* ((re (if (Js-RegExp? regexp) ;; re does not need to be an object
		   regexp
		   (js-new (global-read *jsg-RegExp*) regexp)))
	   (s (if (Js-String? this)
		  (Js-String-str this)
		  (any->string this))))
       (when (not (Js-RegExp? re))
	  (type-error "RegExp required" re))
       (let ((global? (js-property-get re "global")))
	  (if (not global?)
	      (js-call *js-RegExp-exec* re s)
	      ;; mostly copied from RegExp.
	      (let ((native-re (Js-RegExp-re re))
		    (len (string-length s))
		    (res-a (empty-js-Array)))
		 (let loop ((s-pos 0)
			    (a-index 0))
		    (cond
		       ((>fx s-pos len) ;; done
			(js-property-set! re "lastIndex" 0.0)
			res-a)
		       ((regexp-match native-re s s-pos)
			=>
			(lambda (match)
			   (let ((start-index (car match))
				 (final-index (cadr match)))
			      (js-property-set! res-a
						     (integer->string a-index)
						     (substring s
								start-index
								final-index))
			      (loop (if (=fx final-index s-pos)
					(+fx s-pos 1)
					final-index)
				    (+fx a-index 1)))))
		       (else ;; no match
			;; finished -> loop one last time and let the first
			;; condition handle the rest.
			(loop (+fx len 1) a-index))))))))))

(define (procedure-replacement replaceValue this-str match from to)
   (cond
      ((and (procedure? replaceValue)
	    (fixnum? match))
       (js-call replaceValue #f
		(substring this-str from to) from this-str))
      ((procedure? replaceValue) ;; match must be JS-array
       (let* ((l-fl (js-property-get match "length"))
	      ;; CARE: using fixnum for array-index.
	      (l-i (flonum->fixnum l-fl)))
	  ;; the array is nearly good.
	  ;; we just need to add the offset and the string itself
	  (js-property-set! match
				 (integer->string l-i)
				 (fixnum->flonum from))
	  (js-property-set! match
				 (integer->string (+fx l-i 1))
				 this-str)
	  ;; TODO: do not call 'apply' from prototype as it could have been
	  ;; replaced.
	  (js-method-call replaceValue "apply" (js-null) match)))))

(define (string-replacement repl-str repl-len
			    this-str this-len
			    match from to)
   (define (num-char->int c)
      (-fx (char->integer c) (char->integer #\0)))

   (let loop ((i 0)
	      (res-str "")
	      (to-be-copied-pos 0)) ;; first pos of the not-yet-copied chars
      (cond
	 ((>=fx i (-fx repl-len 1)) ;; last char can be $
	  (string-append res-str
			 (substring this-str to-be-copied-pos repl-len)))
	 ((and (char=? (string-ref repl-str i) #\$)
	       (not (=fx to-be-copied-pos i)))
	  ;; simplify task. first copy the not-yet-copied chars.
	  ;; then iterate and deal with $
	  (loop i (string-append res-str
				 (substring this-str to-be-copied-pos i))
		i))
	 ((char=? (string-ref repl-str i) #\$)
	  ;; all previous chars have been dealt with.
	  (let ((c (string-ref repl-str (+fx i 1)))
		(i+2 (+fx i 2)))
	     (case c
		((#\$) (loop i+2 ;; simply add $
			     (string-append res-str "$")
			     i+2))
		((#\&) (loop i+2 ;; the matched string
			     (string-append res-str
					    (substring this-str from to))
			     i+2))
		((#\`) (loop i+2 ;; the preceding substring
			     (string-append res-str
					    (substring this-str 0 from))
			     i+2))
		((#\') (loop i+2 ;; the following substring
			     (string-append res-str
					    (substring this-str from this-len))
			     i+2))
		((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
		 (let* ((double-digit? (and (<fx i+2 repl-len)
					    (char-numeric?
					     (string-ref repl-str i+2))))
			(c2 (and double-digit? (string-ref repl-str i+2)))
			(n (if double-digit?
			       (+fx (*fx 10 (num-char->int c))
				    (num-char->int c2))
			       (num-char->int c)))
			(next-i (if double-digit? (+fx i 3) i+2)))
		    (cond
		       ((fixnum? match)
			(loop next-i res-str next-i))
		       (else
			(let* ((n-str (integer->string n))
			       (capture (js-property-get match n-str)))
			   (cond
			      ((js-undefined? capture)
			       (loop next-i res-str next-i))
			      (else
			       (loop next-i
				     (string-append res-str capture)
				     next-i))))))))
		(else ;; the next char can not be a $ (as this would have
		 ;; matched $$. -> i+2
		 (loop i+2 res-str i)))))
	 (else (loop (+fx i 1) res-str to-be-copied-pos)))))

(define (all-matches str re)
   (js-property-set! re "lastIndex" 0.0)
   (let loop ((rev-ms '())
	      (last-index 0.0))
      (let ((m (js-call *js-RegExp-exec* re str)))
	 (cond
	    ((js-null? m)
	     (reverse! rev-ms))
	    (else
	     (let ((new-last-index (js-property-get re "lastIndex")))
		(if (=fl last-index new-last-index)
		    (begin
		       (js-property-set! re "lastIndex"
					      (+fl last-index 1.0))
		       (loop (cons m rev-ms)
			     (+fl last-index 1.0)))
		    (loop (cons m rev-ms) new-last-index))))))))
   
(define (replace)                       ;; 15.5.4.11
   (js-fun
    this #f #f "String.prototype.replace"
    (searchValue replaceValue)
    (let* ((this-str (if (Js-String? this)
			 (Js-String-str this)
			 (any->string this)))
	   (this-len (string-length this-str))
	   (search-str (and (not (Js-RegExp? searchValue))
			    (any->string searchValue)))
	   (search-len (and search-str (string-length search-str)))
	   (matches (cond
		       ((and (Js-RegExp? searchValue)
			     (not (js-property-get searchValue "global")))
			(js-call *js-RegExp-exec* searchValue this-str))
		       ((Js-RegExp? searchValue)
			(all-matches this-str searchValue))
		       (else
			(string-contains this-str search-str)))))
       ;; if we got a match, then matches is either an JS-array, a list of
       ;; JS-arrays or simply an int.
       (if (or (not matches) ;; string-contains
	       (js-null? matches) ;; non-global re
	       (null? matches)) ;; global re without matches.
	   this-str
	   ;; we can not compute the replace-str earlier, as the conversion
	   ;; could throw an exception.
	   (let* ((replace-str (and (not (procedure? replaceValue))
				    (any->string replaceValue)))
		  (replace-len (and replace-str (string-length replace-str)))
		  ;; avoid common case where no $ is in replace-str.
		  (contains-$? (and replace-str
				    (string-index replace-str #\$)
				    #t)))
	      (define (replace-matched match from to)
		 (cond
		    ((procedure? replaceValue)
		     (procedure-replacement replaceValue
					    this-str match from to))
		    ((not contains-$?)
		     replace-str)
		    (else
		     (string-replacement replace-str replace-len
					 this-str this-len
					 match from to))))
	      (cond
		 ((fixnum? matches)
		  (let ((from matches)
			(to (+fx matches search-len)))
		     (string-append (substring this-str 0 from)
				    (replace-matched match from to)
				    (substring this-str to this-len))))
		 ((Js-Array? matches)
		  (let* ((from (flonum->fixnum
				(js-property-get matches "index")))
			 (matched-str (js-property-get matches "0"))
			 (to (+fx from (string-length matched-str))))
		     (string-append (substring this-str 0 from)
				    (replace-matched matches from to)
				    (substring this-str to this-len))))
		 (else
		  (let loop ((matches matches)
			     (rev-strs '())
			     (to-be-copied-pos 0))
		     (cond
			((null? matches)
			 (apply string-append
				(reverse! (cons (substring this-str
							   to-be-copied-pos
							   this-len)
						rev-strs))))
			(else
			 (let* ((match (car matches))
				(from (flonum->fixnum
				       (js-property-get match "index")))
				(matched-str (js-property-get match "0"))
				(to (+fx from (string-length matched-str))))
			    (loop (cdr matches)
				  (cons* (replace-matched match from to)
					 (substring this-str
						    to-be-copied-pos
						    from)
					 rev-strs)
				  to))))))))))))

(define (search)                        ;; 15.5.4.12
   (js-fun
    this #f #f "String.prototype.search"
    (regexp)
    (let* ((re (if (Js-RegExp? regexp) ;; re does not need to be an object
		   regexp
		   (js-new (global-read *jsg-RegExp*) regexp)))
	   (s (if (Js-String? this)
		  (Js-String-str this)
		  (any->string this))))
       (when (not (Js-RegExp? re))
	  (type-error "RegExp required" re))
       (let ((match-index (RegExp-first-match-pos re s)))
	  (if match-index
	      (fixnum->flonum match-index)
	      -1.0)))))
    
(define (slice)                         ;; 15.5.4.13
   (js-fun
    this #f #f "String.prototype.slice"
    (start-any end-any)
    (let* ((s (if (Js-String? this)
		  (Js-String-str this)
		  (any->string this)))
	   (len (string-length s))
	   (len-fl (fixnum->flonum len))
	   (start (any->integer start-any))
	   (end (if (js-undefined? end-any)
		    len-fl
		    (any->integer end-any)))
	   (from (flonum->fixnum (if (<fl start 0.0)
				     (max-2fl (+fl len-fl start) 0.0)
				     (min-2fl len-fl start))))
	   (to (flonum->fixnum (if (<fl end 0.0)
				   (max-2fl (+fl len-fl end) 0.0)
				   (min-2fl len-fl end)))))
       (if (< from to)
	   (substring s from to)
	   ""))))

(define *char-cache*
   (let ((v (make-vector 256)))
      (let loop ((i 0))
	 (when (< i 256)
	    (vector-set! v i (string (integer->char i)))
	    (loop (+fx i 1))))
      v))

(define (cached-char-string c)
   (let ((n (char->integer c)))
      (if (< n 256)
	  (vector-ref *char-cache* n)
	  (string c))))

(define (split)                         ;; 15.5.4.14
   (js-fun
    this #f #f "String.prototype.split"
    (separator-any limit-any)
    (let* ((s (if (Js-String? this)
		  (Js-String-str this)
		  (any->string this)))
	   (len (string-length s))
	   (a (js-new (global-read *jsg-Array*))) ;; may throw an error
	   (limit (if (js-undefined? limit-any)
		      (maxvalfx) ;; TODO: should be maxuint32
		      ;; TODO using bints here.
		      (flonum->fixnum (any->uint32 limit-any))))
	   (separator (if (Js-RegExp? separator-any)
			  separator-any
			  (any->string separator-any))))
       (cond
	  ((zerofx? limit)
	   a)
	  ((js-undefined? separator-any)
	   (js-property-set! a "0" s)
	   a)
	  ((and (zerofx? len)
		(Js-RegExp? separator))
	   (when (RegExp-test separator s 0)
	      (js-property-set! a "0" s))
	   a)
	  ((zerofx? len) ;; separator must be a string
	   (unless (string-null? separator)
	      (js-property-set! a "0" s))
	   a)
	  ((string-null? separator)
	   ;; CARE: using bint here, but strings can't have more chars
	   ;;       anyways.
	   (let loop ((i 0))
	      (cond
		 ((>=fx i len)
		  a)
		 (else
		  (js-property-set! a (integer->string i)
					 (cached-char-string (string-ref s i)))
		  (loop (+fx i 1))))))
	  ((and (string? separator)
		(orig-Js-Array?)) ;; we can optimize this case.
	   ;; simply remove all matched occurences and create and put all
	   ;; interleaved elements into the array.
	   (let loop ((array-pos 0)
		      (last-pos 0))
	      (cond
		 ((=fx array-pos limit)
		  a)
		 ((string-contains s separator last-pos)
		  =>
		  (lambda (pos)
		     (let ((sep-len (string-length separator)))
			(js-property-set! a
					       (integer->string array-pos)
					       (substring s last-pos pos))
			(loop (+fx array-pos 1)
			      (+fx pos sep-len)))))
		 (else
		  (js-property-set! a
					 (integer->string array-pos)
					 (substring s last-pos len))
		  a))))
	  (else ;; regexp or array is not really array.
	   ;; completely fucked up:
	   ;;   1) we have strings and regexps at the same time.
	   ;;   2) we don't know if the 'a' is actually an array.
	   ;;
	   ;; - we don't know, if a.length actually returns a float.
	   ;;   -> reached-limit? deals with this.
	   ;; - according to the spec the elements should be stored at
	   ;;   the location of a.length. If a is an object, then
	   ;;   this property is probably unset, and we set the
	   ;;   "undefined" property. If it set, we will use the set value.
	   ;;   In any case: we will overwrite the last value again and again.
	   (let ((dummy 'dummy)) ;; need a let here for the defines
	      (define limit-fl (fixnum->flonum limit))
	      (define (reached-limit? len)
		 (and (flonum? len)
		      (=fl len limit-fl)))
	      (define (property-push! o v)
		 (let* ((o-length (js-property-get o "length"))
			(len-str (any->string o-length)))
		    (js-property-set! o len-str v)))
	      (define (split-match pos)
		 ;; returns 4 values: 1) did we find something? 2) start-pos
		 ;;   of matched 3) end-pos 4) a (string) scheme-list of
		 ;;   matched clusters.
		 (if (Js-RegExp? separator)
		     (let ((m (RegExp-exec separator s pos)))
			(if (js-null? m)
			    (values #f 0 0 '())
			    (values #t
				    (RegExp-res-start-pos m)
				    (RegExp-res-stop-pos m)
				    (RegExp-res-captures m s))))
		     (let ((m (string-contains s separator pos)))
			(if m
			    (values #t m (+fx m (string-length separator)) '())
			    (values #f 0 0 '())))))
	      (let loop ((last-pos 0)
			 (search-start-pos 0))
		 (receive (matched? from to captured)
		    (split-match search-start-pos)
		    (cond
		       ((not matched?)
			a)
		       ((=fx last-pos to) ;; implies empty match
			(loop last-pos (+fx search-start-pos 1)))
		       (else
			(property-push! a (substring s last-pos from))
			(if (reached-limit? (js-property-get a "length"))
			    a
			    (let luup ((captured captured))
			       (cond
				  ((null? captured)
				   (loop to to))
				  (else
				   (property-push! a (car captured))
				   (if (reached-limit?
					(js-property-get a "length"))
				       a
				       (luup (cdr captured)))))))))))))))))

(define (js-substring)                 ;; 15.5.4.15
   (js-fun
    this #f #f "String.prototype.substring"
    (start-any end-any)
    (let* ((str (if (Js-String? this)
		    (Js-String-str this)
		    (any->string this)))
	   (str-len (string-length str))
	   (str-len-fl (fixnum->flonum str-len))
	   ;; we have to do the min and max in floating point due to the
	   ;; infinite values... :(
	   (start (flonum->fixnum (min-2fl (max-2fl (any->integer start-any)
						    0.0)
					   str-len-fl)))
	   (end (if (js-undefined? end-any)
		    str-len
		    (flonum->fixnum (min-2fl (max-2fl (any->integer end-any)
						      0.0)
					     str-len-fl)))))
       (if (<fx start end)
	   (substring str start end)
	   (substring str end start)))))

(define (toLowerCase)                  ;; 15.5.4.16
   (js-fun
    this #f #f "String.prototype.toLowerCase"
    ()
    (let ((str (if (Js-String? this)
		   (Js-String-str this)
		   (any->string this))))
       (string-downcase str))))

(define (toLocaleLowerCase)            ;; 15.5.4.17
   (js-fun
    this #f #f "String.prototype.toLocaleLowerCase"
    ()
    (let ((str (if (Js-String? this)
		   (Js-String-str this)
		   (any->string this))))
       (string-downcase str))))

(define (toUpperCase)                  ;; 15.5.4.18
   (js-fun
    this #f #f "String.prototype.toUpperCase"
    ()
    (let ((str (if (Js-String? this)
		   (Js-String-str this)
		   (any->string this))))
       (string-upcase str))))

(define (toLocaleUpperCase)            ;; 15.5.4.19
   (js-fun
    this #f #f "String.prototype.toLocaleUpperCase"
    ()
    (let ((str (if (Js-String? this)
		   (Js-String-str this)
		   (any->string this))))
       (string-upcase str))))
