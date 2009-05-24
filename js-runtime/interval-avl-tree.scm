(module interval-avl-tree
   (static
    (final-class Node
       ;; the interval the Node is covering. might have holes.
       (iv::Interval read-only)
       (l read-only)
       (r read-only)
       (l-height::long read-only)
       (r-height::long read-only))
    (final-class Interval
       from::long
       to::long)
    (final-class Iterator
       waiting-nodes::pair-nil
       current-iv)) ;; #f if finished or empty
   (export (make-empty-interval-avl)
	   (interval-avl-subset? avl1 avl2)
	   (interval-avl-overlap? avl1 avl2)
	   (interval-avl-insert avl from::long to::long)
	   (interval-avl-single-n avl)
	   (interval-avl-out avl)
	   (interval-avl-contains? avl n)
	   (interval-avl-invert avl min max)
	   (interval-avl-union avl1 avl2)))

;; functional implementation of an AVL tree specialized for intervals.
;; Nodes do not carry entries. They contain intervals, but these intervals are
;; not necessarily "full". If a Node contains interval [0, 9] it might have a
;; hole (like [4, 6]). However it guarantees that no value outside the interval
;; is contained inside the tree. (In the previous example 10 could not be
;; contained in the tree).
;; The implementation is functional (every field is read-only).
;; A tree without any elements == #f
;; A (sub-)tree with just one interval is the interval itself (of type
;;  ::Interval). This is the reason 'l' and 'r' are not typed as "Node".
;; Obviously this all might change, but as of 20090510 this is the case.

(define-macro (+fx+ x . L)
   (if (null? L)
       x
       `(+fx x (+fx+ ,@L))))

(define (make-empty-interval-avl) #f)

(define (height avl)
   (cond
      ((Node? avl)
       (+fx (maxfx (Node-l-height avl) (Node-r-height avl)) 1))
      ((not avl) 0)
      (else 1)))

(define (node-from::long n::Node) (Interval-from (Node-iv n)))
(define (node-to::long n::Node) (Interval-to (Node-iv n)))

;; avl must not be #f
(define (avl-from avl)
   (cond
      ((Interval? avl) (Interval-from avl))
      ((Node? avl) (avl-from (Node-iv avl)))
      (else (error "avl-from"
		   "internal error. avl must be either Interval or Node"
		   avl))))
(define (avl-to avl)
   (cond
      ((Interval? avl) (Interval-to avl))
      ((Node? avl) (avl-to (Node-iv avl)))
      (else (error "avl-to"
		   "internal error. avl must be either Interval or Node"
		   avl))))

;; iv-union is for intersecting/bordering intervals only!
(define (interval-union iv1::Interval iv2::Interval)
   (if (interval=? iv1 iv2)
       iv1
       (make-Interval (minfx (Interval-from iv1) (Interval-from iv2))
		      (maxfx (Interval-to iv1) (Interval-to iv2)))))

(define (interval=? iv1::Interval iv2::Interval)
   (and (=fx (Interval-from iv1) (Interval-from iv2))
	(=fx (Interval-to iv1) (Interval-to iv2))))

(define (intervals-intersect?::bool iv1::Interval iv2::Interval)
   (let ((i1- (Interval-from iv1))
	 (i1+ (Interval-to iv1))
	 (i2- (Interval-from iv2)))
      (if (>fx i1- i2-)
	  (intervals-intersect? iv2 iv1)
	  (<=fx i2- i1+))))

(define (intervals-border?::bool iv1::Interval iv2::Interval)
   (let ((i1- (Interval-from iv1))
	 (i1+ (Interval-to iv1))
	 (i2- (Interval-from iv2)))
      (if (>fx i1- i2-)
	  (intervals-border? iv2 iv1)
	  ;; pay attention for overflows... (that's why there's the first test.
	  (and (<fx i1+ i2-)
	       (=fx (+fx i1+ 1) i2-)))))

;; neither intersecting nor bordering.
(define (intervals-far-disjoint?::bool iv1::Interval iv2::Interval)
   (let ((i1- (Interval-from iv1))
	 (i1+ (Interval-to iv1))
	 (i2- (Interval-from iv2)))
      (if (>fx i1- i2-)
	  (intervals-far-disjoint? iv2 iv1)
	  (>fx (-fx i2- i1+) 1))))
   
(define (combine-trees l r)
   (define (avl-iv-union avl1 avl2)
      (cond
	 ((Node? avl1) (avl-iv-union (Node-iv avl1) avl2))
	 ((Node? avl2) (avl-iv-union avl1 (Node-iv avl2)))
	 (else (interval-union avl1 avl2))))

   (instantiate::Node
      (iv (avl-iv-union l r))
      (l l)
      (r r)
      (l-height (height l))
      (r-height (height r))))

;; merge two intervals.
;; Either creates a new merged interval or a new Node with both intervals as
;; children.
(define (merge-intervals iv1::Interval iv2::Interval)
   (let ((i1- (Interval-from iv1))
	 (i1+ (Interval-to iv1))
	 (i2- (Interval-from iv2))
	 (i2+ (Interval-to iv2)))
      (cond
	 ((>fx i1- i2-)
	  (merge-intervals iv2 iv1))
	 ;; from now on: i1- <= i2-
	 ((<=fx i2+ i1+) ;; fully contained
	  iv1)
	 ((<=fx i2- i1+) ;; overlap
	  (make-Interval i1- i2+))
	 ((=fx (+fx i1+ 1) i2-) ;; bordering
	  (make-Interval i1- i2+))
	 (else ;; disjount
	  (combine-trees iv1 iv2)))))

(define (interval-subset? iv::Interval avl)
   (with-access::Interval iv (from to)
      (cond
	 ((not avl) #f)
	 ((Interval? avl)
	  (let ((avl- (Interval-from avl))
		(avl+ (Interval-to avl)))
	     (and (>=fx from avl-) (<=fx to avl+))))
	 ((Node? avl)
	  (let ((n- (node-from avl))
		(n+ (node-to avl)))
	     (and (>=fx from n-) (<=fx to n+)
		  (or (interval-subset? iv (Node-l avl))
		      (interval-subset? iv (Node-r avl))))))
	 (else (error "interval-subset?"
		      "bad avl-tree"
		      avl)))))

(define (interval-avl-insert avl from::long to::long)
   (interval-avl-insert-iv avl (make-Interval from to)))

(define (interval-avl-insert-iv avl iv::Interval)
   (cond
      ((not avl) iv)
      ((Interval? avl)
       (merge-intervals avl iv))
      ((interval-subset? iv avl)
       ;; do nothing
       avl)
      ((Node? avl)
       ;; first remove all intersecting and bordering interval that are
       ;; inside the tree. Than insert the combined interval.
       ;; E.g: suppose [2,4] is inside the tree and we insert 5 now
       ;;    then [2,4] is removed and a new interval [2,5] is inserted.
       ;;  Or suppose [3,9] and [11,13] are in the tree and we insert
       ;;  [5,12]. Then [3,9], [11,13] are deleted first and [3,13] is
       ;;  inserted.
       (receive (pruned-avl interval)
	  (remove-intersecting/bordering avl iv)
	  (safe-insert pruned-avl interval)))
      (else (error "interval-avl-insert" "bad tree" avl))))

;; precondition: iv does not intersect or border with any other interval in the
;; tree.
(define (safe-insert avl iv::Interval)
   (cond
      ((not avl) iv)
      ((Interval? avl) (merge-intervals avl iv))
      ((not (Node? avl))
       (error "safe-insert" "internal error. bad avl-tree" avl))
      ((<fx (Interval-to iv) (avl-to (Node-l avl)))
       ;; iv must be inserted on the left.
       (rebalance-tree2 (safe-insert (Node-l avl) iv)
			(Node-r avl)))
      ((>fx (Interval-from iv) (avl-to (Node-r avl)))
       ;; iv must be inserted on the right.
       (rebalance-tree2 (Node-l avl)
			(safe-insert (Node-r avl) iv)))
      ((>fx (height (Node-l avl)) (height (Node-r avl)))
       ;; left tree is bigger than the right one.
       ;; insert into the right.
       (rebalance-tree2 (Node-l avl)
			(safe-insert (Node-r avl) iv)))
      (else ;; insert into the left.
       (rebalance-tree2 (safe-insert (Node-l avl) iv)
			(Node-r avl)))))

(define (remove-intersecting/bordering avl iv::Interval)
   (cond
      ((or (and (Node? avl) (intervals-far-disjoint? (Node-iv avl) iv))
	   (and (Interval? avl) (intervals-far-disjoint? avl iv)))
       ;; nothing to do. they are not intersecting or bordering.
       (values avl iv))
      ((Interval? avl)
       ;; they must intersect or border (otherwise previous test would have
       ;; matched). Remove the old interval (by returning #f) and return union
       ;; of intervals.
       (values #f (interval-union iv avl)))
      ((Node? avl)
       (receive (pruned-l iv-l)
	  (remove-intersecting/bordering (Node-l avl) iv)
	  (receive (pruned-r iv-r)
	     (remove-intersecting/bordering (Node-r avl) iv)
	     (let ((new-iv (interval-union iv-l iv-r))
		   (old-l (Node-l avl))
		   (old-r (Node-r avl)))
		(cond
		   ((and (eq? pruned-l old-l) (eq? pruned-r old-r))
		    ;; do nothing. shortcut if nothing changed.
		    (values avl new-iv))
		   ;; next two cases handle completely removed subnodes.
		   ((not pruned-l) (values pruned-r new-iv))
		   ((not pruned-r) (values pruned-l new-iv))

		   (else ;; balanced or unbalanced.
		    (values (rebalance-tree2 pruned-l pruned-r) new-iv)))))))
      (else
       (error "remove intersecting/bordering"
	      "bad avl-tree"
	      avl))))

(define (rebalance-tree2 l r)
   (define (heavy-left? n::Node)
      (>fx (-fx (Node-l-height n) (Node-r-height n)) 2))
   (define (heavy-right? n::Node)
      (<fx (-fx (Node-l-height n) (Node-r-height n)) 2))
   (define (heavy-left2? l r)
      (>fx (-fx (height l) (height r)) 2))
   (define (heavy-right2? l r)
      (<fx (-fx (height l) (height r)) 2))

   (cond
      ((not (or (heavy-left2? l r) (heavy-right2? l r)))
       ;; actually balanced.
       (combine-trees l r))
      ((and (heavy-left2? l r) (heavy-right? l))
       (rebalance-tree2 (rotate-left l) r))
      ((and (heavy-right2? l r) (heavy-left? r))
       (rebalance-tree2 l (rotate-right r)))
      ((heavy-left2? l r)
       (rotate-right-and-rebalance l r))
      ((heavy-right2? l r)
       (rotate-left-and-rebalance l r))))

(define (rebalance-tree n::Node)
   (define (balanced? n::Node)
      (<fx (absfx (-fx (Node-l-height n) (Node-r-height n))) 2))
   (if (balanced? n)
       n
       (rebalance-tree2 (Node-l n) (Node-r n))))

(define (rotate-left n::Node)
   (with-access::Node n (l r)
      (let ((new-left (combine-trees l (Node-l r)))
	    (new-right (Node-r r)))
	 (combine-trees new-left new-right))))
(define (rotate-right n::Node)
   (with-access::Node n (l r)
      (let ((new-left (Node-l l))
	    (new-right (combine-trees (Node-r l) r)))
	 (combine-trees new-left new-right))))

;; not just (rebalance (rotate-left ...)) but rebalances the sub-trees too.
(define (rotate-left-and-rebalance l r)
   (let ((new-left (rebalance-tree2 l (Node-l r)))
	 (new-right (Node-r r)))
      (rebalance-tree2 new-left new-right)))
(define (rotate-right-and-rebalance l r)
   (let ((new-left (Node-l l))
	 (new-right (rebalance-tree2 (Node-r l) r)))
      (rebalance-tree2 new-left new-right)))

;; works with avl=#f too.
(define (left-most-iv avl waiting-nodes)
   (cond
      ((Node? avl)
       (left-most-iv (Node-l avl) (cons avl waiting-nodes)))
      (else (values avl waiting-nodes))))

(define (advance-iterator it)
   (with-access::Iterator it (waiting-nodes)
      (cond
	 ((null? waiting-nodes)
	  (values #f '()))
	 (else
	  (left-most-iv (Node-r (car waiting-nodes)) (cdr waiting-nodes))))))
	  
(define (interval-iterator avl)
   (receive (current waiting)
      (left-most-iv avl '())
      (instantiate::Iterator
	 (waiting-nodes waiting)
	 (current-iv avl))))

(define (peek it::Iterator)
   (Iterator-current-iv it))
(define (next::Iterator it::Iterator)
   (with-access::Iterator it (waiting-nodes current-iv)
      (receive (current waiting)
	 (advance-iterator it)
	 (instantiate::Iterator
	    (current-iv current)
	    (waiting-nodes waiting)))))
(define (next! it::Iterator)
   (with-access::Iterator it (waiting-nodes current-iv)
      (let ((tmp current-iv))
	 (receive (current waiting)
	    (advance-iterator it)
	    (set! current-iv current)
	    (set! waiting-nodes waiting)
	    tmp))))

(define (interval-avl-union avl1 avl2)
   (if (>fx (height avl1) (height avl2))
       (interval-avl-union avl2 avl1)
       (let loop ((it (interval-iterator avl1))
		  (res-avl avl2))
	  (let ((iv (peek it)))
	     (if (not iv)
		 res-avl
		 (loop (next it)
		       (interval-avl-insert-iv res-avl iv)))))))


(define (interval-avl-invert avl min max)
   (let loop ((it (interval-iterator avl))
	      (res-avl (make-empty-interval-avl))
	      (last-to -1)
	      (first? #t))
      (let ((iv (peek it)))
	 (cond
	    ((and (not iv) (=fx last-to max))
	     ;; was already reaching max. no need to add an interval.
	     res-avl)
	    ((not iv)
	     (interval-avl-insert res-avl (+fx last-to 1) max))
	    ((and first? (=fx (Interval-from iv) min))
	     ;; was already reaching min. no need to add an interval.
	     (loop (next it)
		   res-avl
		   (Interval-to iv)
		   #f))
	    (first?
	     (loop (next it)
		   (interval-avl-insert res-avl min (-fx (Interval-from iv) 1))
		   (Interval-to iv)
		   #f))
	    (else
	     (with-access::Interval iv (from to)
		(loop (next it)
		      (interval-avl-insert res-avl
					   (+fx last-to 1)
					   (-fx from 1))
		      to
		      #f)))))))

(define (interval-avl-contains? avl n)
   (cond
      ((not avl)       #f)
      ((Interval? avl) (and (>=fx n (Interval-from avl))
			    (<=fx n (Interval-to avl))))
      ((Node? avl)     (or (interval-avl-contains? (Node-l avl) n)
			   (interval-avl-contains? (Node-r avl) n)))
      (else
       (error "interval-avl-contains"
	      "bad avl tree"
	      avl))))

;; is avl1 a subset of avl2?
(define (interval-avl-subset? avl1 avl2)
   (let loop ((it1 (interval-iterator avl1))
	      (it2 (interval-iterator avl2)))
      (let ((iv1 (peek it1))
	    (iv2 (peek it2)))
	 (cond
	    ((not iv1) #t)
	    ((not iv2) #f)
	    ;; not that we don't allow bordering intervals (like [1,2][3,9].
	    ;; we can hence avoid many complicated cases.
	    ((>fx (Interval-from iv2) (Interval-from iv1))
	     #f)
	    ((<fx (Interval-to iv2) (Interval-to iv1))
	     (loop it1 (next it2)))
	    ;; if we are here than iv1 is a subset of iv2.
	    (else
	     (loop (next it1) it2))))))

;; do avl1 and avl2 overlap? is the intersection non-null?
(define (interval-avl-overlap? avl1 avl2)
   (let loop ((it1 (interval-iterator avl1))
	      (it2 (interval-iterator avl2)))
      (let ((iv1 (peek it1))
	    (iv2 (peek it2)))
	 (cond
	    ((not iv1) #f)
	    ((not iv2) #f)
	    ((>fx (Interval-from iv1) (Interval-to iv2))
	     ;; iv1 is completely to the right of iv2
	     (loop it1 (next it2)))
	    ((>fx (Interval-from iv2) (Interval-to iv1))
	     ;; iv2 is completely to the right of iv1
	     (loop (next it1) it2))
	    (else ;; neither is to the right of the other -> they must
	     ;; intercept.
	     #t)))))

;; returns the only contained 'n' or #f if there are 0 or more than 1 ns in the
;; tree.
(define (interval-avl-single-n avl)
   (cond
      ((not avl) #f)
      ((Node? avl) #f)
      ((Interval? avl)
       (with-access::Interval avl (from to)
	  (and (=fx from to) from)))
      (else
       (error "interval-avl-single-n"
	      "bad interval avl tree"
	      avl))))

(define (interval-avl-out avl)
   (let loop ((it (interval-iterator avl))
	      (rev-res '()))
      (let ((iv (peek it)))
	 (cond
	    ((not iv) (reverse! rev-res))
	    (else (loop (next it)
			(cons (cons (Interval-from iv) (Interval-to iv))
			      rev-res)))))))
