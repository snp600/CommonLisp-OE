;;mapping
(defparameter *nums* '(1 2 3 4 5))
;;; we need a list of squares:
(let (acc)
	(dolist (i *nums*)
		(push (* i i) acc))
	(nreverse acc)
	(print acc))

(defun simple-map (fn lst)
	(let (acc)
		(dolist (i lst)
			(push (funcall fn i) acc))
		(nreverse acc)))
(print (simple-map #'(lambda (x) (* x x)) *nums*))
;; or just:
(print (map 'list #'abs '(-1 -2 13 -4)))

(defparameter *chars* #(#\a #\b #\c))
(print (map 'list #'(lambda (x y) (cons x y)) *chars* *nums*))

(setf *charss* '(#\a #\b #\c))
(print (mapcar #'(lambda (x y) (cons x y)) *charss* *nums*)) 
;(mapc #'(lambda (x y) (print (cons x y))) *charss* *nums*) 

;(maplist #'(lambda (x) (print x) (apply #'+ x)) *nums*) ;(15 14 12 9 5)

(mapcar #'(lambda (x y) (list x y)) '(1 2 3) '(4 5 6)) ;((1 4) (2 5) (3 6))
(mapcan #'(lambda (x y) (list x y)) '(1 2 3) '(4 5 6)) ;(1 4 2 5 3 6)
(mapcar #'(lambda (x) (if (evenp x) x nil)) '(1 2 3 4 5 6)) ;(NIL 2 NIL 4 NIL 6)
(mapcan #'(lambda (x) (if (evenp x) (list x) nil)) '(1 2 3 4 5 6)) ;(2 4 6)

(defparameter *a* '(0 0 0))
(map-into *a* #'+ '(1 2 3) '(20 40 50)) ;(21 42 53)

;;filter
;;;destructing analog for 'remove' is 'delete'
(remove 4 '(1 2 3 4 5 6 7)' :test #'<) ;(1 2 3 4)
(remove 4 '(1 2 3 4 5 6 7)' :test #'< :count 2) ;(1 2 3 4 7)
(remove "lisp" '("kek" "lisp" "lol") :test-not #'string-equal) ;("list")
(remove-if #'(lambda (x) (not (oddp x))) '(1 2 3 4 5)) ;(1 3 5)
(remove-if-not #'oddp '(1 2 3 4 5)) ;(1 3 5)
(remove-if #'(lambda (x) (> x 4)) '(1 2 3 4 5 6 7)) ;(1 2 3 4)
(remove-if-not #'(lambda (x) (> x 4)) '(1 2 3 4 5 6 7)) ;(5 6 7)

(substitute 42 24 '(24 0 24)) ;(42 0 42)
(substitute-if 42 #'oddp '(1 2 3 4 5)) ;(42 2 42 4 42)
(substitute-if-not 42 #'oddp '(1 2 3 4 5)) ;(1 42 3 42 5)

;;;find -- the first element that satisfies a criteria 
;;;position -- -//- but index
;;;member -- presense
;;;assoc -- search for an element by key
;;;rassoc -- -//- by value
;;;count -- amount of elements
;;;search -- subsequence

;;reduce
(reduce #'+ '(1 2 3 4 5)) ;15
(reduce #'- '(1 2 3 4 5) :from-end t) ;3
(defun map-on-reduce (fn lst) 
	(nreverse
		(reduce #'(lambda (acc x)
			(push (funcall fn x) acc))
			lst
			:initial-value nil)))
(map-on-reduce #'(lambda (x) (+ 1 x)) '(1 2 3 4)) ;(2 3 4 5)


;;count of prime numbers in a list:
(defparameter *max-random-num* 100)
(defun random-list (n)
	(let (test-list)
		(dotimes (i n)
			(push 
				(+ 1 (random *max-random-num*))
				test-list))
		test-list))

(defun factr (n &optional (res 1)) ;for Wilson's test
	(if (or (= n 0) (= n 1))
		res
		(factr (- n 1) (* res n))))

(defun primep (x) ; Wilson's test
	(= (- x 1) (mod (factr (- x 1) 1) x)))

(defparameter lst (random-list 10))
(reduce #'+ 
	(mapcar #'(lambda (x) (if x 1 0))
		(mapcar #'primep lst)))


;;;hashtable
(defparameter *ht* (make-hash-table :test #'equal))
(setf (gethash "test" *ht*) 42)
(gethash "test" *ht*) ;42
(remhash "test" *ht*)

(defun hash-table-key-value-pairs (h) 
	(let (pairs)
		(maphash #'(lambda (k v) 
			(push (list k v) pairs)) h)
		(nreverse pairs)))

(defun hash-table-add-list (h lst &aux k v)
	(loop
		(if (null lst) (return))
		(setf k (pop lst))
		(if (null lst) (return))
		(setf v (pop lst))
		(setf (gethash k h) v))
	h)

(defun set% (h key val &rest other-pairs)
	(let ((pairs (append (list key val) other-pairs)) k v)
		(hash-table-add-list h pairs)))

;;;sort
(defparameter lst '(20 4 6 8 0 3 1 7))
(sort lst #'>) ;(20 8 7 6 4 3 1 0)

(defparameter lst '(("177" 14) ("78" 30) ("98" 10)))
(sort lst #'(lambda (x y) (> (second x) (second y)))) ;(("78" 30) ("177" 14)  ("98" 10))
(sort lst #'> :key #'second) ;(("78" 30) ("177" 14)  ("98" 10))


;;;practical usage
; 1) delete all the non-private license plates: remove-if
; 2) list of numbers to list of regions: mapcar
; 3) number of violators from each region: reduce
; 4) sort 
(defparameter *vrps* '("a;1" "b;2" "c;3" "d;4" "e;2" "f;2" "g;4"))
(defun private-p (c)
	(and (find (elt c 0) "abcdfg" :test #'char-equal) t))

(defun get-privates-list (lst) 
	(remove-if ;remove-if-not is also can be used
		(complement #'private-p) ;complement -- returns opposite predicate
		lst))

(defun region-from-vrp (v)
	(subseq v (+ 1 (position #\; v))))

(defun get-regions-list (lst) 
	(mapcar #'region-from-vrp (get-privates-list lst)))

(defun hash++ (h key &optional (n 1))
	(let ((val (gethash key h)))
		(cond 
			((or (null val) (not (numberp val)))
				(set% h key n))
			(t (set% h key (+ val n))))
		(gethash key h)))

(defun get-region-rating (lst)
	(sort
		(hash-table-key-value-pairs
			(reduce 
				#'(lambda (h e) (hash++ h e) h)
				(get-regions-list lst)
				:initial-value (make-hash-table :test #'equal)))
	#'> :key #'second))

(print (get-region-rating *vrps*))
