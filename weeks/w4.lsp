;;; Functions

(defun mul (x y z)
	(* x y z))
(mul 2 3 4) ;24

;;sum of arr's elements
(defun sum-n (arr n)
	"Documentation"
	(let ((res 0))
		(dotimes (i n)
			(incf res (elt arr i))) ;or 'aref' instead of 'elt'
		res))

(print (documentation 'sum-n 'function)) ;"Documentation"

(defun add (x &optional (y 1337))
	(+ x y))
(add 1) ;1338

(defun func (a1 a2 &rest r)
	(list a1 a2 r))
(func 1 2 3 4 5) ;(1 2 (3 4 5))

(defun my-plus (&rest r)
	(let ((res 0))
		(dolist (arg r)
			(incf res arg))
		res))
(my-plus 1 2 3 4 5 6) ;21

(defun func-key (&key (k1 0) (k2 11))
	(list k1 k2))
(func-key :k2 1337) ;(0 1337)

(print (let ((x (random 100)) (y (random 100)))
	(list x y))) ;e.g. (16 18)

(defun square-eqn-roots (a b c)
	(let* ((DD (- (* b b) (* 4 a c)))
			(x1 (/ (+ (- b) (sqrt DD)) (* 2 a)))
			(x2 (/ (- (- b) (sqrt DD)) (* 2 a))))
			(list x1 x2)))
(square-eqn-roots 1 0 -4) ;(2 -2)

(defun sqr (a b c &aux (DD (- (* b b) (* 4 a c))))
	(list 	(/ (+ (- b) (sqrt DD)) (* 2 a)) 
			(/ (- (- b) (sqrt DD)) (* 2 a))))
(sqr 1 0 -4) ;(2 -2)

;(destructuring-bind (x1 x2) (sqr 1 0 -4) (format t "~%x1 = ~a, x2 = ~a" x1 x2)) ;x1 = 2, x2 = -2

(defun sqr2 (a b c)
	(labels ((discr (a b c)
				(- (* b b) (* 4 a c)))
			(root (a b c func)
				(/ 
					(funcall func (- b) (sqrt (discr a b c)))
					(* 2 a)
				)))
		(list 
			(root a b c #'+)
			(root a b c #'-)
		)))
(sqr2 1 0 -4) ;(2 -2)

;;;returns
(dolist (i '(1 2 3 4 5))
	(if (= i 3) (return)))
;	(print i))

(defun count-odds (lst)
	(let ((count 0) (odds-lst nil))
		(dolist (i lst)
			(if (oddp i)
				(push i odds-lst)
				(incf count)))
		(values count (nreverse odds-lst))))

;(multiple-value-bind (x1 x2) (count-odds '(1 2 3 4)) (print x2)) ;(1 3)
;(multiple-value-bind (x1 x2) (count-odds '(1 2 3 4)) (print x1)) ;2

;;;recursion
