(defparameter tree (read))
;(defvar tree '(3 (14 (15 nil nil) (7 nil nil)) (5 (8 nil nil) (21 nil nil))))

(defun val (tree)
	(car tree))
(defun left (tree)
	(car (cdr tree)))
(defun right (tree)
	(car (cdr (cdr tree))))

(defun magic (tree &optional (flag 1) (res 0))
	(when (and (oddp flag) (= (rem (val tree) 7) 0)) (incf res (val tree)))
	(incf flag)
	(if (left tree)
		(setf res (magic(left tree) flag res)))
	(if (right tree)
		(setf res (magic(right tree) flag res)))
	res)
(print (magic tree))