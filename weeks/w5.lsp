;;;; lambda functions and closures
(defparameter *fn* #'+)
(funcall *fn* 1 20 21) ;42

(defun f (x) (* x x))

(defun func-coords (fn x0 xN h)
	(do ((x x0 (+ x h )) res)
		((> x xN) (nreverse res))
		(push (list x (funcall fn x)) res)))

(func-coords #'f 0 4 1) ;#' == (function *) ;((0 0) (1 1) (2 4) (3 9) (4 16))
(func-coords #'(lambda (x) (* x x)) 0 4 1) ;((0 0) (1 1) (2 4) (3 9) (4 16))


(setf f #'(lambda (x) (* x x)))
(funcall f 2) ;4

(defun compose (f1 f2)
	(lambda (x)
		(funcall f2 (funcall f1 x))))

;;random element of a sequence
(defun random-elt (seq)
	(elt seq (random (length seq))))
