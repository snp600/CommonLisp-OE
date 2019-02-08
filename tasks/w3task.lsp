(defparameter arr (read))
(defparameter rating nil)

(defvar dim (array-dimensions arr))
(defvar ii (car dim))
(defvar jj (car (cdr dim)))
(defvar kk (caddr dim))

(dotimes (i ii)
	(dotimes (j jj)
		(dotimes (k kk)
		(let ((e (assoc (aref arr i j k) rating)))
			(if e
				(setf e (incf (cdr e)))
				(push (cons (aref arr i j k) 1) rating))))))

(defvar best 0)
(loop for x in rating 
	do (
		if (< best (cdr x))
		(setf best (cdr x))))
(print best)