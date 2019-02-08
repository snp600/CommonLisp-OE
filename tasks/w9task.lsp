(defun factr (n &optional (res 1)) ;for Wilson's test
	(if (or (= n 0) (= n 1))
		res
		(factr (- n 1) (* res n))))

(defun primep (x) ; Wilson's test
	(if (> x 1)
		(= (- x 1) (mod (factr (- x 1) 1) x))))

(set-macro-character #\]
	(get-macro-character #\)))

(set-dispatch-macro-character #\# #\[
	#'(lambda (stream c1 c2)
		(declare (ingore c1 c2))
		(let ((lst (read-delimited-list #\] stream t)))
			`(quote
				,(if (and (> (first lst) -1) (> (second lst) -1))
					(loop for x from (first lst) to (second lst)
						when (primep x) collect x))))))

(print #[10 20])