#|
(defun symmp (n)
	(if (and 
		(< 10 n)
		(let ((a n) (b 0))
			(loop
				(if (= a 0) (return))
				(setf b (+ (* 10 b) (mod a 10)))
				(setf a (truncate a 10)))
			(if (equal n b) t))) t))
|#
(defun factr (n &optional (res 1)) ;for Wilson's test
	(if (or (= n 0) (= n 1))
		res
		(factr (- n 1) (* res n))))

(defun primep (x) ; Wilson's test
	(= (- x 1) (mod (factr (- x 1) 1) x)))
#|
(defmacro do-sym-nums ((var startv endv) &body body)
	`(loop for x from ,startv to ,endv do 
		;(if (symmp x) 
		(if (primep x)
			,@body)
		))
|#
(defun find-next-prime (n endv)
	(let ((x n))
		(loop for i from n to endv do
			(if (primep i)
				(progn 
					(setf x i)
					(return))))
		x))

(defmacro do-primes ((var startv endv) &body body)
	`(let ((acc ,startv))
		(loop
			(setf ,var (find-next-prime acc ,endv))
			,@body
			(setf acc (+ 1 ,var))
			(if (> acc ,endv) (return))
		)))

;(print (macroexpand-1 `(do-sym-nums (x 4 11) (print x))))
(do-primes (x 3 11) (print x))