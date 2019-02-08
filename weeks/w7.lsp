;;;;macros

;;nothing will be swapped
(defun swapf (x y)
	(let ((temp x))
		(setf x y)
		(setf y temp)))

(defmacro swapf (x y) 
	(list 'let (list (list 'temp x))
		(list 'setf x y) (list 'setf y 'temp)))

(defparameter a 10)
(defparameter b 10)
(print (macroexpand '(swapf a b)))

(list 'sum 'of a 'and b 'is (+ a b)) ;(SUM OF 20 AND 10 IS 30)
`(sum of ,a and ,b is ,(+ a b)) ;the same

(defmacro swapf-new (x y) ;;any form can be sent to a macro -- not only 'x'
	`(let ((temp ,x))
		(setf ,x ,y) (setf ,y temp)))

(truncate 3.5) ; 3;0.5
(multiple-value-bind (int frac) (truncate 3.5) (* int frac)) ;1.5

(defmacro mvb (vars-form func-form &body body)
	`(multiple-value-bind ,vars-form ,func-form ,@body))

(print (macroexpand-1 '(mvb (int frac) (truncate 3.5) (* int frac))))

;;some issues:
(defparameter temp 20)
(defparameter temb 15)
;(swapf temp temb) ;doesn't work because in macro we already have 'temp'
;;solution:
(defmacro swapf-new (x y) ;;any form can be sent to a macro -- not only 'x'
	(let ((temp (gensym)))
		`(let ((,temp ,x)) ; 'temp' with ',' !
			(setf ,x ,y) (setf ,y ,temp))))

(defmacro alambda (params &body body)
	`(labels ((self ,params ,@body))
		#'self))

(print 
	(funcall (alambda (n) 
		(if (> n 1) 
			(* n (self (- n 1)))
			1)) 
	3))

(print (macroexpand '(alambda (n) 
		(if (> n 1) 
			(* n (self (- n 1)))
			1))))

;;loop macro
;(dotimes (i 7) (print i))
;(loop for i from 1 to 7 by 2 do (print i))
;(loop repeat 3 do (print "kek"))
;(dolist (i '(1 2 3 4)) (print i))
(defparameter *arr* #(10 20 30 40))
;(loop for x in '(1 2 3 4) by #'cddr do (print x))
;(loop for x across *arr* do (print x))
;(loop for x = 33 then (random 10) do 
;	(if (zerop x) (return) (print x)))

(loop repeat 2 for x = (random 100)
	sum x into x-sum
	count x into x-count
	minimize x into x-min
	maximize x into x-max
	do (print x)
	finally (format t "~%~a ~a ~a ~a~%" x-sum x-count x-min x-max))

(loop repeat 50 for x = (random 100)
	count (oddp x) into x-odds
	finally (return x-odds))

(loop repeat 5 for x = (random 100) collect x) ;returns a list of x's
(loop for x across "test" collect x) ;returns (#\t #\e #\s #\t)

;(loop repeat 5 for x = (random 100)
;	when (oddp x) do (print x)) ;or unless 

(loop for x in '(1 3 5) always (oddp x)) ;t ;never, thereis

(defmacro with-gensyms- (names &body body)
	`(let ,(loop for n in names collect `(,n (gensym)))
		,@body))

(defmacro with-gensyms-init (names init-vals &body body)
	`(with-gensyms- (,@names)
		`(let (,,@(loop for v in init-vals for n in names 
			collect ``(,,n  ,,v)))
		,,@body)))

(print (macroexpand '(with-gensyms-init ($x $y $z) (10 30 20) (print $x))))