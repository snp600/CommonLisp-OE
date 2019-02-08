;;;reader macros -- before compilation
(set-macro-character #\$
	#'(lambda (stream c)
		(list 'quote (read stream nil t))))
(eq 'a $a) ;t
(eq (quote a) $a) ;t

(get-macro-character #\') ;#<FUNCTION (LAMBDA (STREAM C) {***}>)

(set-macro-character #\]
	(get-macro-character #\)))

(set-dispatch-macro-character #\# #\[
	#'(lambda (stream c1 c2)
		(declare (ingore c1 c2))
		(let ((lst (read-delimited-list #\] stream t)))
			`(quote
				,(loop for x from (first lst) to (second lst)
					by (or (third lst) 1) collect x)))))

(print #[1 10 2]) ;(1 3 5 7 9)


;;;lazy evaluation
(defmacro lazy (&body body)
	(let ((forced (gensym)) (value (gensym)))
		`(let ((,forced nil ) (,value nil))
			#'(lambda ()
				(unless ,forced 
					(setf ,value (progn ,@body))
					(setf ,forced t))
				,value))))
(defun force (x)
	(funcall x))

(defparameter expr (lazy (loop for x from 1 to 5 collect x)))

(force expr) ;(1 2 3 4 5)