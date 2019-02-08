;;;;input/output

(print "Hello" *error-output*)
(progn (prin1 "1") (prin1 "2")) ;"1""2"
(progn (prin1 "1") (terpri) (prin1 "2")) ;1 \n 2

;(defparameter ch (read-char))
;(defparameter s (read-line))

(defparameter *str-out* (make-string-output-stream))
(progn (princ 42 *str-out*)
	(princ 42 *str-out*)
	(princ 42 *str-out*)
	(princ 42 *str-out*)
	(princ 42 *str-out*))
(get-output-stream-string *str-out*) ;4242424242

(defparameter *str-in* (make-string-input-stream "42 3.14"))
(read *str-in*) ;42
(read *str-in*) ;3.14

(with-input-from-string (s "111 42 3.14" :start 3)
	(+ (read s) (read s))) ;45.14

;;; FORMAT
(format t "~a" "test") ;test
(format t "~s" "test") ;"test"
(format t "~a~%~%~a~&~a" "first" "second" "third") ;~% -- \n, ~& -- (fresh-line)

(format t "~&~d ~:*~o ~:*~x ~:*~b" 42) ;~:* -- takes previous argument

(format t "~10d" 42) ;        42
(format t "~10,'_d" 42) ;_______42

(multiple-value-list (get-decoded-time)) ;(0 24 23 7 5 2016 0 NIL -3)
(apply #'format t "~&~*~*~*~2,'0d.~2,'0d.~d"(multiple-value-list (get-decoded-time))) ;~* -- pass one argument

(format nil "~3r" 42) ;"1120" ternary system
(format nil "~@r" 42) ;"XLII"
(format nil "~r" 42) ;"forty-two"
(format nil "~:r" 42) ;"forty-second"
(format t "~:r" 1337) ;one thousand, three hundrer thirty-seventh

(format t "~&~,2f" pi) ;3.14
(format t "~&~,2e" pi) ;3.14L+0

(format t "~&~[1~;2~;3~;4~]" (random 4))
(format t "~&~d is ~:[NOT~;indeed~] an odd" 3 (oddp 3))

(defparameter *count-down* `(3 2 1))
(format t "~&~{~r...~%~}poehali!~&" *count-down*)

(defparameter *pac* `(1 2 3))
(format t "~&~{~dpac~%~}none!~&" *pac*)

;;; fio/fout

(with-open-file (out "~/Avanta/alchemy/Lisp/test.txt"
					:direction :output
					:if-exists :append) ;supersede -- rewrite
	(loop for x = (random 100)
		repeat 20 do
			(format out "~,3d " x)))

(with-open-file (in "~/Avanta/alchemy/Lisp/test.txt")
	(loop for x = (read in nil) while x do
		(format t "~d " x)))
