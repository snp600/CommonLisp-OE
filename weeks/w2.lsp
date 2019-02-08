;;;; comment in the beginning of a file
;;; before function
;; in function
; in the end of a row
#|
	lol kek chebureck
|#
(random 100)
(print (+ 111 222))
(format t "~r" (+ 111 222))
(machine-type)
(defun print-n-randoms (n max-limit)
    (dotimes (i n)
	(print (random max-limit))))
(print-n-randoms 10 10)

(if (= 4 (* 2 2))
    (print "lol")
    (print "kek"))

(when (= 4 (* 2 2))
    (print "lol")
    (print "kek"))
;unless -- the same as when (but vice versa)
(defvar *x* 1337) ;global
(setf x 228) 
(print x)
(oddp 7) ;true
(abs -10)
(expt 10 2)
(truncate 12.34) ;12 and 0.34
(rem 12 10) ;2


(print #2R1000) ;8 #b #o #x
(print (+ 3/5 1/7))
(format t "~c" #\NOT_EQUAL_TO)

(char-upcase #\a)
(char-downcase #\A)
(char-invertcase #\A)
(char-code #\a)
(code-char 97)
(quote (1 2 3))

(print (format nil "~b" #xABCF))

(defvar x 42)
(incf x 10) ;52

(floor -7.5) ;-8
(ceiling -7.5) ;-7

(ash 1 1) ;2 -- shift

(setf n 3)
(loop
    (unless (>= n 0) (return))
    (print n)
    (decf n))
