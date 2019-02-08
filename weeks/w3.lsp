;;;; Lists/Arrays

;;;ARRAYS
(defvar a (make-array '(3 3)))
(aref a 1 1) ; a[1][1]
(setf (aref a 1 1) 42)

(setf a (make-array 3
	:initial-element 42))

(setf a (make-array 5
	:initial-contents '(1 2 3 4 5)))


(setf a (make-array '(3 2)))
(array-rank a) ;2
(array-dimensions a) ;(3 2)
(array-total-size a) ;6

;;stack
(setf arr 
	(make-array 3 :fill-pointer 0))
(vector-push 42 arr)
(vector-push "kek" arr)
(vector-pop arr)
(fill-pointer arr) ;1
(vector-push-extend 42 arr 1) ;add 3 cells

(setf lul (
	make-array '(2 2)
	:initial-contents '((1 2) (3 4))))
(block outer
(dotimes (i 2)
	(dotimes (j 2)
		(when (= 3 (aref lul i j))
			(return-from outer
				(print j))))))

;;;STRINGS
(defvar str "kek")
(length str)
(setf s (copy-seq str))
(defvar k (concatenate 'string "lol" "kek"))
(string-upcase "high")
(string-downcase "LOW")
(string-capitalize "gagarin")
(reverse "puchka")

(string-equal "itmo" "ITMO") ;T
(string= "itmo" "ITMO") ;NIL
(string< "ITMO" "IFMO") ;NIL
(print (search "ri" "gagarin"))
(remove #\."K.E.K.") ;KEK
(string-trim " " "  asdf  ") ;asdf
(substitute #\  #\. "I.T.M.O.") ;I T M O

(write-to-string 42) ;"42"

(parse-integer "42")
(print (read-from-string "3.14")) ;3.14

;;;LISTS
(cons 33 nil) ;list's cell
(cons 11 (cons 22 (cons 33 nil))) ;--bad
(quote (11 22 33)) ;--better
(list 11 22 33) ;the same

(defvar lst '(11 22 33))
(car lst) ;11
(cdr lst) ;(22 33)
(setf (nth 2 lst) 44) ;(11 22 44)

(setf lst '(11))
(dotimes (i 5)
	(push (* i 3) lst))
(setf lst (nreverse lst))
;(print lst) ;(11 0 3 6 9 12)

;;;MAPS
(setf *alist* '((1 . "one") (2 . "two")))
(assoc 2 *alist*) ;(2 . "two")

(defparameter arr #(1 2 1 3 1 4 1 3 4 1 5))
(defparameter alist nil)
(dotimes (i (length arr))
	(let ((e (assoc (aref arr i) alist)))
		(if e
			(setf e (incf (cdr e)))
			(push (cons (aref arr i) 1) alist))))
(print alist) ;((5 . 1) (4 . 2) (3 . 2 (2 . 1) (1 . 5))

(defvar *set* nil)
(setf *set* (adjoin 1 *set*))
(setf *set* (adjoin 2 *set*))
(setf *set* (adjoin 1 *set*)) ;nothing will happen

(setf *set* '(1 2 3 4 5))
(member 3 *set*) ;(3 4 5)
(subsetp '(1 2 3) *set*) ;T
(intersection '(1 7) *set*) ;(1)
(union '(1 2) '(2 3)) ;(1 2 3)
(set-difference '(1 2 3 4 5) '(1 3 5)) ;2 4
(set-exclusive-or '(1 2 3) '(3 4)) ;(1 2 4)