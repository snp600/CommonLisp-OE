<img src="https://habrastorage.org/files/7d4/1a1/bc3/7d41a1bc3b2c4463bf8364f24fd3ed5d.png" width="150" height="150">

# Common Lisp overview 
[(view on hackmd)](https://hackmd.io/tzHUdJKaQx6tOEhaXVEpsw)
## Section 1
### Math basics
```lisp
(oddp 7) ;t
(evenp 7) ;nil
(floor -7.5) ;-8
(ceiling -7.5) ;-7
(abs -5) ;5 
(expt 5 3) ;125
(rem 17 10) ;7
(truncate 17.10) ;17
(print #2R101) ;5 #b #o #x
(print (+ 3/5 1/7)) ;26/35
(print (format nil "~b" #xABCF)) ;"1010101111001111"
```

### Chars
```lisp
(char-upcase #\a) ;A
(char-downcase #\A) ;a
(char-invertcase #\A) ;a
(char-code #\a) ;97
(code-char 97) ;a
```

### Comments

```lisp
;;;; comment in the beginning of a file
;;; comment before a function
;; comment in function
; comment in the end of a row
```


### Example 1: Lucky tickets

```lisp
(defun nth-digit(num n)
    (rem (truncate (/ num (expt 10 n))) 10))

(defun first-three (n)
    (+ (nth-digit n 3) (nth-digit n 4) (nth-digit n 5)))

(defun last-three (n)
    (+ (nth-digit n 0) (nth-digit n 1) (nth-digit n 2)))

(defun lucky (n)
    (if (= (first-three n) (last-three n))
       (print "True")
       (print "False")))

(lucky 752770) ;"True"
(lucky 111222) ;"False"
```

## Section 2
```lisp
(defvar *x* 1337) ;global value
(setf *x* 15)
```

### Arrays
```lisp
(defvar a (make-array `(3 3)))
(aref a 1 1) ;a[1][1]
(setf (aref a 1 1) 42) ;a[1][1] = 42

(setf a (make-array 3
    :initial-element 42))
(setf a (make-array 5
    :initial-contents '(1 2 3 4 5)))
(setf b (
    make-array '(2 2)
    :initial-contents '((1 2) (3 4))))
```

### Stack
```lisp
(setf arr (make-array 3 :fill-pointer 0))
(vector-push 42 arr)
(vector-push "grizlik" arr)
(vector-pop arr)
(fill-pointer arr) ;1
```


### String

```lisp
(length "pumba")
(setf s (copy-seq str))
(concatenate 'string "bit" "coin") ;"bitcoin"
(string-upcase "high") "HIGH"
(string-downcase "LOW") ;"low"
(string-capitalize "gagarin") ;"Gagarin"
(reverse "puchka")

(string-equal "itmo" "ITMO") ;T
(string= "itmo" "ITMO") ;NIL
(string< "ITMO" "IFMO") ;NIL
(search "ri" "gagarin") ;4 
(remove #\."M.I.T.") ;MIT
(string-trim " " "  asdf  ") ;asdf
(substitute #\  #\. "I.T.M.O.") ;I T M O

(write-to-string 42) ;"42"
(parse-integer "42") ;42
(read-from-string "3.14") 3.14
```

### List
```lisp
(cons 33 nil) ;list's cell
(cons 11 (cons 22 (cons 33 nil))) ;bad
(quote (11 22 33)) ;better
(list 11 22 33) ;the same

(defvar lst '(11 22 33))
(car lst) ;11
(cdr lst) ;(22 33)
(setf (nth 2 lst) 44) ;(11 22 44)

(setf lst '(11))
(dotimes (i 5)
	(push (* i 3) lst))
(setf lst (nreverse lst))
(print lst) ;(11 0 3 6 9 12)
```

### Map
```lisp
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
```

### Set
```lisp
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
```

### Outer loop break
```lisp
(block outer
(dotimes (i 2)
    (dotimes (j 2)
        (when (= 3 (aref lul i j))
            (return-from outer
                (print j))))))
```

## Section 3

### Functions

#### Basics
```lisp
(defun mul (x y z)
    (* x y z))
(mul 2 3 4) ;24

;;sum of arrs elements
(defun sum-n (arr n)
    "Documentation"
    (let ((res 0))
        (dotimes (i n)
            (incf res (elt arr i))) ;or 'aref' instead of 'elt'
        res))

(documentation 'sum-n 'function);"Documentation"

```
#### Modifiers
```lisp
(defun add (x &optional (y 1337))
    (+ x y))
(add 1) ;1338

(defun func (a1 a2 &rest r)
    (list a1 a2 r))
(func 1 2 3 4 5) ;(1 2 (3 4 5))

(defun my-plus (&rest r)
    (let ((res 0))
        (dolist (arg r)
            (incf res arg))
        res))
(my-plus 1 2 3 4 5 6) ;21

(defun func-key (&key (k1 0) (k2 11))
    (list k1 k2))
(func-key :k2 1337) ;(0 1337)
```

#### Example: Quadratic equation
```lisp
(defun sqr1 (a b c)
    (let* ((DD (- (* b b) (* 4 a c)))
        (x1 (/ (+ (- b) (sqrt DD)) (* 2 a)))
        (x2 (/ (- (- b) (sqrt DD)) (* 2 a))))
			(list x1 x2)))
(sqr1 1 0 -4) ;(2 -2)
```

```lisp
(defun sqr2 (a b c &aux (DD (- (* b b) (* 4 a c))))
    (list (/ (+ (- b) (sqrt DD)) (* 2 a)) 
          (/ (- (- b) (sqrt DD)) (* 2 a))))
(sqr2 1 0 -4) ;(2 -2)
```

```lisp
(defun sqr3 (a b c)
    (labels ((discr (a b c)
                (- (* b b) (* 4 a c)))
            (root (a b c func)
                (/ 
                    (funcall func (- b) (sqrt (discr a b c)))
                    (* 2 a)
                )))
        (list 
            (root a b c #'+)
            (root a b c #'-)
        )))
(sqr3 1 0 -4) ;(2 -2)
```

```lisp
(destructuring-bind (x1 x2) (sqr 1 0 -4) 
    (format t "~%x1 = ~a, x2 = ~a" x1 x2)) ;"x1 = 2, x2 = -2"
```

### Multiple binding
```lisp
(defun count-odds (lst)
    (let ((count 0) (odds-lst nil))
        (dolist (i lst)
            (if (oddp i)
                (push i odds-lst)
                (incf count)))
        (values count (nreverse odds-lst))))

;(multiple-value-bind (x1 x2) (count-odds '(1 2 3 4)) (print x2)) ;(1 3)
;(multiple-value-bind (x1 x2) (count-odds '(1 2 3 4)) (print x1)) ;2
```

### Example: DFS
```lisp
(defvar tree '(1 (10 (15 nil nil) (7 nil nil)) (5 (2 nil nil) (8 nil nil))))

(defun val (tree)
    (car tree))
(defun left (tree)
    (car (cdr tree)))
(defun right (tree)
    (car (cdr (cdr tree))))

(defun magic (tree &optional (res 0))
    (print (val tree))
    (incf res (val tree))
    (if (left tree)
        (setf res (magic(left tree) res)))
    (if (right tree)
        (setf res (magic(right tree) res)))
    res)
(print (magic tree)) ;1 10 15 7 5 2 8 48
```

### Lambda
```lisp
(defparameter *fn* #'+)
(funcall *fn* 1 20 21) ;42
```

```lisp
(defun f (x) (* x x))

(defun func-coords (fn x0 xN h)
    (do ((x x0 (+ x h )) res)
        ((> x xN) (nreverse res))
        (push (list x (funcall fn x)) res)))

(func-coords #'f 0 4 1) ;#' == (function *) ;((0 0) (1 1) (2 4) (3 9) (4 16))
(func-coords #'(lambda (x) (* x x)) 0 4 1) ;((0 0) (1 1) (2 4) (3 9) (4 16))
```

```lisp
(setf f #'(lambda (x) (* x x)))
(funcall f 2) ;4
```

```lisp
(defun compose (f1 f2)
    (lambda (x)
        (funcall f2 (funcall f1 x))))
```

```=
;;;random element of a sequence
(defun random-elt (seq)
	(elt seq (random (length seq))))
```

## Section 4

### Mapping

##### map
```lisp
(defparameter *nums* '(1 2 3 4 5))

;;; we need a list of squares:
(let (acc)
    (dolist (i *nums*)
        (push (* i i) acc))
    (nreverse acc)
    (print acc)) ;(1 4 9 16 25)
```

```lisp
(defun simple-map (fn lst)
    (let (acc)
        (dolist (i lst)
            (push (funcall fn i) acc))
        (nreverse acc)))
        
(simple-map #'(lambda (x) (* x x)) *nums*) ;(1 4 9 16 25)
```

```lisp
;;default function
(map 'list #'abs '(-1 -2 13 -4)) ;(1 2 13 4)
```





##### map-into

```lisp
(defparameter *a* '(0 0 0))
(map-into *a* #'+ '(1 2 3) '(20 40 50)) ;(21 42 53)
```

##### mapcar/mapcan

```lisp
(mapcar #'(lambda (x y) (list x y)) '(1 2 3) '(4 5 6)) ;((1 4) (2 5) (3 6))
(mapcan #'(lambda (x y) (list x y)) '(1 2 3) '(4 5 6)) ;(1 4 2 5 3 6)
(mapcar #'(lambda (x) (if (evenp x) x nil)) '(1 2 3 4 5 6)) ;(NIL 2 NIL 4 NIL 6)
(mapcan #'(lambda (x) (if (evenp x) (list x) nil)) '(1 2 3 4 5 6)) ;(2 4 6)
```

```lisp
(defparameter *chars* #(#\a #\b #\c))
(map 'list #'(lambda (x y) (cons x y)) *chars* *nums*)
;;but:
(setf *charss* '(#\a #\b #\c))
(mapcar #'(lambda (x y) (cons x y)) *charss* *nums*))
;;both: ((#\a . 1) (#\b . 2) (#\c . 3))
```

### Filtering

```lisp
;;;destructing analog for 'remove' is 'delete'
(remove 4 '(1 2 3 4 5 6 7)' :test #'<) ;(1 2 3 4)
(remove 4 '(1 2 3 4 5 6 7)' :test #'< :count 2) ;(1 2 3 4 7)
(remove "lisp" '("kek" "lisp" "lol") :test-not #'string-equal) ;("list")
(remove-if #'(lambda (x) (not (oddp x))) '(1 2 3 4 5)) ;(1 3 5)
(remove-if-not #'oddp '(1 2 3 4 5)) ;(1 3 5)
(remove-if #'(lambda (x) (> x 4)) '(1 2 3 4 5 6 7)) ;(1 2 3 4)
(remove-if-not #'(lambda (x) (> x 4)) '(1 2 3 4 5 6 7)) ;(5 6 7)

(substitute 42 24 '(24 0 24)) ;(42 0 42)
(substitute-if 42 #'oddp '(1 2 3 4 5)) ;(42 2 42 4 42)
(substitute-if-not 42 #'oddp '(1 2 3 4 5)) ;(1 42 3 42 5)

;;;find -- the first element that satisfies a criteria 
;;;position -- -//- but index
;;;member -- presense
;;;assoc -- search for an element by key
;;;rassoc -- -//- by value
;;;count -- amount of elements
;;;search -- subsequence
```

### Reducing

```lisp
(reduce #'+ '(1 2 3 4 5)) ;15
(reduce #'- '(1 2 3 4 5) :from-end t) ;3

(defun map-on-reduce (fn lst) 
    (nreverse
        (reduce #'(lambda (acc x)
            (push (funcall fn x) acc))
            lst
            :initial-value nil)))
            
(map-on-reduce #'(lambda (x) (+ 1 x)) '(1 2 3 4)) ;(2 3 4 5)
```

### Example: Count primes in a list

```lisp
(defparameter *max-random-num* 100)
(defun random-list (n)
    (let (test-list)
        (dotimes (i n)
            (push 
                (+ 1 (random *max-random-num*))
                test-list))
        test-list))

(defun factr (n &optional (res 1)) ;for Wilson's test
    (if (or (= n 0) (= n 1))
        res
        (factr (- n 1) (* res n))))

(defun primep (x) ; Wilson's test
    (= (- x 1) (mod (factr (- x 1) 1) x)))

(defparameter lst (random-list 10))

(reduce #'+ 
    (mapcar #'(lambda (x) (if x 1 0))
        (mapcar #'primep lst))) ;counts primes
```

### Sort
```lisp
(defparameter lst '(20 4 6 8 0 3 1 7))
(sort lst #'>) ;(20 8 7 6 4 3 1 0)

(defparameter lst '(("177" 14) ("78" 30) ("98" 10)))
(sort lst #'(lambda (x y) (> (second x) (second y)))) ;(("78" 30) ("177" 14)  ("98" 10))
(sort lst #'> :key #'second) ;(("78" 30) ("177" 14)  ("98" 10))
```

## Section 5

### Macros

```lisp
;;nothing will be swapped because of scopes
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
;(LET ((TEMP A)) (SETF A B) (SETF B TEMP))

(list 'sum 'of a 'and b 'is (+ a b)) 
;(SUM OF 20 AND 10 IS 30)
`(sum of ,a and ,b is ,(+ a b)) ;the same

(defmacro swapf-new (x y) ;;any form can be sent to a macro
    `(let ((temp ,x))
        (setf ,x ,y) (setf ,y temp)))
```

##### mvb

```lisp
(truncate 3.5) ; 3;0.5
(multiple-value-bind (int frac) (truncate 3.5) (* int frac)) ;1.5

(defmacro mvb (vars-form func-form &body body)
    `(multiple-value-bind ,vars-form ,func-form ,@body))
    
(macroexpand-1 '(mvb (int frac) (truncate 3.5) (* int frac)))
;(MULTIPLE-VALUE-BIND (INT FRAC) (TRUNCATE 3.5) (* INT FRAC)
```

##### gensym

```lisp
;;some issues:
(defparameter temp 20)
(defparameter tempp 15)
;(swapf temp tempp) ;doesn't work because in macro we already have 'temp'

;;solution:
(defmacro swapf-new (x y) ;;any form can be sent to a macro -- not only 'x'
    (let ((temp (gensym)))
        `(let ((,temp ,x)) ; 'temp' with ',' !
            (setf ,x ,y) (setf ,y ,temp))))
```

##### A function that calls itself (lambda):
```lisp
(defmacro alambda (params &body body)
    `(labels ((self ,params ,@body))
        #'self))

(funcall (alambda (n) 
    (if (> n 1) 
        (* n (self (- n 1)))
        1)) 
    3) ;6 -- factorial

(macroexpand '(alambda (n) 
    (if (> n 1) 
        (* n (self (- n 1)))
        3)))
;(LABELS ((SELF (N) (IF (> N 1) (* N (SELF (- N 1 ))) 3))) #'SELF)
```

### Loop
```lisp
(dotimes (i 7) (print i))
(loop for i from 1 to 7 by 2 do (print i))
(loop repeat 3 do (print "pumba"))
(dolist (i '(1 2 3 4)) (print i))

(defparameter *arr* #(10 20 30 40))
(loop for x across *arr* do (print x))

(loop for x in '(1 2 3 4) by #'cddr do (print x))

(loop for x = 33 then (random 10) do 
    (if (zerop x) (return) (print x)))

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

(loop repeat 5 for x = (random 100)
    when (oddp x) do (print x)) ;or unless 

(loop for x in '(1 3 5) always (oddp x)) ;t
;never ;thereis
```

##### with-gensyms-
```lisp
(defmacro with-gensyms- (names &body body)
    `(let ,(loop for n in names collect `(,n (gensym)))
        ,@body))

(defmacro with-gensyms-init (names init-vals &body body)
    `(with-gensyms- (,@names)
    `(let (,,@(loop for v in init-vals for n in names 
        collect ``(,,n  ,,v)))
    ,,@body)))
```

### Example: Do something with primes from a to b

```lisp
(defun factr (n &optional (res 1)) ;for Wilson's test
    (if (or (= n 0) (= n 1))
        res
        (factr (- n 1) (* res n))))

(defun primep (x) ; Wilson's test
    (= (- x 1) (mod (factr (- x 1) 1) x)))
```

```lisp
(defmacro do-sym-nums ((var startv endv) &body body)
    `(loop for x from ,startv to ,endv do 
        (if (primep x)
            ,@body)))
            
(do-primes (x 3 11) (print x)) ;3 5 7 11
```

## Section 6

### input/output

```lisp
(print "Hello" *error-output*)
(progn (prin1 "1") (prin1 "2")) ;"1""2"
(progn (prin1 "1") (terpri) (prin1 "2")) ;1 \n 2
```


```lisp
(defparameter *str-out* (make-string-output-stream))
(progn (princ 42 *str-out*)
    (princ 42 *str-out*)
    (princ 42 *str-out*)
    (princ 42 *str-out*)
    (princ 42 *str-out*))
(get-output-stream-string *str-out*) ;4242424242
```

```lisp
(defparameter *str-in* (make-string-input-stream "42 3.14"))
(read *str-in*) ;42
(read *str-in*) ;3.14
```

```lisp
(with-input-from-string (s "111 42 3.14" :start 3)
    (+ (read s) (read s))) ;45.14
```

### Format

```lisp
(format t "~a" "test") ;test
(format t "~s" "test") ;"test"
(format t "~a~%~%~a~&~a" "first" "second" "third") ;~% -- \n, ~& -- (fresh-line)

(format t "~&~d ~:*~o ~:*~x ~:*~b" 42) ;~:* -- takes previous argument

(format t "~10d" 42) ;        42
(format t "~10,'_d" 42) ;_______42

```

```lisp
(multiple-value-list (get-decoded-time)) ;(0 24 23 7 5 2016 0 NIL -3)
(apply #'format t "~&~*~*~*~2,'0d.~2,'0d.~d
    (multiple-value-list (get-decoded-time))) ;~* -- pass one argument
```

```lisp
(format nil "~3r" 42) ;"1120" ternary system
(format nil "~@r" 42) ;"XLII"
(format nil "~r" 42) ;"forty-two"
(format nil "~:r" 42) ;"forty-second"
(format t "~:r" 1337) ;one thousand, three hundrer thirty-seventh

(format t "~&~,2f" pi) ;3.14
(format t "~&~,2e" pi) ;3.14L+0

(format t "~&~[1~;2~;3~;4~]" (random 4))
(format t "~&~d is ~:[NOT~;indeed~] an odd" 3 (oddp 3))

```
```lisp
(defparameter *pac* `(1 2 3))
(format t "~&~{~dpac~%~}none~&" *pac*)
; 1pac 2pac 3pac none
```

### File input/output

```lisp
(with-open-file (out "~/***/test.txt"
            :direction :output
            :if-exists :append) ;supersede -- rewrite
    (loop for x = (random 100)
        repeat 20 do
            (format out "~,3d " x)))

(with-open-file (in "~/***/test.txt")
    (loop for x = (read in nil) while x do
        (format t "~d " x)))
```

### Reader macros

```lisp
(set-macro-character #\$
    #'(lambda (stream c)
        (list 'quote (read stream nil t))))
(eq 'a $a) ;t
(eq (quote a) $a) ;t

(get-macro-character #\') ;#<FUNCTION (LAMBDA (STREAM C) {***}>)
```

```lisp
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
```

### Example: #[a b] for primes from a to b

```lisp
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

(print #[10 20]) ;(11 13 17 19)
```
***
[Source course [RU]](https://courses.openedu.ru/courses/course-v1:ITMOUniversity+FPBC+spring_2018/info)