(defun nth-digit(num n)
    (rem (truncate (/ num (expt 10 n))) 10))

(defun first-three (n)
    (+ (nth-digit n 3) (nth-digit n 4) (nth-digit n 5)))

(defun last-three (n)
    (+ (nth-digit n 0) (nth-digit n 1) (nth-digit n 2)))

(defun lucky (n)
    (if (= (first-three n) (last-three n)) 
    	t))

(print (lucky 752771))