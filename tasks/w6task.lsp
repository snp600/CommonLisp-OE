#|
(defparameter *cdrs* 
	'("1101;+79119989911;122" 
		"+49231114563;1102;347" 
		"1101;+420023334521;134" 
		"1102;+49023334521;811" 
		"ERR01:1234;;;0;0" 
		"1101;+390145211212;93" 
		"1101;+49023334521;756"))
|#


(defparameter *cdrs* 
	'("1101;+79119989911;122" 
		" +47231114563;1102;347" 
		"+47023334521;1101;134" 
		"1102;+49023334521;811" 
		"1102;1101;42" 
		"ERR01:1234;;;0;0" 
		" +390145211212; 1102; 93" 
		"+47023414522;1102;753" 
		"1102;+79119989911;771"))

;(defparameter *cdrs* (read))

(defun get-time (record)
	(let ((sub (subseq record (+ 1 (position #\; record)))))
		(subseq sub (+ 1 (position #\; sub)))))

(defun get-outcall-number (record)
	(subseq record 0 (position #\; record)))


(defun needed-p (record)
	(let ((code 
		(let ((offset (position #\; record)))
		(subseq record (+ 1 offset) (+ 4 offset)))))
		(or (equal "+39" code)
			(equal "+49" code))))	

(defun get-needed-cdrs (cdrs)
	(remove-if-not #'needed-p cdrs))

(setf *cdrs* (get-needed-cdrs *cdrs*))

(defun get-outcalls-rating (cdrs)
	(let ((rating (make-hash-table :test #'equal)))
		(dolist (record cdrs)
			(defparameter out (get-outcall-number record))
			(let (
					(out (get-outcall-number record)) 
					(ctime (parse-integer (get-time record)))
					(val (gethash out rating)))

				(if val
					(setf (gethash out rating) (+ ctime val))
					(setf (gethash out rating) ctime))))
		(ht-to-list rating)))

(defun ht-to-list (h) 
	(let (pairs)
		(maphash #'(lambda (k v) 
			(push (list k v) pairs)) h)
		(nreverse pairs)))

(defun get-the-worst-waster-number (cdrs)
	(parse-integer (first (car (sort (get-outcalls-rating cdrs) #'> :key #'second)))))

(defun get-the-worst-waster-time (cdrs)
	(second (car (sort (get-outcalls-rating cdrs) #'> :key #'second))))
