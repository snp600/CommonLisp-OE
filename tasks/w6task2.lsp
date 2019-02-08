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
#|
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
		|#
(defparameter *cdrs* (read))
(defparameter init (copy-list *cdrs*))


(defun get-time (record)
	(let ((sub (subseq record (+ 1 (position #\; record)))))
		(subseq sub (+ 1 (position #\; sub)))))

(defun get-incall-number (record)
	(let ((sub (subseq record (+ 1 (position #\; record)))))
		(subseq sub 0 (position #\; sub))))

(defun get-outcall-number (record)
	(subseq record 0 (position #\; record)))

(defun needed-p (record)
	(equal "+47" (subseq record 0 3)))

(defun get-needed-cdrs (cdrs)
	(remove-if-not #'needed-p cdrs))

(defun get-outcalls-rating (cdrs)
	(let ((rating (make-hash-table :test #'equal)))
		(dolist (record cdrs)
			(defparameter in (get-incall-number record))
			(let (
					(in (get-incall-number record)) 
					(val (gethash in rating)))

				(if val
					(setf (gethash in rating) (+ 1 val))
					(setf (gethash in rating) 1))))
		(ht-to-list rating)))

(defun ht-to-list (h) 
	(let (pairs)
		(maphash #'(lambda (k v) 
			(push (list k v) pairs)) h)
		(nreverse pairs)))

(defun get-needed-puchka (cdrs)
	(first (car (sort (get-outcalls-rating cdrs) #'> :key #'second))))

(defun total-time (puchka)
	(let ((acc 0))
		(dolist (record init)
			(if (equal puchka (get-outcall-number record))
				(incf acc (parse-integer (get-time record))))
		) 
		acc))



(setf *cdrs* (mapcar #'(lambda (x) (string-trim " " x)) *cdrs*))
(setf *cdrs* (get-needed-cdrs *cdrs*))

(print (total-time (get-needed-puchka *cdrs*)))
