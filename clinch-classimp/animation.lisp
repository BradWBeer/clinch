;;;; animation.lisp
;;;; Please see the licence.txt for the CLinch 

(in-package :clinch)

(defmethod translate-weights ((bone classimp:bone))
  (map 'list (lambda (x) (cons (classimp:id x) (classimp:weight x))) (classimp:weights bone)))

(defmethod add-weights-to-hash-table ((bone classimp:bone) (hash hash-table))
  (let ((bone bone))
    (loop for (id . weight) in (translate-weights bone)
          do (progn

	       (setf (gethash id hash)
                     (cons (cons weight bone) (gethash id hash)))))))

(defmethod get-mesh-weights ((mesh classimp:mesh) (hash hash-table))
  (map nil (lambda (x) (add-weights-to-hash-table x hash))
       (classimp:bones mesh))
  (loop for i from 0 below (length (classimp:faces mesh))
     collect (cons i (sort (gethash i hash)
			   (lambda (x y)
			     (> (car x) (car y)))))))

(labels ((between (start end value)
	   (and (>= value start)
		(<= value end)))
	 
	 (interpolate (start end value)
	   (let ((val (float 
		       (/ (- value start)
			  (- end start)))))
	     (values (- 1 val)
		     val)))

	 (find-between (alst time &optional repeat)
	   (assoc (if repeat 
		      (mod time repeat)
		      time)
		  alst :test (lambda (a x)
			       (let ((start (car x))
				     (end (cdr x)))
				 (if end
				     (between start end a)
				     (>= a start)))))))

  (defun find-animation-mix (alst time &optional repeat)
    (let* ((between (find-between alst time repeat))
	   (start (caar between))
	   (end (cdar between)))
      (if end
	  (multiple-value-bind (lo hi) (interpolate start end time)
	    (values lo hi (cdr between)))
	  (values 1 0 (cdr between))))))


(defmethod translate-vector-key ((this classimp::vector-key))
  (list (slot-value this 'classimp::time)
	(slot-value this 'classimp:value)))

(defun create-animation-list (lst)
  (when lst
    (cons (cons  
	   (cons (caar lst) (caadr lst))
	   (cons (cadar lst) (cadadr lst)))
	  (create-animation-list (cdr lst)))))

;; turns a list of vector keys into a useful list.
(defun vector-keys->list (keys)
  (map 'list #'translate-vector-key keys))

(defun slerp-animation (alst time &optional repeat)
  (multiple-value-bind (s1 s2 vecs) (find-animation-mix alst time repeat)
    (if (cdr vecs)
	(q:slerp (car vecs) (cdr vecs) s2)
	(car vecs))))

