;;;; vector.lisp
;;;; Please see the licence.txt for the CLinch 

(in-package #:clinch)

(defun make-vector (a b c)
  (v! (coerce a 'single-float)
      (coerce b 'single-float)
      (coerce c 'single-float)))

(defun make-identity-matrix3 ()
  (make-array 9 :element-type 'single-float :initial-contents '(1.0 0.0 0.0
								0.0 1.0 0.0
								0.0 0.0 1.0)))

(defun make-matrix3 (&rest vals)
  (if (> (length vals) 9)
      (error "make-matrix takes up to 9 values!")
      
      (let ((ret (make-array 9 :element-type 'single-float :initial-element 0.0)))
	(dotimes (i (length vals))
	  (setf (aref ret i) (nth i vals)))
	ret)))

(defun convert-matrix4-to-matrix3 (m4)
  (let ((ret (make-array 9 :element-type 'single-float :initial-element 0.0)))
    (dotimes (i 9)
      (setf (aref ret i) (aref m4 i)))
    ret))

(defun ray-triangle-intersect? (origin ray-dir v0 v1 v2)
  (let ((epsilon 1.0e-6))
    (let ((edge1 (v3:- v1 v0))
	  (edge2 (v3:- v2 v0)))
      (let* ((pvec (v3:cross ray-dir edge2))
	     (det (v3:dot edge1 pvec)))
	(when (not (and (> det (- epsilon)) (< det epsilon)))
	  (let* ((inverse-det (/ 1.0 det))
		 (tvec (v3:- origin v0))
		 (u (* (v3:dot tvec pvec) inverse-det)))
	    (when (not (or (< u 0.0) (> u 1.0)))
	      (let* ((qvec (v3:cross tvec edge1))
		     (v (* (v3:dot ray-dir qvec) inverse-det)))
		(when (not (or (< v 0.0) (> (+ u v) 1.0)))
		  (let ((hit-distance (* (v3:dot edge2 qvec)
					 inverse-det)))
		    ;; values ?
		    (when (>= hit-distance 0.0)
		      (values hit-distance u v))))))))))))
