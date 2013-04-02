;;;; vector.lisp
;;;; Please see the licence.txt for the CLinch 

(in-package #:clinch)


(defun make-vector (a b c)
  (sb-cga:vec (float a)
	      (float b)
	      (float c)))

(defun ray-triangle-intersect? (origin ray-dir v0 v1 v2)
  (declare (ignore v2))
  (let ((epsilon 1.0e-6))
    (let ((edge1 (sb-cga:vec- v1 v0))
	  (edge2 (sb-cga:vec- v2 v0)))
      (let* ((pvec (sb-cga:cross-product ray-dir edge2))
	     (det (sb-cga:dot-product edge1 pvec)))
	(when (not (and (> det (- epsilon)) (< det epsilon)))
	  (let* ((inverse-det (/ 1.0 det))
		 (tvec (sb-cga:vec- origin v0))
		 (u (* (sb-cga:dot-product tvec pvec) inverse-det)))
	    (when (not (or (< u 0.0) (> u 1.0)))
	      (let* ((qvec (sb-cga:cross-product tvec edge1))
		     (v (* (sb-cga:dot-product ray-dir qvec) inverse-det)))
		(when (not (or (< v 0.0) (> (+ u v) 1.0)))
		  (let ((hit-distance (* (sb-cga:dot-product edge2 qvec)
					 inverse-det)))
		    ;; values ?
		    (when (>= hit-distance 0.0)
		      (values hit-distance u v))))))))))))