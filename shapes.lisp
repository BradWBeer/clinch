;;;; shapes.lisp
;;;; Please see the licence.txt for the CLinch 
;; Not used. (yet?)

(in-package #:clinch)

;; returns indexes, vertexes, normals, and texcoords in values
(defun make-sphere (radius rings sectors)

  (let ((vertices  (make-array (* rings sectors 3)))
	(normals   (make-array (* rings sectors 3)))
	(texcoords (make-array (* rings sectors 2)))
	(indices   (make-array (* rings sectors 6)))
	(slice-r (/ (1- rings)))
	(slice-s (/ (1- sectors))))
    
    (loop for r from 0 to (1- rings)
       do (loop for s from 0 to (1- sectors)

	     for y = (sin (+ (/ pi -2)
			     (* pi r slice-r)))
	       
	     for x = (* (cos (* pi 2 s slice-s))
			(sin (* pi r slice-r)))
	       
	     for z =  (* (sin (* 2 pi s slice-s))
			 (sin (* pi r slice-r)))
	       
	     do (let* ((triangle (+ (* r sectors) s))
		       (3t (* 3 triangle))
		       (2t (* 2 triangle))
		       (6t (* 6 triangle)))

		  (when (and (< (abs x) .01)
			     (< (abs y) .01)
			     (< (abs z) .01))

		    (format t "~A ~A was all zeros (~A ~A ~A)~%" r s x y z))

		  (setf (aref vertices (+ 3t 0)) (* radius x)
			(aref vertices (+ 3t 1)) (* radius y)
			(aref vertices (+ 3t 2)) (* radius z)

			(aref normals (+ 3t 0)) x
			(aref normals (+ 3t 1)) y
			(aref normals (+ 3t 2)) z
			
			(aref texcoords (+ 2t 0)) (* s slice-s)
			(aref texcoords (+ 2t 1)) (* r slice-r)
			
			(aref indices (+ 6t 0)) (+ (* r sectors) s)
			(aref indices (+ 6t 1)) (+ (* r sectors) s 1)
			(aref indices (+ 6t 2)) (+ (* (1+ r) sectors) s 1)

			(aref indices (+ 6t 3)) (+ (* r sectors) s)
			(aref indices (+ 6t 4)) (+ (* (1+ r) sectors) s 1)
			(aref indices (+ 6t 5)) (+ (* (1+ r) sectors) s)))))
    

    (values (coerce indices 'list)
	    (coerce vertices 'list)
	    (coerce normals 'list)
	    (coerce texcoords 'list))))
