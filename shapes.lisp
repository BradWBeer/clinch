;;;; shapes.lisp
;;;; Please see the licence.txt for the CLinch 
;; 
(in-package #:clinch)

(defun make-quad-indices ()
  (make-instance 'clinch:index-buffer :data '(0 1 2 0 2 3)))

(defun make-quad-vertexes (width height &key (center :center))

  (let* ((x/2 (float (/ width 2)))
	 (y/2 (float (/ height 2)))
	 (x)
	 (y) 
	 (-x)
	 (-y))
    
    (case center
      (:center (setf x  x/2
		     -x (- x/2)
		     y  y/2
		     -y (- y/2)))
      (:top-center (setf x x/2
			 -x (- x/2)
			 y 0.0 
			 -y (- height)))
      (:bottom-center (setf x  x/2
			    -x (- x/2)
			    y  height
			    -y 0.0))
      (:center-left (setf x  width
			  -x 0.0
			  y  y/2
			  -y (- y/2)))
      (:center-right (setf x 0.0
			   -x (- width)
			   y  y/2
			   -y (- y/2)))
      (:top-left (setf x width
		       -x 0.0
		       y 0.0
		       -y (- height)))
      (:bottom-left (setf x width
			  -x 0.0
			  y height
			  -y 0.0))
      (:top-right (setf x 0.0
			-x (- width)
			y 0.0
			-y (- height)))
      
      (:bottom-right (setf x 0.0
			   -x (- width)
			   y height
			   -y 0.0)))
    

    (make-instance 'clinch:buffer 
		   :Stride 3
		   :data (map 'list (lambda (x)
				      (coerce x 'single-float))
			      (list  -x  y 0
				     -x  -y 0
				     x  -y 0
				     x   y 0)))))



(defun make-quad-texture-coordinates ()
  (make-instance 'clinch:buffer 
		 :Stride 2
		 :data (map 'list (lambda (x)
				    (coerce x 'single-float))
			    '(0.0   1.0
			      0.0   0.0
			      1.0   0.0
			      1.0   1.0))))

(defun make-quad (width height &key (center :center) (shader-program nil) texture)

  (make-instance 'clinch:entity
		 :shader-program (or shader-program (get-generic-single-texture-shader))
		 :indexes (make-quad-indices)
		 :attributes `(("v" . ,(make-quad-vertexes width height :center center))
			       ("tc1" . ,(make-quad-texture-coordinates)))
		 :uniforms `(("M" . :model)
			     ("P" . :projection)
			     ("t1" . ,(or texture (get-identity-texture))))))


;; ;; returns indexes, vertexes, normals, and texcoords in values
;; (defun make-sphere (radius rings sectors)

;;   (let ((vertices  (make-array (* rings sectors 3)))
;; 	(normals   (make-array (* rings sectors 3)))
;; 	(texcoords (make-array (* rings sectors 2)))
;; 	(indices   (make-array (* rings sectors 6)))
;; 	(slice-r (/ (1- rings)))
;; 	(slice-s (/ (1- sectors))))

;;     (loop for r from 0 to (1- rings)
;;        do (loop for s from 0 to (1- sectors)

;; 	     for y = (sin (+ (/ pi -2)
;; 			     (* pi r slice-r)))

;; 	     for x = (* (cos (* pi 2 s slice-s))
;; 			(sin (* pi r slice-r)))

;; 	     for z =  (* (sin (* 2 pi s slice-s))
;; 			 (sin (* pi r slice-r)))

;; 	     do (let* ((triangle (+ (* r sectors) s))
;; 		       (3t (* 3 triangle))
;; 		       (2t (* 2 triangle))
;; 		       (6t (* 6 triangle)))

;; 		  (when (and (< (abs x) .01)
;; 			     (< (abs y) .01)
;; 			     (< (abs z) .01))

;; 		    (format t "~A ~A was all zeros (~A ~A ~A)~%" r s x y z))

;; 		  (setf (aref vertices (+ 3t 0)) (* radius x)
;; 			(aref vertices (+ 3t 1)) (* radius y)
;; 			(aref vertices (+ 3t 2)) (* radius z)

;; 			(aref normals (+ 3t 0)) x
;; 			(aref normals (+ 3t 1)) y
;; 			(aref normals (+ 3t 2)) z

;; 			(aref texcoords (+ 2t 0)) (* s slice-s)
;; 			(aref texcoords (+ 2t 1)) (* r slice-r)

;; 			(aref indices (+ 6t 0)) (+ (* r sectors) s)
;; 			(aref indices (+ 6t 1)) (+ (* r sectors) s 1)
;; 			(aref indices (+ 6t 2)) (+ (* (1+ r) sectors) s 1)

;; 			(aref indices (+ 6t 3)) (+ (* r sectors) s)
;; 			(aref indices (+ 6t 4)) (+ (* (1+ r) sectors) s 1)
;; 			(aref indices (+ 6t 5)) (+ (* (1+ r) sectors) s)))))


;;     (values (coerce indices 'list)
;; 	    (coerce vertices 'list)
;; 	    (coerce normals 'list)
;; 	    (coerce texcoords 'list))))
