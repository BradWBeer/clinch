;;;; shapes.lisp
;;;; Please see the licence.txt for the CLinch 
;; 
(in-package #:clinch)

(defun make-quad-indices ()
  (make-instance 'clinch:buffer 
		 :Stride 3
		 :data (map 'list (lambda (x)
				    (coerce x 'single-float))
			    (list -w/2  h/2 0.0
				  -w/2 -h/2 0.0
				  w/2  -h/2 0.0
				  w/2   h/2 0.0))))

(defun make-quad-vertexes (width height &key (center :center))

  )

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

  (let* ((x/2 (float (/ width 2)))
	 (y/2 (float (/ height 2))))
    
    (case center
      (:center (setf w/2 x/2
		     -w/2 (- x/2)
		     h/2 y/2
		     -h/2 (- y/2))
	       (:top-left (setf 0 0
				/2 0
				h/2 h
				-h/2 0))
	       (:top-right nil)
	       (:top-center nil)
	       (:bottom-left nil)
	       (:bottom-right nil)
	       (:bottom-center nil)
	       (:center-left nil)
	       (:center-right nil)))

    (make-instance 'clinch:entity
		   :indexes (make-instance 'clinch:index-buffer :data (make-quad-indices))
		   :attributes `(("v" . ,(make-quad-vertexes))
				 ("tc1" . ,(make-quad-texture-coordinates)))
		   :uniforms `(("M" . :model)
			       ("P" . :projection)
			       ("t1" . ,(or texture (get-identity-texture)))))))
  
  
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
