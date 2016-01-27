;;;; transform.lisp
;;;; Please see the licence.txt for the CLinch 

(in-package #:clinch)

(defconstant +pi+ (coerce pi 'single-float)
  "A single-float version of pi.")

(defmacro ensure-float (x)
  "Coerce a number to a single-float."
  `(coerce ,x 'single-float))


(defmacro degrees->radians (degrees)
  "Converts degrees to radians."
  `(coerce (* 2 pi (/ ,degrees 360)) 'single-float))

(defmacro d->r (degrees)
  "Converts degrees to radians."
  `(degrees->radians ,degrees))


(defmacro radians->degrees (radians)
  "Converts radians to degrees."
  `(coerce (* 180 (/ ,radians pi)) 'single-float))

(defmacro r->d (radians)
  "Converts radians to degrees."
  `(radians->degrees ,radians))

;;; Do I still need this? !!!
(defun make-orthogonal-transform (width height near far)
  "Create a raw CFFI orthogonal matrix."
  (make-matrix (/ 2 width) 0.0 0.0 0.0
	       0.0 (/ 2 height) 0.0 0.0
	       0.0 0.0 (/ (- far near)) (/ (- near) (- far near)) 
	       0.0 0.0 0.0 1.0))

;;; Do I still need this? !!!
(defun make-frustum-transform (left right bottom top near far)
  "Create a raw CFFI frustum matrix."  
  (let ((a (/ (+ right left) (- right left)))
	(b (/ (+ top bottom) (- top bottom)))
	(c (- (/ (+ far near) (- far near))))
	(d (- (/ (* 2 far near) (- far near)))))
    
    (m4:! (/ (* 2 near) (- right left)) 0.0 A 0.0
		 0.0 (/ (* 2 near) (- top bottom)) B 0.0
		 0.0 0.0 C D
		 0.0 0.0 -1.0 0.0)))

;;; Do I still need this? !!!
(defun make-perspective-transform  (fovy aspect znear zfar)
  "Create a raw CFFI perspective matrix."
  (let* ((ymax (* znear (tan fovy)))
	 (xmax (* ymax aspect)))
    (make-frustum-transform (- xmax) xmax (- ymax) ymax znear zfar)))

;;; Do I still need this? !!!
(defun transform-point (p m)
  (let ((w (/
	    (+ (* (elt m 3) (elt p 0))
	       (* (elt m 7) (elt p 1))
	       (* (elt m 11) (elt p 2))
	       (elt m 15)))))
    (make-vector (* w (+ (* (elt m 0) (elt p 0))
			 (* (elt m 4) (elt p 1))
			 (* (elt m 8) (elt p 2))
			 (elt m 12)))
		 (* w (+ (* (elt m 1) (elt p 0))
			 (* (elt m 5) (elt p 1))
			 (* (elt m 9) (elt p 2))
			 (elt m 13)))
		 (* w (+ (* (elt m 2) (elt p 0))
			 (* (elt m 6) (elt p 1))
			 (* (elt m 10) (elt p 2))
			 (elt m 14))))))

						     
;;; Do I still need this? !!!
(defun unproject (x y width height inv-transform)
  (let* ((new-x (1- (/ (* 2 x) width)))
	 (new-y (- (1- (/ (* 2 y) height))))
	 (start (clinch:transform-point (v! new-x new-y 0) inv-transform))
	 (end   (clinch:transform-point (v! new-x new-y 1) inv-transform)))

    (values start
	    (v3:normalize (v3:- end start)))))
  
;;; Do I still need this? !!!
(defun get-screen-direction (lens-1)
  (let ((start-of-box (clinch:transform-point (v! 0 0 0)
					lens-1))
	(end-of-box   (clinch:transform-point (v! 0 0 1)
						 lens-1)))
    (values start-of-box
	   (v3:normalize (v3:- end-of-box start-of-box)))))

