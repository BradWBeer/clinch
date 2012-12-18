;;;; transform.lisp
;;;; Please see the licence.txt for the CLinch 

(in-package #:clinch)

(defmacro degrees->radians (degrees)
  (coerce (* 2 pi (/ degrees 360)) 'single-float))

(defclass transform ()
  ((type
    :initform :float
    :initarg :qtype
    :reader  qtype)
   (transform
    :initform nil
    :initarg  :transform
    :reader transform))
  (:documentation "Wrapper for 4x4 cffi matrix."))


(defmethod (setf transform) ((other-transform transform) (this transform))
  "Copies from one transform to another. This is an IN-PLACE operation!"
  (copy-foreign-array-data (transform other-transform)
			   (transform this)
			   16
			   (qtype other-transform)
			   (qtype this)))



(defun transform->list (m)
  "Convenience function for reading raw cffi 4x4 matrix."
  (list (cffi:mem-aref m :float 0)
	(cffi:mem-aref m :float 4)
	(cffi:mem-aref m :float 8)
	(cffi:mem-aref m :float 12)
	(cffi:mem-aref m :float 1)
	(cffi:mem-aref m :float 5)
	(cffi:mem-aref m :float 9)
	(cffi:mem-aref m :float 13)
	(cffi:mem-aref m :float 2)
	(cffi:mem-aref m :float 6)
	(cffi:mem-aref m :float 10)
	(cffi:mem-aref m :float 14)
	(cffi:mem-aref m :float 3)
	(cffi:mem-aref m :float 7)
	(cffi:mem-aref m :float 11)
	(cffi:mem-aref m :float 15)))
  
(defun make-foreign-array (length type &optional contents)
  "Convenience function for creating raw cffi 4x4 matrix."
  (cffi:foreign-alloc type :count length :initial-contents contents))

(defun copy-foreign-array-data (src dest len &optional (type-src :float) (type-dest :float))
  "Convenience function for copying values from one raw cffi 4x4 matrix to another."
  (loop for i from 0 to (1- len)
     do (setf (cffi:mem-aref dest type-dest i)
	      (cffi:mem-aref src  type-src  i))))


(defun make-identity-transform (&optional (type :float))
  "Convenience function to create raw cffi 4x4 identity matrix."
  (make-foreign-array 16 type '(1.0 0.0 0.0 0.0
				0.0 1.0 0.0 0.0
				0.0 0.0 1.0 0.0
				0.0 0.0 0.0 1.0)))


(defmethod set-identity-transform ((this transform) &key)
  "Sets the matrix back to the identity matrix."
  (with-accessors ((m transform)
		   (type qtype)) this
    (loop
       for val in '(1.0 0.0 0.0 0.0
		    0.0 1.0 0.0 0.0
		    0.0 0.0 1.0 0.0
		    0.0 0.0 0.0 1.0)
       for i from 0 to 15
       do (setf (cffi:mem-aref m type i) val))))

(defmethod qreset ((this transform) &key)
  "Sets the matrix back to the identity matrix."
  (set-identity-transform this))

(defmethod print-object ((this transform) s)
  "The custom printing function for transforms."
  (format s "#<TRANSFORM ~S ~%{~{~,4f~t~,4f~t~,4f~t~,4f~^~% ~}}>" (qtype this) (transform->list (transform this))))


(defmethod initialize-instance :after ((this transform) &key)
  "After creating the Transform instance, puts a raw matrix in (slot transform) and sets up garbage collection."
  (with-slots ((m transform)) this
    (unless m
      (setf m (make-identity-transform (qtype this))))
    (trivial-garbage:finalize this (lambda () (cffi:foreign-free m)))))

(eval
 `(defun m*m (a b dest)
    "Raw cffi matrix multiplication."
    (declare (optimize (speed 3)))
    (cffi:with-foreign-object  (c :float 16)
      ,@(loop for i from 0 to 3
	   append (loop for j from 0 to 3
		     collect `(setf (cffi:mem-aref c :float ,(+ (* j 4) i))
				    (+ ,@(loop for k from 0 to 3
					    collect `(* (cffi:mem-aref a :float ,(+ (* k 4) i)) 
							(cffi:mem-aref b :float ,(+ (* j 4) k))))))))
      (copy-foreign-array-data c dest 16)
      dest)))


(defmethod m* ((this transform) (that transform) &optional in-place)
  "Transform Multiplication. If in-place is true, the first matrix will be modified. It still makes a temporary raw matrix until it is copied."
  (if in-place
      (progn
	(m*m (transform that)
	     (transform this)
	     (transform this))
	this)
      (let ((ret (make-instance 'transform :qtype (qtype this))))
	(m*m (transform that)
	     (transform this)
	     (transform ret))
	ret)))

(eval 
 `(defun mT (a dest)
    "Raw cffi matrix transpose."
    (declare (optimize (speed 3)))
    (cffi:with-foreign-object (c :float 16)
      ,@(loop for i from 0 to 3
	   append (loop for j from 0 to 3
		     collect `(setf (cffi:mem-aref c :float ,(+ (* 4 j) i))
				    (cffi:mem-aref a :float ,(+ (* 4 i) j)))))
      (copy-foreign-array-data c dest 16)      
      dest)))

(defmethod transpose ((this transform) &optional in-place)
  "Transpose Transform. If in-place is true, the matrix will be modified. It still makes a temporary raw matrix until it is copied."
  (if in-place
      (progn
	(mT (transform this) (transform this))
	this)
      (let ((ret (make-instance 'transform :qtype (qtype this))))
	(mT (transform this) (transform ret))
	ret)))


(defun det (m &key (type :float))
  "Raw cffi matrix determinate."
  (-
   (- (* (CFFI:MEM-AREF M TYPE 0) (CFFI:MEM-AREF M TYPE 5)
	 (CFFI:MEM-AREF M TYPE 10) (CFFI:MEM-AREF M TYPE 15))
      (* (CFFI:MEM-AREF M TYPE 0) (CFFI:MEM-AREF M TYPE 6)
	 (CFFI:MEM-AREF M TYPE 11) (CFFI:MEM-AREF M TYPE 13))
      (* (CFFI:MEM-AREF M TYPE 0) (CFFI:MEM-AREF M TYPE 7)
	 (CFFI:MEM-AREF M TYPE 9) (CFFI:MEM-AREF M TYPE 14))
      (* (CFFI:MEM-AREF M TYPE 1) (CFFI:MEM-AREF M TYPE 4)
	 (CFFI:MEM-AREF M TYPE 11) (CFFI:MEM-AREF M TYPE 14))
      (* (CFFI:MEM-AREF M TYPE 1) (CFFI:MEM-AREF M TYPE 6)
	 (CFFI:MEM-AREF M TYPE 8) (CFFI:MEM-AREF M TYPE 15))
      (* (CFFI:MEM-AREF M TYPE 1) (CFFI:MEM-AREF M TYPE 7)
	 (CFFI:MEM-AREF M TYPE 10) (CFFI:MEM-AREF M TYPE 12))
      (* (CFFI:MEM-AREF M TYPE 2) (CFFI:MEM-AREF M TYPE 4)
	 (CFFI:MEM-AREF M TYPE 9) (CFFI:MEM-AREF M TYPE 15))
      (* (CFFI:MEM-AREF M TYPE 2) (CFFI:MEM-AREF M TYPE 5)
	 (CFFI:MEM-AREF M TYPE 11) (CFFI:MEM-AREF M TYPE 12))
      (* (CFFI:MEM-AREF M TYPE 2) (CFFI:MEM-AREF M TYPE 7)
	 (CFFI:MEM-AREF M TYPE 8) (CFFI:MEM-AREF M TYPE 13))
      (* (CFFI:MEM-AREF M TYPE 3) (CFFI:MEM-AREF M TYPE 4)
	 (CFFI:MEM-AREF M TYPE 10) (CFFI:MEM-AREF M TYPE 13))
      (* (CFFI:MEM-AREF M TYPE 3) (CFFI:MEM-AREF M TYPE 5)
	 (CFFI:MEM-AREF M TYPE 8) (CFFI:MEM-AREF M TYPE 14))
      (* (CFFI:MEM-AREF M TYPE 3) (CFFI:MEM-AREF M TYPE 6)
	 (CFFI:MEM-AREF M TYPE 9) (CFFI:MEM-AREF M TYPE 12)))

   (+ (* (CFFI:MEM-AREF M TYPE 0) (CFFI:MEM-AREF M TYPE 5)
	 (CFFI:MEM-AREF M TYPE 11) (CFFI:MEM-AREF M TYPE 14))
      (* (CFFI:MEM-AREF M TYPE 0) (CFFI:MEM-AREF M TYPE 6)
	 (CFFI:MEM-AREF M TYPE 9) (CFFI:MEM-AREF M TYPE 15))
      (* (CFFI:MEM-AREF M TYPE 0) (CFFI:MEM-AREF M TYPE 7)
	 (CFFI:MEM-AREF M TYPE 10) (CFFI:MEM-AREF M TYPE 13))
      (* (CFFI:MEM-AREF M TYPE 1) (CFFI:MEM-AREF M TYPE 4)
	 (CFFI:MEM-AREF M TYPE 10) (CFFI:MEM-AREF M TYPE 15))
      (* (CFFI:MEM-AREF M TYPE 1) (CFFI:MEM-AREF M TYPE 6)
	 (CFFI:MEM-AREF M TYPE 11) (CFFI:MEM-AREF M TYPE 12))
      (* (CFFI:MEM-AREF M TYPE 1) (CFFI:MEM-AREF M TYPE 7)
	 (CFFI:MEM-AREF M TYPE 8) (CFFI:MEM-AREF M TYPE 14))
      (* (CFFI:MEM-AREF M TYPE 2) (CFFI:MEM-AREF M TYPE 4)
	 (CFFI:MEM-AREF M TYPE 11) (CFFI:MEM-AREF M TYPE 13))
      (* (CFFI:MEM-AREF M TYPE 2) (CFFI:MEM-AREF M TYPE 5)
	 (CFFI:MEM-AREF M TYPE 8) (CFFI:MEM-AREF M TYPE 15))
      (* (CFFI:MEM-AREF M TYPE 2) (CFFI:MEM-AREF M TYPE 7)
	 (CFFI:MEM-AREF M TYPE 9) (CFFI:MEM-AREF M TYPE 12))
      (* (CFFI:MEM-AREF M TYPE 3) (CFFI:MEM-AREF M TYPE 4)
	 (CFFI:MEM-AREF M TYPE 9) (CFFI:MEM-AREF M TYPE 14))
      (* (CFFI:MEM-AREF M TYPE 3) (CFFI:MEM-AREF M TYPE 5)
	 (CFFI:MEM-AREF M TYPE 10) (CFFI:MEM-AREF M TYPE 12))
      (* (CFFI:MEM-AREF M TYPE 3) (CFFI:MEM-AREF M TYPE 6)
	 (CFFI:MEM-AREF M TYPE 8) (CFFI:MEM-AREF M TYPE 13)))))


(defmethod determinate ((this transform))
  "Determinate of Transform. Returns a scalar."
  (det (transform this) :type (qtype this)))


(defun m-1 (in out &key (type :float))
  "get raw cffi matrix inverse."
  (cffi:with-foreign-object (tmp type 16)

    (setf (cffi:mem-aref tmp type 0)  
	  (+ (* (cffi:mem-aref in type 5)  (cffi:mem-aref in type 10) (cffi:mem-aref in type 15))
	     (- (* (cffi:mem-aref in type 5)  (cffi:mem-aref in type 11) (cffi:mem-aref in type 14)))
	     (- (* (cffi:mem-aref in type 9)  (cffi:mem-aref in type 6)  (cffi:mem-aref in type 15)))
	     (+ (cffi:mem-aref in type 9)  (cffi:mem-aref in type 7)  (cffi:mem-aref in type 14))
	     (+ (cffi:mem-aref in type 13) (cffi:mem-aref in type 6)  (cffi:mem-aref in type 11))
	     (- (* (cffi:mem-aref in type 13) (cffi:mem-aref in type 7) (cffi:mem-aref in type 10)))))

    (setf (cffi:mem-aref tmp type 4)
	  (+ 
	   (* (- (cffi:mem-aref in type 4))  (cffi:mem-aref in type 10) (cffi:mem-aref in type 15))
	   (* (cffi:mem-aref in type 4)  (cffi:mem-aref in type 11) (cffi:mem-aref in type 14))
	   (* (cffi:mem-aref in type 8) (cffi:mem-aref in type 6)  (cffi:mem-aref in type 15))
	   (- (* (cffi:mem-aref in type 8)  (cffi:mem-aref in type 7) (cffi:mem-aref in type 14)))
	   (- (* (cffi:mem-aref in type 12) (cffi:mem-aref in type 6) (cffi:mem-aref in type 11)))
	   (* (cffi:mem-aref in type 12) (cffi:mem-aref in type 7) (cffi:mem-aref in type 10))))

    (setf (cffi:mem-aref tmp type 8)
	  (+ (* (cffi:mem-aref in type 4) (cffi:mem-aref in type 9) (cffi:mem-aref in type 15))
	     (- (* (cffi:mem-aref in type 4) (cffi:mem-aref in type 11) (cffi:mem-aref in type 13)))
	     (- (* (cffi:mem-aref in type 8) (cffi:mem-aref in type 5)  (cffi:mem-aref in type 15)))
	     (* (cffi:mem-aref in type 8) (cffi:mem-aref in type 7) (cffi:mem-aref in type 13))
	     (* (cffi:mem-aref in type 12) (cffi:mem-aref in type 5) (cffi:mem-aref in type 11))
	     (- (* (cffi:mem-aref in type 12) (cffi:mem-aref in type 7) (cffi:mem-aref in type 9)))))

    (setf (cffi:mem-aref tmp type 12)
	  (+ (* (- (cffi:mem-aref in type 4)) (cffi:mem-aref in type 9) (cffi:mem-aref in type 14))
	     (* (cffi:mem-aref in type 4) (cffi:mem-aref in type 10) (cffi:mem-aref in type 13))
	     (* (cffi:mem-aref in type 8) (cffi:mem-aref in type 5)  (cffi:mem-aref in type 14))
	     (- (* (cffi:mem-aref in type 8) (cffi:mem-aref in type 6) (cffi:mem-aref in type 13)))
	     (- (* (cffi:mem-aref in type 12) (cffi:mem-aref in type 5) (cffi:mem-aref in type 10)))
	     (* (cffi:mem-aref in type 12) (cffi:mem-aref in type 6) (cffi:mem-aref in type 9))))

    (setf (cffi:mem-aref tmp type 1)
	  (+ (* (- (cffi:mem-aref in type 1)) (cffi:mem-aref in type 10) (cffi:mem-aref in type 15))
	     (* (cffi:mem-aref in type 1)  (cffi:mem-aref in type 11) (cffi:mem-aref in type 14))
	     (* (cffi:mem-aref in type 9)  (cffi:mem-aref in type 2)  (cffi:mem-aref in type 15))
	     (- (* (cffi:mem-aref in type 9)  (cffi:mem-aref in type 3) (cffi:mem-aref in type 14)))
	     (- (* (cffi:mem-aref in type 13) (cffi:mem-aref in type 2) (cffi:mem-aref in type 11)))
	     (* (cffi:mem-aref in type 13) (cffi:mem-aref in type 3) (cffi:mem-aref in type 10))))

    (setf (cffi:mem-aref tmp type 5)
	  (+ (* (cffi:mem-aref in type 0) (cffi:mem-aref in type 10) (cffi:mem-aref in type 15))
	     (- (* (cffi:mem-aref in type 0) (cffi:mem-aref in type 11) (cffi:mem-aref in type 14)))
	     (- (* (cffi:mem-aref in type 8) (cffi:mem-aref in type 2)  (cffi:mem-aref in type 15)))
	     (* (cffi:mem-aref in type 8) (cffi:mem-aref in type 3) (cffi:mem-aref in type 14))
	     (* (cffi:mem-aref in type 12) (cffi:mem-aref in type 2) (cffi:mem-aref in type 11))
	     (- (* (cffi:mem-aref in type 12) (cffi:mem-aref in type 3) (cffi:mem-aref in type 10)))))

    (setf (cffi:mem-aref tmp type 9)
	  (+ (* (- (cffi:mem-aref in type 0)) (cffi:mem-aref in type 9) (cffi:mem-aref in type 15))
	     (* (cffi:mem-aref in type 0) (cffi:mem-aref in type 11) (cffi:mem-aref in type 13))
	     (* (cffi:mem-aref in type 8) (cffi:mem-aref in type 1)  (cffi:mem-aref in type 15))
	     (- (* (cffi:mem-aref in type 8)  (cffi:mem-aref in type 3) (cffi:mem-aref in type 13)))
	     (- (* (cffi:mem-aref in type 12) (cffi:mem-aref in type 1) (cffi:mem-aref in type 11)))
	     (* (cffi:mem-aref in type 12) (cffi:mem-aref in type 3) (cffi:mem-aref in type 9))))

    (setf (cffi:mem-aref tmp type 13)
	  (+ (* (cffi:mem-aref in type 0) (cffi:mem-aref in type 9) (cffi:mem-aref in type 14))
	     (- (* (cffi:mem-aref in type 0) (cffi:mem-aref in type 10) (cffi:mem-aref in type 13)))
	     (- (* (cffi:mem-aref in type 8) (cffi:mem-aref in type 1)  (cffi:mem-aref in type 14)))
	     (* (cffi:mem-aref in type 8) (cffi:mem-aref in type 2) (cffi:mem-aref in type 13))
	     (* (cffi:mem-aref in type 12) (cffi:mem-aref in type 1) (cffi:mem-aref in type 10))
	     (- (* (cffi:mem-aref in type 12) (cffi:mem-aref in type 2) (cffi:mem-aref in type 9)))))
    
    (setf (cffi:mem-aref tmp type 2)
	  (+ (* (cffi:mem-aref in type 1) (cffi:mem-aref in type 6) (cffi:mem-aref in type 15))
	     (- (* (cffi:mem-aref in type 1) (cffi:mem-aref in type 7) (cffi:mem-aref in type 14)))
	     (- (* (cffi:mem-aref in type 5) (cffi:mem-aref in type 2) (cffi:mem-aref in type 15)))
	     (* (cffi:mem-aref in type 5) (cffi:mem-aref in type 3) (cffi:mem-aref in type 14))
	     (* (cffi:mem-aref in type 13) (cffi:mem-aref in type 2) (cffi:mem-aref in type 7))
	     (- (* (cffi:mem-aref in type 13) (cffi:mem-aref in type 3) (cffi:mem-aref in type 6)))))

    (setf (cffi:mem-aref tmp type 6)
	  (+ (* (- (cffi:mem-aref in type 0)) (cffi:mem-aref in type 6) (cffi:mem-aref in type 15))
	     (* (cffi:mem-aref in type 0) (cffi:mem-aref in type 7) (cffi:mem-aref in type 14))
	     (* (cffi:mem-aref in type 4) (cffi:mem-aref in type 2) (cffi:mem-aref in type 15))
	     (- (* (cffi:mem-aref in type 4) (cffi:mem-aref in type 3) (cffi:mem-aref in type 14)))
	     (- (* (cffi:mem-aref in type 12) (cffi:mem-aref in type 2) (cffi:mem-aref in type 7)))
	     (* (cffi:mem-aref in type 12) (cffi:mem-aref in type 3) (cffi:mem-aref in type 6))))

    (setf (cffi:mem-aref tmp type 10)
	  (+ (* (cffi:mem-aref in type 0)  (cffi:mem-aref in type 5) (cffi:mem-aref in type 15))
	     (- (* (cffi:mem-aref in type 0) (cffi:mem-aref in type 7) (cffi:mem-aref in type 13)))
	     (- (* (cffi:mem-aref in type 4) (cffi:mem-aref in type 1) (cffi:mem-aref in type 15)))
	     (* (cffi:mem-aref in type 4) (cffi:mem-aref in type 3) (cffi:mem-aref in type 13))
	     (* (cffi:mem-aref in type 12) (cffi:mem-aref in type 1) (cffi:mem-aref in type 7))
	     (- (* (cffi:mem-aref in type 12) (cffi:mem-aref in type 3) (cffi:mem-aref in type 5)))))

    (setf (cffi:mem-aref tmp type 14)
	  (+ (* (- (cffi:mem-aref in type 0))  (cffi:mem-aref in type 5) (cffi:mem-aref in type 14)) 
	     (* (cffi:mem-aref in type 0) (cffi:mem-aref in type 6) (cffi:mem-aref in type 13))
	     (* (cffi:mem-aref in type 4) (cffi:mem-aref in type 1) (cffi:mem-aref in type 14))
	     (- (* (cffi:mem-aref in type 4) (cffi:mem-aref in type 2) (cffi:mem-aref in type 13)))
	     (- (* (cffi:mem-aref in type 12) (cffi:mem-aref in type 1) (cffi:mem-aref in type 6)))
	     (* (cffi:mem-aref in type 12) (cffi:mem-aref in type 2) (cffi:mem-aref in type 5))))

    (setf (cffi:mem-aref tmp type 3)
	  (+ (* (- (cffi:mem-aref in type 1)) (cffi:mem-aref in type 6) (cffi:mem-aref in type 11)) 
	     (* (cffi:mem-aref in type 1) (cffi:mem-aref in type 7) (cffi:mem-aref in type 10)) 
	     (* (cffi:mem-aref in type 5) (cffi:mem-aref in type 2) (cffi:mem-aref in type 11))
	     (- (* (cffi:mem-aref in type 5) (cffi:mem-aref in type 3) (cffi:mem-aref in type 10)))
	     (- (* (cffi:mem-aref in type 9) (cffi:mem-aref in type 2) (cffi:mem-aref in type 7)))
	     (* (cffi:mem-aref in type 9) (cffi:mem-aref in type 3) (cffi:mem-aref in type 6))))

    (setf (cffi:mem-aref tmp type 7)
	  (+ (* (cffi:mem-aref in type 0) (cffi:mem-aref in type 6) (cffi:mem-aref in type 11))
	     (- (* (cffi:mem-aref in type 0) (cffi:mem-aref in type 7) (cffi:mem-aref in type 10)))
	     (- (* (cffi:mem-aref in type 4) (cffi:mem-aref in type 2) (cffi:mem-aref in type 11)))
	     (* (cffi:mem-aref in type 4) (cffi:mem-aref in type 3) (cffi:mem-aref in type 10))
	     (* (cffi:mem-aref in type 8) (cffi:mem-aref in type 2) (cffi:mem-aref in type 7))
	     (- (* (cffi:mem-aref in type 8) (cffi:mem-aref in type 3) (cffi:mem-aref in type 6)))))
    
    (setf (cffi:mem-aref tmp type 11)
	  (+ (* (- (cffi:mem-aref in type 0)) (cffi:mem-aref in type 5) (cffi:mem-aref in type 11))
	     (* (cffi:mem-aref in type 0) (cffi:mem-aref in type 7) (cffi:mem-aref in type 9))
	     (* (cffi:mem-aref in type 4) (cffi:mem-aref in type 1) (cffi:mem-aref in type 11))
	     (- (* (cffi:mem-aref in type 4) (cffi:mem-aref in type 3) (cffi:mem-aref in type 9)))
	     (- (* (cffi:mem-aref in type 8) (cffi:mem-aref in type 1) (cffi:mem-aref in type 7)))
	     (* (cffi:mem-aref in type 8) (cffi:mem-aref in type 3) (cffi:mem-aref in type 5))))

    (setf (cffi:mem-aref tmp type 15)
	  (+ (* (cffi:mem-aref in type 0) (cffi:mem-aref in type 5) (cffi:mem-aref in type 10))
	     (- (* (cffi:mem-aref in type 0) (cffi:mem-aref in type 6) (cffi:mem-aref in type 9)))
	     (- (* (cffi:mem-aref in type 4) (cffi:mem-aref in type 1) (cffi:mem-aref in type 10)))
	     (* (cffi:mem-aref in type 4) (cffi:mem-aref in type 2) (cffi:mem-aref in type 9))
	     (* (cffi:mem-aref in type 8) (cffi:mem-aref in type 1) (cffi:mem-aref in type 6))
	     (- (* (cffi:mem-aref in type 8) (cffi:mem-aref in type 2) (cffi:mem-aref in type 5)))))

    (let ((det (+ (* (cffi:mem-aref in type 0) (cffi:mem-aref tmp type 0))
		  (* (cffi:mem-aref in type 1) (cffi:mem-aref tmp type 4))
		  (* (cffi:mem-aref in type 2) (cffi:mem-aref tmp type 8))
		  (* (cffi:mem-aref in type 3) (cffi:mem-aref tmp type 12)))))
      (if (zerop det)
	  nil
	  (loop with d = (/ det)
	     for i from 0 to 15
	     do (setf (cffi:mem-aref out type i)
		      (* (cffi:mem-aref tmp type i) d))
	     finally (return out))))))


(defmethod invert ((this transform) &optional in-place)
  "Get inverse of transform. If in-place is true, the matrix will be modified. It still makes a temporary raw matrix until it is copied."
  (if in-place
      (progn
	(m-1 (transform this) (transform this) :type (qtype this))
	this)
      (let ((ret (make-instance 'transform :qtype (qtype this))))
	(m-1 (transform this) (transform ret) :type (qtype this))
	ret)))



(defun set-scale-transform (m x y z &optional (type :float))
  "create Raw CFFI scale matrix."
  (loop for count from 0
     for i in `(,(float x)   0.0                     0.0 0.0
		 0.0         ,(float y)              0.0 0.0
		 0.0         0.0         ,(float z)  0.0
		 0.0         0.0         0.0         1.0)
     do (setf (cffi:mem-aref m type count) i))
  m)

(defmethod scale ((this transform) x y z &optional in-place)
  "Scale a transform by x, y and z. If in-place is true, the matrix will be modified. It still makes a temporary raw matrix until it is copied."
  (cffi:with-foreign-object  (scale :float 16)
    (set-scale-transform scale x y z (qtype this))
    
    (if in-place
	(progn
	  (m*m (transform this) scale (transform this))
	  this)
	(let ((ret (make-foreign-array 16 (qtype this))))
	  (make-instance 'transform
			 :qtype (qtype this)
			 :transform (m*m (transform this) scale ret))))))


(defun set-translation-transform (m x y z type)
  "Create raw CFFI translation matrix."
  (loop for counter from 0
     for i in `(1.0 0.0 0.0 0.0 
		    0.0 1.0 0.0 0.0
		    0.0 0.0 1.0 0.0
		      ,(float x)   ,(float y)   ,(float z) 1.0)
     do (setf (cffi:mem-aref m type counter) i))
  m)

(defmethod translate ((this transform) x y z &optional in-place)
  "Translate a transform by x, y and z. If in-place is true, the matrix will be modified. It still makes a temporary raw matrix until it is copied."
  (cffi:with-foreign-object  (translation :float 16)
    (set-translation-transform translation x y z (qtype this))
    
    (if in-place
	(progn
	  (m*m (transform this) translation (transform this))
	  this)
	(let ((ret (make-foreign-array 16 (qtype this))))
	  (make-instance 'transform
			 :qtype (qtype this)
			 :transform (m*m (transform this) translation ret))))))



(defun set-rotation-transform (m angle x y z &optional (type :float))
  "Create a raw CFFI rotation matrix."
  (let* ((n (sqrt (+ (* x x)
		     (* y y)
		     (* z z))))
	 (x (/ x n))
	 (y (/ y n))
	 (z (/ z n))
	 (c (cos angle))
	 (s (sin angle))
	 (1-c (- 1 c)))

    (loop for index from 0
       for i in (map 'list #'float
		     `(,(+ (* x x 1-c) c)        ,(+ (* y x 1-c) (* z s))  ,(- (* x z 1-c) (* y s)) 0
			,(- (* x y 1-c) (* z s))    ,(+ (* y y 1-c) c)        ,(+ (* y z 1-c) (* x s)) 0
			,(+ (* x z 1-c) (* y s))    ,(- (* y z 1-c) (* x s))  ,(+ (* z z 1-c) c)       0
			0                           0                         0                        1))
       do (setf (cffi:mem-aref m type index) i))
    m))


(defmethod rotate ((this transform) angle x y z &optional in-place)
  "Rotate a transform by angle about the (x, y, z) vector. If in-place is true, the matrix will be modified. It still makes a temporary raw matrix until it is copied."  
  (cffi:with-foreign-object  (rotation :float 16)
    (set-rotation-transform rotation angle x y z (qtype this))
    
    (if in-place
	(progn
	  (m*m (transform this) rotation (transform this))
	  this)

	(let ((ret (make-instance 'transform :qtype (qtype this) :transform  (make-foreign-array 16 (qtype this)))))
	  (m*m (transform this) rotation (transform ret))
	  ret))))




(defun _make-orthogonal-transform (width height near far &optional (type :float))
  "Create a raw CFFI orthogonal matrix."
  (make-foreign-array 16 type (map 'list #'float `(,(/ 2 width) 0.0 0.0 0.0
						    0.0 ,(/ 2 height) 0.0 0.0
						    0.0 0.0 ,(/ (- far near)) ,(/ (- near) (- far near)) 
						    0.0 0.0 0.0 1.0))))

(defun make-orthogonal-transform (width height near far &optional (type :float))
  "Sets transform to orthogonal transform. EXISTING DATA WILL BE DELETED!"
  (make-instance 'transform
		 :transform (_make-orthogonal-transform width height near far type)
		 :qtype type))


(defun _frustum (m left right bottom top near far &optional (type :float))
  "Create a raw CFFI frustum matrix."  
  (let ((a (/ (+ right left) (- right left)))
	(b (/ (+ top bottom) (- top bottom)))
	(c (- (/ (+ far near) (- far near))))
	(d (- (/ (* 2 far near) (- far near)))))
    
    (loop for x from 0
       for i in (map 'list (lambda (x) (coerce x (case type
						   (:float 'single-float)
						   (:double 'double-float))))
		     `(,(/ (* 2 near) (- right left)) 0 ,A 0
			0 ,(/ (* 2 near) (- top bottom)) ,B 0
			0 0 ,C -1
			0 0 ,D 0))
       do (setf (cffi:mem-aref m type x) i))
    m))

(defun make-frustum-transform (left right bottom top near far &optional (type :float))
  "Sets transform to frustum transform. EXISTING DATA WILL BE DELETED!"
  (make-instance 'transform
		 :transform (_frustum (make-foreign-array 16 type) left right bottom top near far type)
		 :qtype type))


(defun _perspective  (m fovy aspect znear zfar &optional (type :float))
  "Create a raw CFFI perspective matrix."
  (let* ((ymax (* znear (tan fovy)))
	 (xmax (* ymax aspect)))
    (_frustum m (- xmax) xmax (- ymax) ymax znear zfar type)))


(defun make-perspective-transform (fovy aspect near far &optional (type :float))
  "Sets transform to perspective transform. EXISTING DATA WILL BE DELETED!"
  (make-instance 'transform
		 :transform (_perspective (make-foreign-array 16 type) fovy aspect near far type)
		 :qtype type))


(defun get-current-gl-matrix (&optional (type :modelview-matrix))
  "Convenience function to get current matrix on gl stack."
  (let ((m (make-instance 'transform)))
    (%gl:get-float-v type (transform m))
    m))

(defmethod use-transform ((this transform))
  "Convenience function to write transform to current active gl stack."
  (gl:matrix-mode :modelview)
  (%gl:load-matrix-f (transform this)))

(defmethod use-projection-transform ((this transform))
  "Convenience function to write transform to current active gl projection stack."
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (%gl:load-matrix-f (transform this))
  (gl:matrix-mode :modelview))


(defmethod M*V ((this transform) (that list))
  "Quick function to test results of transformations."
  (with-accessors ((transform transform)) this
    (let ((x (first that))
	  (y (second that))
	  (z (third that)))
      (list (+ (* x (cffi:mem-aref transform (qtype this) 0))
	       (* y (cffi:mem-aref transform (qtype this) 4))
	       (* z (cffi:mem-aref transform (qtype this) 8))
	       (cffi:mem-aref transform (qtype this) 12))
	    
	    (+ (* x (cffi:mem-aref transform (qtype this) 1))
	       (* y (cffi:mem-aref transform (qtype this) 5))
	       (* z (cffi:mem-aref transform (qtype this) 9))
	       (cffi:mem-aref transform (qtype this) 13))
	    
	    (+ (* x (cffi:mem-aref transform (qtype this) 2))
	       (* y (cffi:mem-aref transform (qtype this) 6))
	       (* z (cffi:mem-aref transform (qtype this) 10))
	       (cffi:mem-aref transform (qtype this) 14))))))

