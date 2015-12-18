(in-package #:clinch)

(defclass physics-player ()
  ((body :initform (error "You must pass a physics-body to a physics-player object!")
	 :initarg :body
	 :reader body)
   (max-walk :initform 1
	     :initarg max-walk
	     :accessor max-walk)
   (max-run :initform 2
	    :initarg :max-run
	    :initarg max-run)
   (max-run-time :initform nil
		 :initarg :max-run-time
		 :accessor max-run-time)
   (horizontal :initform 0
	       :initarg :horizontal
	       :accessor horizontal)
   (vertical :initform 0
	     :initarg :vertical
	     :accessor vertical)
   (jump-force :initform '(0 10 0)
	       :initarg :jump-force
	       :accessor jump-force)
   (jumps :initform 1
	  :initarg :jumps
	  :accessor jumps)))

(defmethod initialize-instance :after ((this physics-player) &key)

  (let ((pbody (ode::pointer (ode::body this))))
    
  (ode:body-set-max-angular-speed pbody 0)
  (ode::body-set-kinematic pbody)
  (ode:Body-Set-Auto-Disable-Flag pbody 0)
  (ode:Body-Set-Angular-Damping pbody 0)
  (ode:Body-Set-linear-Damping pbody 0)))


(defun mouse-math (x y z horz vert) 
  (sb-cga:matrix* (sb-cga:rotate-around (sb-cga:vec 1.0 0.0 0.0) (clinch:degrees->radians vert))
		  
		  (sb-cga:matrix* (sb-cga:rotate-around (sb-cga:vec 0.0 1.0 0.0) (clinch:degrees->radians horz))
				  
				  (sb-cga:translate* (clinch::ensure-float x)
						     (clinch::ensure-float y)
						     (clinch::ensure-float z)))))


(defmethod load-player-camera ((this physics-player) projection-matrix)

  (gl:matrix-mode :projection)
  (let ((pos (ode:body-get-position (ode::pointer (ode::body this)))))
    
    (gl:load-matrix (sb-cga:matrix* projection-matrix (mouse-math (- (aref pos 0))
								  (- (aref pos 1))
								  (- (aref pos 2))
								  (ode:horizontal this)
								  (ode:vertical this)))))
  (gl:matrix-mode :modelview))
  


(defmethod jump ((this physics-player))
  (add-to-body-velocity (body this)
			(jump-force this)))
