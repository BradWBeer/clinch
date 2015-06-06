(in-package #:clode)

(defclass physics-player ()
  ((body :initform (error "You must pass a physics-body to a physics-player object!")
	 :initarg :body
	 :reader body)))


(defmethod initialize-instance :after ((this physics-player) &key)

  (let ((pbody (clode::pointer (clode::body this))))
    
  (clode:body-set-max-angular-speed pbody 0)
  (clode::body-set-kinematic pbody)
  (clode:Body-Set-Auto-Disable-Flag pbody 0)
  (clode:Body-Set-Angular-Damping pbody 0)
  (clode:Body-Set-linear-Damping pbody 0)))


