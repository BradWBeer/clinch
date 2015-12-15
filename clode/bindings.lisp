(in-package #:clode)

(cl:eval-when (:compile-toplevel :load-toplevel)
  (cl:unless (cl:fboundp 'swig-lispify-noprefix)
    (cl:defun swig-lispify-noprefix (name flag cl:&optional (package cl:*package*))
      (cl:labels ((helper (lst last rest cl:&aux (c (cl:car lst)))
                    (cl:cond
                      ((cl:null lst)
                       rest)
                      ((cl:upper-case-p c)
                       (helper (cl:cdr lst) 'upper
                               (cl:case last
                                 (lower (cl:list* c #\- rest))
                                 (cl:t (cl:cons c rest)))))
                      ((cl:lower-case-p c)
                       (helper (cl:cdr lst) 'lower (cl:cons (cl:char-upcase c) rest)))
                      ((cl:digit-char-p c)
                       (helper (cl:cdr lst) 'digit 
                               (cl:case last
                                 ((upper lower) (cl:list* c #\- rest))
                                 (cl:t (cl:cons c rest)))))
                      ((cl:char-equal c #\_)
                       (helper (cl:cdr lst) '_ (cl:cons #\- rest)))
                      (cl:t
                       (cl:error "Invalid character: ~A" c))))
                  (strip-prefix (prf str)
                    (let ((l (length prf)))
                      (if (and (> (length str) l) (string= prf (subseq str 0 l)))
			  (subseq str l)
			  str))))
        (cl:let ((fix (cl:case flag
                        ((constant enumvalue) "+")
                        (variable "*")
                        (cl:t ""))))
          (cl:intern
           (cl:concatenate
            'cl:string
            fix
            (cl:nreverse (helper (cl:concatenate 'cl:list (strip-prefix "d" name)) cl:nil cl:nil))
            fix)
           package))))))


(defmacro defcfun-rename-function (name &rest rest)
  (let ((lisp-name (swig-lispify-noprefix name 'function)))
    `(progn
       (defcfun (,name ,lisp-name)
	   ,@rest)
       (cl:export (swig-lispify-noprefix ,name 'function)))))


(defcfun-rename-function "dGetConfiguration" :string)
  
(defvar is-double-precision?)
(setf is-double-precision? (search "ODE_double_precision" (get-configuration)))


(defun number->dreal (x)
  (coerce x (if is-double-precision?
		'double-float
		'single-float)))

;;(if is-double-precision?
    (defctype dreal (:wrapper :double
			      :to-c number->dreal))
   ;; (defctype dreal (:wrapper :float
   ;; 			      :to-c number->dreal)))


(defmacro infinity (&optional (precision is-double-precision?))
  `(if (eql ,precision :single)
       (progn
         #+sbcl sb-ext:single-float-positive-infinity
         #+clozure 1S++0
         #+abcl ext:single-float-positive-infinity
         #+allegro excl::*infinity-single*
         #+cmu ext:single-float-positive-infinity
         #+(and ecl (not infinity-not-available)) si:single-float-positive-infinity
         #+lispworks (coerce infinity$$ 'single-float)
         #+scl ext:single-float-positive-infinity
         #+t most-positive-single-float)
       (progn
         #+sbcl sb-ext:double-float-positive-infinity
         #+clozure 1D++0
         #+abcl ext:double-float-positive-infinity
         #+allegro excl::*infinity-double*
         #+cmu ext:double-float-positive-infinity
         #+(and ecl (not infinity-not-available)) si:double-float-positive-infinity
         #+lispworks #.(read-from-string "10E999")
         #+scl ext:double-float-positive-infinity
         #+t most-positive-double-float)))

(defmacro ode-num (x)
  `(coerce ,x 'double-float))


(defbitfield Contact-Enum
  (:Mu2		 #x001)
  (:FDir1	 #x002)
  (:Bounce	 #x004)
  (:Soft-ERP	 #x008)
  (:Soft-CFM	 #x010)
  (:Motion1	 #x020)
  (:Motion2	 #x040)
  (:MotionN	 #x080)
  (:Slip1	 #x100)
  (:Slip2	 #x200)
  (:Rolling      #x400)
  (:Approx0	 #x0000)
  (:Approx1-1	 #x1000)
  (:Approx1-2	 #x2000)
  (:Approx1-N    #x4000)
  (:Approx1      #x7000))



(defctype dVector3 (:array dReal 4))
(defctype dVector4 (:array dReal 4))
(defctype dMatrix3 (:array dReal 12))
(defctype dMatrix4 (:array dReal 16))
(defctype dMatrix6 (:array dReal 48))
(defctype dQuaternion (:array dReal 4))

(defcstruct dMass
  (mass dReal)
  (c dVector3)
  (i dMatrix3))

(defctype dWorldID :pointer)
(defctype dSpaceID :pointer)
(defctype dBodyID :pointer)
(defctype dGeomID :pointer)
(defctype dJointID :pointer)
(defctype dJointGroupID :pointer)

(defcstruct dSurfaceParameters 
  (mode Contact-Enum)
  (mu dReal)
  (mu2 dReal)
  (rho dReal)
  (rho2 dReal)
  (rhoN dReal)
  (bounce dReal)
  (bounce-vel dReal)
  (soft-erp dReal)
  (soft-cfm dReal)
  (motion1 dReal)
  (motion2 dReal)
  (motionN dReal)
  (slip1 dReal)
  (slip2 dReal))



(defcstruct dContactGeom 
  (pos dVector3)
  (normal dVector3)
  (depth dReal)
  (g1 dGeomID)
  (g2 dGeomID)
  (side1 :int)
  (side2 :int))


(defcstruct dContact 
  (surface (:struct dSurfaceParameters))
  (geom (:struct dContactGeom))
  (fdir1 dVector3))

(defcfun-rename-function "dInitODE" :void)

(defcfun-rename-function "dInitODE2" :void
  (init-flags :unsigned-int))

(defcfun-rename-function "dCloseODE" :void)




(defcfun-rename-function "dBodyCreate" dBodyID
  (world dWorldID))

(defcfun-rename-function "dBodyDestroy" dBodyID
  (world dWorldID))

(defcfun-rename-function "dBodyGetPosition" dVector3
  (body dBodyID))

(defcfun-rename-function "dBodyGetRotation" dVector3
  (body dBodyID))

(defcfun-rename-function "dBodySetMass" :void
  (body dBodyID)
  (mass (:pointer dMass)))

(defcfun-rename-function "dBodySetPosition" :void
  (body dBodyID)
  (x dReal)
  (y dReal)
  (z dReal))

(defcfun-rename-function "dBodySetLinearVel" :void
  (body dBodyID)
  (x dReal)
  (y dReal)
  (z dReal))


(defcfun-rename-function "dBodySetAngularVel" :void
  (body dBodyID)
  (x dReal)
  (y dReal)
  (z dReal))

(defcfun-rename-function "dBodyGetLinearVel" dVector3
  (body dBodyID))

(defcfun-rename-function "dBodyGetAngularVel" dVector3
  (body dBodyID))

(defcfun-rename-function "dBodySetMaxAngularSpeed" :void
  (body dBodyID)
  (max-speed dReal))

(defcfun-rename-function "dBodySetRotation" :void
  (body dBodyID)
  (R dMatrix3))

(defcfun-rename-function "dBodyEnable" :void
  (body dBodyID))

(defcfun-rename-function "dBodyDisable" :void
  (body dBodyID))


(defcfun-rename-function "dBodyIsEnabled" :int
  (body dBodyID))

(defcfun-rename-function "dBodySetAutoDisableFlag" :void
  (body dBodyID)
  (auto-disable :int))

(defcfun-rename-function "dBodyGetAutoDisableFlag" :int
  (body dBodyID))

(defcfun-rename-function "dBodyAddForce" :void 
  (body dBodyID)
  (fx dReal)
  (fy dReal)
  (fz dReal))

(defcfun-rename-function "dBodyAddTorque" :void
  (body dBodyID)
  (fx dReal)
  (fy dReal)
  (fz dReal))

(defcfun-rename-function "dBodyAddRelForce" :void 
  (body dBodyID)
  (fx dReal)
  (fy dReal)
  (fz dReal))

(defcfun-rename-function "dBodyAddRelTorque" :void
  (body dBodyID)
  (fx dReal)
  (fy dReal)
  (fz dReal))

(defcfun-rename-function "dBodyAddForceAtPos" :void
  (body dBodyID)
  (fx dReal)
  (fy dReal) 
  (fz dReal)
  (px dReal)
  (py dReal)
  (pz dReal))

(defcfun-rename-function "dBodyAddForceAtRelPos" :void
  (body dBodyID)
  (fx dReal)
  (fy dReal) 
  (fz dReal)
  (px dReal)
  (py dReal)
  (pz dReal))

(defcfun-rename-function "dBodyAddRelForceAtPos" :void
  (body dBodyID)
  (fx dReal)
  (fy dReal) 
  (fz dReal)
  (px dReal)
  (py dReal)
  (pz dReal))

(defcfun-rename-function "dBodyAddRelForceAtRelPos" :void
  (body dBodyID)
  (fx dReal)
  (fy dReal) 
  (fz dReal)
  (px dReal)
  (py dReal)
  (pz dReal))

(defcfun-rename-function "dBodyGetForce" dVector3 
  (body dBodyID))

(defcfun-rename-function "dBodyGetTorque" dVector3 
  (body dBodyID))

(defcfun-rename-function "dBodySetForce" :void 
  (body dBodyID)
  (x dReal)
  (y dReal) 
  (z dReal))

(defcfun-rename-function "dBodySetTorque" :void 
  (body dBodyID)
  (x dReal)
  (y dReal) 
  (z dReal))

(defcfun-rename-function "dBodySetDynamic" :void 
  (body dBodyID))

(defcfun-rename-function "dBodySetKinematic" :void 
  (body dBodyID))

(defcfun-rename-function "dBodyIsKinematic" :int 
  (body dBodyID))

(defcfun-rename-function "dBodyVectorToWorld" :void
  (body dBodyID)
  (px dReal)
  (py dReal)
  (pz dReal)
  (result dVector3))

(defcfun-rename-function "dBodyVectorFromWorld" :void
  (body dBodyID)
  (px dReal)
  (py dReal)
  (pz dReal)
  (result dVector3))

(defcfun-rename-function "dBodySetAutoDisableLinearThreshold" :void 
  (body dBodyID)
  (linear-threshold dReal))

(defcfun-rename-function "dBodyGetAutoDisableLinearThreshold" dReal
  (body dBodyID))

(defcfun-rename-function "dBodySetAutoDisableAngularThreshold" :void  
  (body dBodyID) 
  (angular-threshold dReal))

(defcfun-rename-function "dBodyGetAutoDisableAngularThreshold" dReal
  (body dBodyID))

(defcfun-rename-function "dBodySetAutoDisableSteps" :void
  (body dBodyID)
  (steps :int))

(defcfun-rename-function "dBodyGetAutoDisableSteps" :int
  (body dBodyID))

(defcfun-rename-function "dBodySetAutoDisableTime" :void
  (body dBodyID)
  (time dReal))

(defcfun-rename-function "dBodyGetAutoDisableTime" dReal
  (body dBodyID))

(defcfun-rename-function "dBodySetAutoDisableAverageSamplesCount" :void 
  (body dBodyID)
  (average-samples-count :unsigned-int))

(defcfun-rename-function "dBodyGetAutoDisableAverageSamplesCount" :int 
  (body dBodyID))

(defcfun-rename-function "dBodySetAutoDisableDefaults" :void 
  (body dBodyID))

(defcfun-rename-function "dBodySetMovedCallback" :void 
  (body dBodyID)
  (callback :pointer))


(defcfun-rename-function "dBodyGetLinearDamping" dReal
  (body dBodyID))

(defcfun-rename-function "dBodyGetAngularDamping" dReal
  (body dBodyID))

(defcfun-rename-function "dBodySetLinearDamping" :void
  (body dBodyID)
  (scale dReal)) 

(defcfun-rename-function "dBodySetAngularDamping" :void
  (body dBodyID)
  (scale dReal))

(defcfun-rename-function "dGeomSetPosition" :void
  (geom dGeomID)
  (x dReal)
  (y dReal)
  (z dReal))

(defcfun-rename-function "dGeomSetRotation" :void
  (geom dGeomID)
  (r dMatrix3))

(defcfun-rename-function "dGeomSetQuaternion" :void
  (geom dGeomID)
  (q dQuaternion))

(defcfun-rename-function "dGeomGetPosition" dVector3
  (geom dGeomID))

(defcfun-rename-function "dGeomGetRotation" dMatrix3
  (geom dGeomID))

(defcfun-rename-function "dGeomGetQuaternion" :void
  (geom dGeomID)
  (result dQuaternion))

(defcfun-rename-function "dGeomSetOffsetPosition" :void
  (geom dGeomID)
  (x dReal)
  (y dReal)
  (z dReal))

(defcfun-rename-function "dGeomSetOffsetRotation" :void
  (geom dGeomID)
  (r dMatrix3))

(defcfun-rename-function "dGeomSetOffsetQuaternion" :void 
  (geom dGeomID)
  (q dQuaternion))

(defcfun-rename-function "dGeomClearOffset" :void
  (geom dGeomID))

(defcfun-rename-function "dCreateBox" dGeomID
  (space dSpaceID)
  (lx dReal)
  (ly dReal)
  (lz dReal))

(defcfun-rename-function "dCreatePlane" dGeomID
  (space dSpaceID)
  (a dReal)
  (b dReal)
  (c dReal)
  (d dReal))

(defcfun-rename-function "dGeomDestroy" :void
  (obj dGeomID))

(defcfun-rename-function "dGeomSetBody" :void
  (geom dGeomID)
  (body dBodyID))

(defcfun-rename-function "dHashSpaceCreate" dSpaceID
  (space dSpaceID))

(defcfun-rename-function "dQuadTreeSpaceCreate" dSpaceID
  (space dSpaceID)
  (center dVector3)
  (extents dVector3)
  (depth :int))


(defcfun-rename-function "dJointGroupEmpty" :void
  (jointGroup dJointGroupID))

(defcfun-rename-function "dJointAttach" :void
  (joint dJointID)
  (body1 dBodyID)
  (body2 dBodyID))

(defcfun-rename-function "dJointGroupCreate" dJointGroupID
  (max-size :int))

(defcfun-rename-function "dJointGroupDestroy" :void
  (joint-group dJointGroupID))

(defcfun-rename-function "dMassSetBox" :void
  (mass (:pointer dMass))
  (m dReal)
  (lx dReal)
  (ly dReal)
  (lz dReal))


(defcfun-rename-function "dMassSetBoxTotal" :void
  (mass (:pointer dMass))
  (m dReal)
  (lx dReal)
  (ly dReal)
  (lz dReal))


(defcfun-rename-function "dMassSetZero" :void
  (mass (:pointer dMass)))

(defcfun-rename-function "dRFromAxisAndAngle" :void
  (R dMatrix3)
  (ax dReal)
  (ay dReal)
  (az dReal)
  (angle dReal))

(defcfun-rename-function "dSpaceCollide" :void
  (space dSpaceID)
  (data :pointer)
  (callback :pointer))

(defcfun-rename-function "dWorldCreate" dWorldID) 

(defcfun-rename-function "dWorldDestroy" :void
  (world dWorldID))

(defcfun-rename-function "dWorldSetGravity" :void
  (world dWorldID)
  (x dReal)
  (y dReal)
  (z dReal))

(defcfun-rename-function "dWorldStep" :void
  (world dWorldID)
  (step_size dReal))

(defcfun-rename-function "dBodySetMovedCallback" :void
  (b dBodyID)
  (callback :pointer))


(defcfun-rename-function "dCreateSphere" dGeomID
  (space dSpaceID)
  (radius dReal))

(defcfun-rename-function "dMassSetSphere" :void
  (m (:pointer dMass))
  (density dReal)
  (radius dReal))

(defcfun-rename-function "dMassSetSphereTotal" :void
  (m (:pointer dMass))
  (total-mass dReal)
  (radius dReal))


(defcfun-rename-function "dCreateCylinder" dGeomID
  (space dSpaceID)
  (radius dReal)
  (length dReal))

(defcfun-rename-function "dMassSetCylinder" :void
  (m (:pointer dMass))
  (density dReal)
  (direction :int)
  (radius dReal)
  (length dReal))

(defcfun-rename-function "dMassSetCylinderTotal" :void
  (m (:pointer dMass))
  (density dReal)
  (direction :int)
  (radius dReal)
  (length dReal))

(defcfun-rename-function "dCreateCapsule" dGeomID
  (space dSpaceID)
  (radius dReal)
  (length dReal))

(defcfun-rename-function "dMassSetCapsule" :void
  (m (:pointer dMass))
  (density dReal)
  (direction :int)
  (radius dReal)
  (length dReal))

(defcfun-rename-function "dMassSetCapsuleTotal" :void
  (m (:pointer dMass))
  (density dReal)
  (direction :int)
  (radius dReal)
  (length dReal))


(defcfun-rename-function "dCreateRay" dGeomID
  (space dSpaceID)
  (length dReal))

(defcfun-rename-function "dGeomRaySetLength" :void 
  (ray dGeomID)
  (length dReal))

(defcfun-rename-function "dGeomRayGetLength" dReal
  (ray dGeomID))

(defcfun-rename-function "dGeomRaySet" :void 
  (ray dGeomID)
  (px dReal)
  (py dReal)
  (pz dReal)
  (dx dReal)
  (dy dReal) 
  (dz dReal))

(defcfun-rename-function "dGeomRayGet" :void
  (ray dGeomID)
  (start :pointer)
  (dir :pointer))


(defcfun-rename-function "dGeomRaySetParams" :void 
  (ray dGeomID)
  (first-contact :int) 
  (backface-cull :int))

;; (defcfun-rename-function "dGeomRayGetParams" :void 
;;   (ray dGeomID)
;;   *FirstContact, int *BackfaceCull );


(defcfun-rename-function "dGeomRaySetClosestHit" :void
  (ray dGeomID)
  (ClosestHit :int))

(defcfun-rename-function "dGeomRayGetClosestHit" :int
  (ray dGeomID))

(defcfun-rename-function "dSpaceDestroy" :void
  (space dSpaceID))

(defcfun-rename-function "dWorldSetCFM" :void
  (world dWorldID)
  (cfm dReal))

(defcfun-rename-function "dWorldSetERP" :void
  (world dWorldID)
  (erp dReal))


(defcfun-rename-function "dWorldQuickStep" :void
  (w dWorldID)
  (stepsize dReal))

(defcfun-rename-function "dGeomGetBody" dBodyID
  (geom dGeomID))

(defcfun-rename-function "dCollide" :int
  (o1 dGeomID)
  (o2 dGeomID)
  (flags :int)
  (contact (:pointer dContactGeom))
  (skip :int))
	   
(defcfun-rename-function "dJointCreateContact" dJointID
  (world dWorldID)
  (joint-group dJointGroupID)
  (contact (:pointer (:struct dContact))))

(defcfun-rename-function "dJointGetType" :int
  (id dJointID))

(defcfun-rename-function "dWorldSetLinearDamping" :void
  (world dWorldID)
  (scale dReal))

(defcfun-rename-function "dWorldSetAngularDamping" :void
  (world dWorldID)
  (scale dReal))

(defcfun-rename-function "dWorldSetDamping" :void
  (world dWorldID)
  (linear-scale dReal)
  (angular-scale dReal))
 
(defcfun-rename-function "dWorldGetLinearDampingThreshold" dReal
  (world dWorldID))

(defcfun-rename-function "dWorldGetAngularDampingThreshold" dReal
  (world dWorldID))

(defcfun-rename-function "dWorldSetLinearDampingThreshold" :void
  (world dWorldID)
  (threshold dReal))

(defcfun-rename-function "dWorldSetAngularDampingThreshold" :void
  (world dWorldID) 
  (threshold dReal))

(defcfun-rename-function "dWorldSetAutoDisableFlag" :void
  (world dWorldID)
  (auto-disable :int))

(defcfun-rename-function "dWorldGetAutoDisableFlag" :int
  (world dWorldID))
