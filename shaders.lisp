;;;; shaders.lisp
;;;; Please see the licence.txt for the CLinch

(in-package #:clinch)

(defclass shader ()
  ((name :reader name
	 :initform nil)
   (id :initform nil
       :initarg :id
       :reader id)
   (shader-type :initform nil
		:initarg :shader-type
		:reader shader-type)
   (attributes :reader shader-attributes
	       :initform nil)
   (uniforms :reader shader-uniforms
	     :initform nil)
   (key :initform (gensym "shader")
	:reader key))
  (:documentation "Base class for all individual shaders."))

(defclass vertex-shader (shader)
  ((shader-type :initform :vertex-shader))
  (:documentation "Vertex Shader Class."))

(defclass fragment-shader (shader)
  ((shader-type :initform :fragment-shader))
  (:documentation "Fragment Shader Class."))

(defclass geometry-shader (shader)
  ((shader-type :initform :geometry-shader))
  (:documentation "Geometry Shader Class."))


(defmethod initialize-instance :after ((this shader) &key code defs undefs)
  (with-slots ((id id) (key key)) this 

    (shader-compile this :code code :defs defs :undefs undefs)

    (trivial-garbage:cancel-finalization this)
    (setf (gethash (key this) *uncollected*) this)
    (trivial-garbage:finalize
     this
     (let ((val id)
	   (key (key this)))
       
       (lambda ()
	 (remhash key *uncollected*)
	 (sdl2:in-main-thread (:background t) 
	   (gl:delete-shader val)))))))
      

(defmethod shader-compile ((this shader) &key code defs undefs)

  (when (null code) (error "No code to build shader!"))

  (with-slots ((id id)) this

    (unless id
      (create-shader this))

    (gl:shader-source id (concatenate 'string
				      (format nil "~{#define ~A~%~}" defs)
				      (format nil "~{#undef ~A~%~}" undefs)
				      code))
    (gl:compile-shader id)
    
    (let ((log (gl:get-shader-info-log id)))
      (unless (string-equal log "")
	(format t "Shader Log: ~A~%" log)))
    
    (unless (gl:get-shader id :compile-status)
      (error "Could not compile shader!"))))

(defmethod create-shader ((this vertex-shader))
  (setf (slot-value this 'id)
	(gl:create-shader :vertex-shader)))

(defmethod create-shader ((this fragment-shader))
  (setf (slot-value this 'id)
	(gl:create-shader :fragment-shader)))

(defmethod create-shader ((this geometry-shader))
  (setf (slot-value this 'id)
	(gl:create-shader :geometry-shader)))
  
(defmethod unload ((this shader) &key)
  "Unloads and releases the shader."

  (with-slots ((id id))
    
    (trivial-garbage:cancel-finalization this)
    (remhash (key this) *uncollected*)

    (gl:delete-shader id)

    (setf id nil
	  (slot-value this 'uniforms) nil
	  (slot-value this 'attributes) nil
	  (slot-value this 'name) nil)))


 
