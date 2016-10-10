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
  "Creates a shader."
  (when code 
    (!
      (with-slots ((id id) (key key)) this 
	(shader-compile this :code code :defs defs :undefs undefs)))))


(defmethod add-finalizer ((this shader))
  (trivial-garbage:cancel-finalization this)
  (add-uncollected this)
  (trivial-garbage:finalize
   this
   (let ((val (id this))
	 (key (key this)))
     
     (lambda ()
       (remhash key *uncollected*)
       (sdl2:in-main-thread (:background t) 
	 (gl:delete-shader val))))))


(defmethod shader-compile ((this shader) &key code defs undefs)

  (when (null code) (error "No code to build shader!"))

  (! 
    (let ((old-id (id this)))
      
      (setf (slot-value this 'id) (make-shader this))
      
      (when old-id
	(gl:delete-shader old-id)))
          
    (gl:shader-source (id this) (concatenate 'string
					(format nil "~{#define ~A~%~}" defs)
					(format nil "~{#undef ~A~%~}" undefs)
					code))
    (gl:compile-shader (id this)))
  
  (unless (> (id this) 0)
    (error "Could not create a shader object!"))
  
  (let ((err (gl:get-shader (id this) :compile-status)))
    (unless err
      (error (format nil "Could not compile shader: ~A" (gl:get-shader-info-log (id this))))))
    
  (add-finalizer this))

(defmethod make-shader ((this vertex-shader))
  "Creates a vertex shader."
  (setf (slot-value this 'id)
	(gl:create-shader :vertex-shader)))

(defmethod make-shader ((this fragment-shader))
  "Creates a fragment shader."
  (setf (slot-value this 'id)
	(gl:create-shader :fragment-shader)))

(defmethod make-shader ((this geometry-shader))
  "Creates a geometry shader."
  (setf (slot-value this 'id)
	(gl:create-shader :geometry-shader)))

(defmethod shader-source ((this shader))
  (with-slots ((id id)) this
    (!
      (list (cffi:foreign-enum-keyword '%gl::enum 
				       (gl:get-shader id :shader-type))
	    (! (gl:get-shader-source id))))))

(defmethod (setf shader-source) ((code string) (this shader))
  (shader-compile this :code code))

(defmethod unload ((this shader) &key)
  "Unloads and releases the shader."

  (with-slots ((id id))
      
      (trivial-garbage:cancel-finalization this)
    (remove-uncollected this)
    
    (when id
      (gl:delete-shader id))

    (setf id nil
	  (slot-value this 'uniforms) nil
	  (slot-value this 'attributes) nil
	  (slot-value this 'name) nil)))
