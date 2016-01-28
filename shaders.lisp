;;;; shaders.lisp
;;;; Please see the licence.txt for the CLinch

(in-package #:clinch)

(defclass shader ()
  ((name :reader name
	 :initform nil)
   (id :initform nil
       :initarg :id
       :reader id)
   (attributes :reader shader-attributes
	       :initform nil)
   (uniforms :reader shader-uniforms
	     :initform nil)
   (key :initform (gensym "shader")
	:reader key))
  (:documentation "Base class for all individual shaders."))

(defclass vertex-shader (shader) ()
  (:documentation "Vertex Shader Class."))

(defclass fragment-shader (shader) ()
  (:documentation "Fragment Shader Class."))

(defclass geometry-shader (shader) ()
  (:documentation "Geometry Shader Class."))


(defmethod initialize-instance ((this shader) &key code defs undefs)
  (with-slots ((id) (key)) this 

    (shader-compile this :code code :defs defs :undefs undefs)

    (setf code nil
	  defs nil
	  undefs nil)

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
  
  (gl:shader-source id (concatenate 'string
				    (format nil "~{#define ~A~%~}" defs)
				    (format nil "~{#undef ~A~%~}" undefs)
				    code))
  (gl:compile-shader id)
  
  (let ((log (gl:get-shader-info-log id)))
    (unless (string-equal log "")
      (format t "Shader Log: ~A~%" log)))
  
  (unless (gl:get-shader id :compile-status)
    (error "Could not compile shader!")))
  
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


;; (!
;; 	   (loop for i in (gl:get-attached-shaders (program *texture-shader*))
;; 	      collect (list (cffi:foreign-enum-keyword '%gl::enum 
;; 						       (gl:get-shader i :shader-type))
;; 			    (gl:get-shader-source i))))
