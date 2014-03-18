;;;; shaders.lisp
;;;; Please see the licence.txt for the CLinch

(in-package #:clinch)

(defclass shader ()
  ((name
    :reader name
    :initform nil)
   (program
    :reader program
    :initform nil)
   (frag-shader
    :reader frag-shader
    :initform nil)
   (vert-shader
    :reader vert-shader
    :initform nil)
   (geo-shader
    :reader geo-shader
    :initform nil)
   (attributes
    :reader shader-attribute 
    :initform nil)
  (uniforms
    :reader shader-uniform
    :initform nil))
   
  (:documentation "Creates and keeps track of the shader objects. Requires an UNLOAD call when you are done. Bind Buffer functions are in Buffer.l"))


(defmethod initialize-instance :after ((this shader) &key
				       name
				       vertex-shader-text
				       fragment-shader-text
				       geometry-shader-text
				       attributes
				       uniforms
				       defines
				       undefs)
  "Create the shader. Currently there is no way to change the shader. You must make a new one."

  (with-slots ((vs vert-shader)
  	       (fs frag-shader)
	       (geo geo-shader)
  	       (program program)) this
    (setf vs (gl:create-shader :vertex-shader))
    (setf fs (gl:create-shader :fragment-shader))    

    (gl:shader-source vs (concatenate 'string
				      (format nil "~{#define ~A~%~}" defines)
				      (format nil "~{#undef ~A~%~}" undefs)
				      vertex-shader-text))
    (print "Compiling vertex shader...")
    (gl:compile-shader vs)
    (print (gl:get-shader-info-log vs))
    (unless (gl:get-shader vs :compile-status)
      (error "Could not compile vertex shader!"))


    (gl:shader-source fs   (concatenate 'string
					(format nil "~{#define ~A~%~}" defines)
					(format nil "~{#undef ~A~%~}" undefs)
					fragment-shader-text))
    (print "Compiling fragment shader...")
    (gl:compile-shader fs)
    (print (gl:get-shader-info-log fs))
    (unless (gl:get-shader fs :compile-status)
      (error "Could not compile fragment shader!"))

    (when geometry-shader-text
      (setf geo (gl:create-shader :geometry-shader))
      (gl:shader-source geo  (concatenate 'string
					  (format nil "~{#define ~A~%~}" defines)
					  (format nil "~{#undef ~A~%~}" undefs)
					  geometry-shader-text))
      (print "Compiling geometry shader...")
      (gl:compile-shader geo)
      (print (gl:get-shader-info-log geo))
      (unless (gl:get-shader geo :compile-status)
	(error "Could not compile geometry shader!")))



    (setf program (gl:create-program))
    ;; You can attach the same shader to multiple different programs.
    (gl:attach-shader program vs)
    (gl:attach-shader program fs)

    (when geo
      (gl:attach-shader program geo))
    ;; Don't forget to link the program after attaching the
    ;; shaders. This step actually puts the attached shader together
    ;; to form the program.
    (gl:link-program program)

    (when attributes
      (setf (slot-value this 'attributes) (make-hash-table :test 'equal))
      (loop for (name type) in attributes
	   do (setf (gethash name (slot-value this 'attributes))
		    (cons type
			  (gl:get-attrib-location program name)))))
      

    (when uniforms
      (setf (slot-value this 'uniforms) (make-hash-table :test 'equal))
      (loop for (name type) in uniforms
	 do (setf (gethash name (slot-value this 'uniforms))
		  (cons type
			(gl:Get-Uniform-Location program name)))))
      

    (when name (setf (slot-value this 'name) name))

    (let ((v fs)
	  (f fs)
	  (g geo)
	  (p program))


    (trivial-garbage:finalize this
			      (lambda () (when p
					   (when v
					     (gl:detach-shader p v)
					     (gl:delete-shader v))
					   
					   (when f
					     (gl:delete-shader f)
					     (gl:detach-shader p f))
					   
					   (when g
					     (gl:detach-shader p g)
					     (gl:delete-shader g))
					   
					   (gl:delete-program p)))))))



(defmethod use-shader ((this shader) &key)
  "Start using the shader."
  (gl:use-program (program this)))

(defmethod get-uniform-id ((this shader) uniform)
  "Shaders pass information by using named values called Uniforms and Attributes. This gets the gl id of a uniform name."
  (gethash uniform
	 (slot-value this 'uniforms)))

(defmethod get-attribute-id ((this shader) attribute)
  "Shaders pass information by using named values called Uniforms and Attributes. This gets the gl id of a attribute name."
  (gethash attribute
	   (slot-value this 'attributes)))


(defmethod attach-uniform ((this shader) (uniform string) value)
  "Shaders pass information by using named values called Uniforms and Attributes. This sets a uniform to value."

  (destructuring-bind (type . id) (get-uniform-id this uniform)
    
    (let ((f (case type
	       (:float #'gl:uniformf)
	       (:int #'gl:uniformi)
	       (:matrix (lambda (id value)
			  (gl:uniform-matrix id 2 (cond
						    ((arrayp value) value)
						    ((typep value 'node) (transform
									  value))
						    (t (error "Unknown Type in attach-uniform!")))))))))
						      
      
      (if (listp value)
	  (apply f id value)
	  (apply f id (list value))))))

(defmethod attach-uniform ((this shader) (uniform string) (matrix array))
  "Shaders pass information by using named values called Uniforms and Attributes. This sets a uniform to value."

  (destructuring-bind (type . id) (get-uniform-id this uniform)
    (gl:uniform-matrix id 4 matrix)))

(defmethod attach-uniform ((this shader) (uniform string) (matrix node))
  "Shaders pass information by using named values called Uniforms and Attributes. This sets a uniform to value."

  (destructuring-bind (type . id) (get-uniform-id this uniform)
    (gl:uniform-matrix id 4 (transform matrix))))



(defmethod bind-static-values-to-attribute ((this shader) name &rest vals)
  "It is possible to bind static information to an attribute. Your milage may vary."
  (let ((id (cdr (get-attribute-id this name))))
    (gl:disable-vertex-attrib-array id)
    (apply #'gl:vertex-attrib id vals)))


(defmethod unload ((this shader) &key)
  "Unloads and releases all shader resources."
  (trivial-garbage:cancel-finalization this)
  
  (with-slots ((vs vert-shader)
	       (fs frag-shader)
	       (geo geo-shader)
	       (program program)) this

    (when program

      (when vs
	(gl:detach-shader program vs)
	(gl:delete-shader vs))
      
      (when fs
	(gl:delete-shader fs)
	(gl:detach-shader program fs))

      (when geo
	(gl:detach-shader program geo)
	(gl:delete-shader geo))

      (gl:delete-program program))
    
    (setf (slot-value this 'uniforms) nil
	  (slot-value this 'attributes) nil
	  (slot-value this 'name) nil
	  (slot-value this 'attributes) nil
	  (slot-value this 'uniforms) nil)))


(defmethod (setf shader-attribute) (value (this shader) key)

  (setf (gethash key (slot-value this 'shader-attribute)) value))

(defmethod (setf shader-uniform) (value (this shader) key)

  (setf (gethash key (slot-value this 'shader-uniform)) value))


(defmacro gl-shader (&body rest)

  `(make-instance 'shader ,@rest))

