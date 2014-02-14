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
    :reader attributes 
    :initform nil)
   (uniforms
    :reader uniforms 
    :initform nil))
  (:documentation "Creates and keeps track of the shader objects. Requires an UNLOAD call when you are done. Bind Buffer functions are in Buffer.l"))


(defmethod initialize-instance :after ((this shader) &key
				       name
				       vertex-shader-text
				       fragment-shader-text
				       geometry-shader-text
				       keep-fragment-shader?
				       keep-vertex-shader?
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
      (gl:shader-source geo  (concatenate 'string
					  (format nil "~{#define ~A~%~}" defines)
					  (format nil "~{#undef ~A~%~}" undefs)
					  geometry-shader-text))
      (print "Compiling geometry shader...")
      (gl:compile-shader geo)
      (print (gl:get-shader-info-log geo))
      (unless (gl:get-shader geo :compile-status)
	(error "Could not compile fragment shader!")))



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

    (setf (slot-value this 'attributes)
    	  (loop for (name type) in attributes
    	     collect (cons
    		      name
		      (cons type
			    (gl:get-attrib-location program name)))))

    (setf (slot-value this 'uniforms)
    	  (loop for (name type) in uniforms
	     do (print (list name type))
	       
    	     collect (cons
    		      name
		      (cons type
			    (gl:Get-Uniform-Location program name)))))

    
    (when name (setf (slot-value this 'name) name))
    ))



(defmethod use-shader ((this shader) &key)
  "Start using the shader."
  (gl:use-program (program this)))

(defmethod get-uniform-id ((this shader) uniform)
  "Shaders pass information by using named values called Uniforms and Attributes. This gets the gl id of a uniform name."
  (assoc uniform
	 (uniforms this)
	 :test #'equal))

(defmethod get-attribute-id ((this shader) attribute)
  "Shaders pass information by using named values called Uniforms and Attributes. This gets the gl id of a attribute name."
  (assoc attribute
	 (attributes this)
	 :test #'equal))


(defmethod attach-uniform ((this shader) (uniform string) value)
  "Shaders pass information by using named values called Uniforms and Attributes. This sets a uniform to value."
  (destructuring-bind (type . id) (cdr (get-uniform-id this uniform))
    
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
  (destructuring-bind (name type . id) (get-uniform-id this uniform)
    (gl:uniform-matrix id 4 matrix)))

(defmethod attach-uniform ((this shader) (uniform string) (matrix node))
  "Shaders pass information by using named values called Uniforms and Attributes. This sets a uniform to value."
  (destructuring-bind (name type . id) (get-uniform-id this uniform)
    (gl:uniform-matrix id 4 (transform matrix))))



(defmethod bind-static-values-to-attribute ((this shader) name &rest vals)
  "It is possible to bind static information to an attribute. Your milage may vary."
  (let ((id (cddr (get-attribute-id this name))))
    (gl:disable-vertex-attrib-array id)
    (apply #'gl:vertex-attrib id vals)))


(defmethod unload ((this shader) &key)
  "Unloads and releases all shader resources."
  (with-slots ((vs vert-shader)
	       (fs frag-shader)
	       (geo geo-shader)
	       (program program)) this

    (when program
      (gl:detach-shader program vs)
      (gl:detach-shader program fs))

    (when vs (gl:delete-shader vs))
    (when fs (gl:delete-shader fs))
    (when geo (gl:delete-shader geo))

    (when program (gl:delete-program program))

    (setf (slot-value this 'uniforms) nil
	  (slot-value this 'attributes) nil
	  (slot-value this 'name) nil)))



