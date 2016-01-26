;;;; shader.lisp
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
    :initform nil)
   (key :initform (gensym "shader")
	:reader key))
   
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

  (build-shader this :name name
		:vertex-shader-text vertex-shader-text
		:fragment-shader-text fragment-shader-text
		:geometry-shader-text geometry-shader-text
		:attributes attributes
		:uniforms uniforms
		:defines defines
		:undefs undefs))


(defmethod build-shader ((this shader) &key
					 name
					 vertex-shader-text
					 fragment-shader-text
					 geometry-shader-text
					 attributes
					 uniforms
					 defines
					 undefs)

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

    (if program (unload this))
    
    (gl:compile-shader vs)

    (let ((log (gl:get-shader-info-log vs)))
      (unless (string-equal log "")
	(format t "Shader Log: ~A~%" log)))
    
    (unless (gl:get-shader vs :compile-status)
      (error "Could not compile vertex shader!"))

    (gl:shader-source fs   (concatenate 'string
					(format nil "~{#define ~A~%~}" defines)
					(format nil "~{#undef ~A~%~}" undefs)
					fragment-shader-text))

    (gl:compile-shader fs)
    
    (let ((log (gl:get-shader-info-log fs)))
      (unless (string-equal log "")
	(format t "Shader Log: ~A~%" log)))


    (unless (gl:get-shader fs :compile-status)
      (error "Could not compile fragment shader!"))

    (when geometry-shader-text
      (setf geo (gl:create-shader :geometry-shader))
      (gl:shader-source geo  (concatenate 'string
					  (format nil "~{#define ~A~%~}" defines)
					  (format nil "~{#undef ~A~%~}" undefs)
					  geometry-shader-text))

      (gl:compile-shader geo)
      
      (let ((log (gl:get-shader-info-log geo)))
	(unless (string-equal log "")
	  (format t "Shader Log: ~A~%" log)))

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
    
    (setf (slot-value this 'uniforms) (make-hash-table :test 'equal))
    (setf (slot-value this 'attributes) (make-hash-table :test 'equal))

    (trivial-garbage:cancel-finalization this)
    (setf (gethash (key this) *uncollected*) this)
    (trivial-garbage:finalize this
			      (let ((program-val program)
				    (fs-val fs)
				    (vs-val vs)
				    (geo-val geo)
				    (key (key this)))
				
				(lambda ()
				  (remhash key *uncollected*)
				  (sdl2:in-main-thread (:background t) 
				    (gl:detach-shader program-val fs-val)
				    (gl:delete-shader fs-val)
				    (gl:detach-shader program-val vs-val)
				    (gl:delete-shader vs-val)
				    (when geo-val 
				      (gl:detach-shader program-val geo-val)
				      (gl:delete-shader geo-val))
				    (gl:delete-program program-val)))))

    (when attributes
      (loop for (name type) in attributes
	 for location = (gl:get-attrib-location program name)
	 if (>= location 0)
	 do (setf (gethash name (slot-value this 'attributes))
		  (cons type location))
	   else do (format t "could not find attribute ~A!~%" name)))
	         

    (when uniforms
      (loop for (name type) in uniforms
	 for location = (gl:Get-Uniform-Location program name)
	 if (>= location 0)
	 do (setf (gethash name (slot-value this 'uniforms))
		  (cons type location))
	 else do (format t "could not find uniform ~A!~%" name)))
			

    (when name (setf (slot-value this 'name) name))))
  

(defmethod use-shader ((this shader) &key)
  "Start using the shader."
  (gl:use-program (program this)))

(defmethod get-uniform-id ((this shader) (id integer))
  "Shaders pass information by using named values called Uniforms and Attributes. If we are using the raw id, this returns it."
  (when (and id (>= (cdr id) 0)) id))

(defmethod get-uniform-id ((this shader) (uniform string))
  "Shaders pass information by using named values called Uniforms and Attributes. This gets the gl id of a uniform name."
  (let ((id (gethash uniform
		     (slot-value this 'uniforms))))
    (when (and id (>= (cdr id) 0)) id)))

(defmethod get-attribute-id ((this shader) (id integer))
  "Shaders pass information by using named values called Uniforms and Attributes. If we are using the raw id, this returns it."
  (when (and id
	     (>= (cdr id) 0))
    id))

(defmethod get-attribute-id ((this shader) (attribute string))
  "Shaders pass information by using named values called Uniforms and Attributes. This gets the gl id of a attribute name."
  (let ((id (gethash attribute
		     (slot-value this 'attributes))))
    (when (and id
	       (>= (cdr id) 0))
	       id)))


(defmethod attach-uniform ((this shader) (uniform string) value)
  "Shaders pass information by using named values called Uniforms and Attributes. This sets a uniform to value."
  (let ((ret (get-uniform-id this uniform)))

    (when ret
      ;;(unless (eq (gethash ret *current-shader-uniforms*) value)
	(setf (gethash ret *current-shader-uniforms*) value)
	
	(destructuring-bind (type . id) ret
	  
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
		(apply f id (list value))))))))
    
(defmethod attach-uniform ((this shader) (uniform string) (matrix array))
  "Shaders pass information by using named values called Uniforms and Attributes. This sets a uniform to a matrix value."

  (let ((ret (get-uniform-id this uniform)))
    (when ret 
      ;;(unless (eq (gethash ret *current-shader-uniforms*) ret)
	(setf (gethash ret *current-shader-uniforms*) ret)
	(destructuring-bind (type . id) ret
	  
	  (gl::with-foreign-matrix (foreign-matrix matrix)
	    (%gl:uniform-matrix-4fv id 1 nil foreign-matrix))))))
    
(defmethod attach-uniform ((this shader) (uniform string) (matrix node))
  "Shaders pass information by using named values called Uniforms and Attributes. This sets a uniform to the matrix of a node."

  (let ((ret (get-uniform-id this uniform)))
    (when ret 
      ;;(unless (eq (gethash ret *current-shader-uniforms*) matrix)
	(setf (gethash ret *current-shader-uniforms*) matrix)
	
	(let ((ret (get-uniform-id this uniform)))
	  (when ret 
	    (destructuring-bind (type . id) ret
	      
	      (gl::with-foreign-matrix (foreign-matrix (transform matrix))
		(%gl:uniform-matrix-4fv id 1 nil foreign-matrix))))))))

(defmethod bind-static-values-to-attribute ((this shader) name &rest vals)
  "It is possible to bind static information to an attribute. Your milage may vary."
  (let ((id (cdr (get-attribute-id this name))))
    (when id 
      (gl:disable-vertex-attrib-array id)
      (apply #'gl:vertex-attrib id vals))))


(defmethod unload ((this shader) &key)
  "Unloads and releases all shader resources."

  (with-slots ((vs vert-shader)
	       (fs frag-shader)
	       (geo geo-shader)
	       (program program)) this
    
    (trivial-garbage:cancel-finalization this)
    (remhash (key this) *uncollected*)

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

      (gl:delete-program program)
      (setf program nil))
    
    (setf (slot-value this 'uniforms) nil
	  (slot-value this 'attributes) nil
	  (slot-value this 'name) nil
	  (slot-value this 'attributes) nil
	  (slot-value this 'uniforms) nil)))


(defmethod shader-attribute ((this shader) key)
  "Gets a shader attribute"
  (gethash key (slot-value this 'shader-attribute)))

(defmethod (setf shader-attribute) (value (this shader) key)
  "Sets a shader attribute."
  (setf (gethash key (slot-value this 'shader-attribute)) value))

(defmethod remove-shader-attribute ((this shader) key)
  "Removes a shader attribute"
  (remhash key (slot-value this 'shader-attribute)))

(defmethod shader-uniform ((this shader) key)
  "Gets a shader uniform"
  (gethash key (slot-value this 'shader-uniform)))

(defmethod (setf shader-uniform) (value (this shader) key)
  "Sets a shader uniform."
  (setf (gethash key (slot-value this 'shader-uniform)) value))

(defmethod remove-shader-uniform ((this shader) key)
  "Removes a shader uniform"
  (remhash key (slot-value this 'shader-uniform)))

(defmacro gl-shader (&body rest)

  `(make-instance 'shader ,@rest))

