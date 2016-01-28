;;;; shader-program.lisp
;;;; Please see the licence.txt for the CLinch

(in-package #:clinch)

(defclass shader-program ()
  ((name
    :reader name
    :initform nil)
   (program
    :reader program
    :initform nil)
   (frag-shader
    :reader frag-shader
    :initform nil
    :initarg :frag)
   (vert-shader
    :reader vert-shader
    :initform nil
    :initarg :vert)
   (geo-shader
    :reader geo-shader
    :initform nil
    :initarg :geo)
   (attributes
    :reader shader-program-attributes
    :initform nil)
   (uniforms
    :reader shader-program-uniforms
    :initform nil)
   (key :initform (gensym "shader-program")
	:reader key))
   
  (:documentation "Creates and keeps track of the shader-program objects. Requires an UNLOAD call when you are done. Bind Buffer functions are in Buffer.l"))

(defmethod initialize-instance :after ((this shader-program) &key
				       name
				       vertex-shader-text
				       fragment-shader-text
				       geometry-shader-text
				       attributes
				       uniforms
				       defines
				       undefs)
  "Create the shader program. Currently there is no way to change the shader. You must make a new one."

  (build-shader-program this :name name
		:vertex-shader-text vertex-shader-text
		:fragment-shader-text fragment-shader-text
		:geometry-shader-text geometry-shader-text
		:attributes attributes
		:uniforms uniforms
		:defines defines
		:undefs undefs))


(defmethod build-shader-program ((this shader-program) &key
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

    ;; Vertex and Fragment shaders are always required.
    (setf vs (or vs (gl:create-shader :vertex-shader)))
    (setf fs (or fs (gl:create-shader :fragment-shader)))

    ;; if there is already a program...unload it, but keep the number.
    (if program
	(gl:delete-program program)
	(setf program (gl:create-program))

    (gl:shader-source vs (concatenate 'string
				      (format nil "~{#define ~A~%~}" defines)
				      (format nil "~{#undef ~A~%~}" undefs)
				      vertex-shader-text))


    
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
  

(defmethod use-shader-program ((this shader-program) &key)
  "Start using the shader-program."
  (gl:use-program (program this)))

(defmethod get-uniform-id ((this shader-program) (id integer))
  "Shaders pass information by using named values called Uniforms and Attributes. If we are using the raw id, this returns it."
  (when (and id (>= (cdr id) 0)) id))

(defmethod get-uniform-id ((this shader-program) (uniform string))
  "Shaders pass information by using named values called Uniforms and Attributes. This gets the gl id of a uniform name."
  (let ((id (gethash uniform
		     (slot-value this 'uniforms))))
    (when (and id (>= (cdr id) 0)) id)))

(defmethod get-attribute-id ((this shader-program) (id integer))
  "Shaders pass information by using named values called Uniforms and Attributes. If we are using the raw id, this returns it."
  (when (and id
	     (>= (cdr id) 0))
    id))

(defmethod get-attribute-id ((this shader-program) (attribute string))
  "Shaders pass information by using named values called Uniforms and Attributes. This gets the gl id of a attribute name."
  (let ((id (gethash attribute
		     (slot-value this 'attributes))))
    (when (and id
	       (>= (cdr id) 0))
	       id)))


(defmethod attach-uniform ((this shader-program) (uniform string) value)
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
    
(defmethod attach-uniform ((this shader-program) (uniform string) (matrix array))
  "Shaders pass information by using named values called Uniforms and Attributes. This sets a uniform to a matrix value."

  (let ((ret (get-uniform-id this uniform)))
    (when ret 
      ;;(unless (eq (gethash ret *current-shader-uniforms*) ret)
	(setf (gethash ret *current-shader-uniforms*) ret)
	(destructuring-bind (type . id) ret
	  
	  (gl::with-foreign-matrix (foreign-matrix matrix)
	    (%gl:uniform-matrix-4fv id 1 nil foreign-matrix))))))
    
(defmethod attach-uniform ((this shader-program) (uniform string) (matrix node))
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

(defmethod bind-static-values-to-attribute ((this shader-program) name &rest vals)
  "It is possible to bind static information to an attribute. Your milage may vary."
  (let ((id (cdr (get-attribute-id this name))))
    (when id 
      (gl:disable-vertex-attrib-array id)
      (apply #'gl:vertex-attrib id vals))))


(defmethod unload ((this shader-program) &key)
  "Unloads and releases all shader-program resources."

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


(defmethod shader-program-attribute ((this shader-program) key)
  "Gets a shader-program attribute"
  (gethash key (slot-value this 'shader-program-program-attribute)))

(defmethod (setf shader-program-attribute) (value (this shader-program) key)
  "Sets a shader-program attribute."
  (setf (gethash key (slot-value this 'shader-program-attribute)) value))

(defmethod remove-shader-program-attribute ((this shader-program) key)
  "Removes a shader-program attribute"
  (remhash key (slot-value this 'shader-program-attribute)))

(defmethod shader-program-uniform ((this shader-program) key)
  "Gets a shader-program uniform"
  (gethash key (slot-value this 'shader-program-uniform)))

(defmethod (setf shader-program-uniform) (value (this shader-program) key)
  "Sets a shader-program uniform."
  (setf (gethash key (slot-value this 'shader-program-uniform)) value))

(defmethod remove-shader-program-uniform ((this shader-program) key)
  "Removes a shader-program uniform"
  (remhash key (slot-value this 'shader-program-uniform)))

