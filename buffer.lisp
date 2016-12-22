;;;; buffer.lisp
;;;; Please see the licence.txt for the CLinch 

(in-package #:clinch)

(defun cffi-type->gl-type (type)
  "Returns the buffer type as a cffi type."
  (case type
    (:unsigned-char :unsigned-byte)
    (otherwise type)))

(defclass buffer ()
  ((id
    :reader id
    :initform nil
    :initarg :id)
   (type
    :accessor qtype
    :initarg :qtype
    :initform :float)
   (usage
    :accessor usage
    :initform :static-draw
    :initarg :usage)
   (stride
    :reader Stride
    :initform 3
    :initarg :stride)
   (Vertex-Count
    :reader Vertex-Count
    :initform nil
    :initarg :count)
   (target
    :reader target
    :initform :array-buffer
    :initarg :target)
   (loaded
    :accessor loaded?
    :initform nil)
   (key :initform (gensym "buffer")
	:reader key))
  (:documentation "Creates and keeps track of GPU buffer object (shared memory with gpu)."))

(defgeneric pullg (this &key))
(defgeneric pushg (this data &key))
(clone-function pullg !>)
(clone-function pushg !<)
    
(defmethod initialize-instance :after ((this buffer) &key data)
  "Sets up a buffer instance.
      type:   cffi type NOTE: Use the index buffer for indices.
      id:     OpenGL buffer id
      vcount: vertex count (or number of tuples if not using vertexes)
      stride: The number of items in each vertex (or each tuple.) NOTE: use 1 if you are creating an index buffer. 
      target: OpenGL buffer target. If you use this, think about subclassing. For more info lookup glBindBuffer().
              NOTE: use :element-array-buffer if you are creating an index buffer.
      usage:  Tells OpenGL how often you wish to access the buffer. 
      loaded: Has data been put into the buffer. Buffers without data is just future storage, just be sure to set it before you use it.
      data:   The data with which to fill the buffer. If data has a size, vcount does not need to be set."
  
  (with-slots ((type    type)
	       (id      id)
	       (vcount  vertex-count)
	       (target  target)
	       (stride  stride)
	       (usage   usage)
	       (loaded? loaded))  this

    (!
      ;; if they didn't give a vcount, see if we can derive one...
      (when (and (not vcount) (length data))
	(setf vcount (/ (length data) stride)))
      
      (unless id
	(setf id (car (gl:gen-buffers 1))))

      (trivial-garbage:cancel-finalization this)
      (add-uncollected this)
      
      (trivial-garbage:finalize this
				(let ((id-value id)
				      (key (key this)))
				  (lambda ()

				    (remhash key *uncollected*)
				    (sdl2:in-main-thread (:background t)
				      (gl:delete-buffers (list id-value))))))
      (gl:bind-buffer target id)

      (cond
	;; Raw CFFI data?
	((cffi:pointerp data)

	 (%gl:Buffer-Data target
			  (size-in-bytes this)
			  data
			  usage)
	 (setf loaded? t))
	
	;; Data in List?
	(data

	 (let ((p (cffi:foreign-alloc type :initial-contents data :count (get-size this))))
	   (unwind-protect
		(progn (%gl:Buffer-Data target
					(size-in-bytes this)
					p
					usage)
		       (setf loaded? t))
	     
	     (cffi:foreign-free p))))
	
	;; No Data?
	(t

	 (%gl:Buffer-Data target
			  (size-in-bytes this)
			  (cffi:null-pointer)
			  usage)
	 (setf loaded? nil))))))

(defmethod resize-buffer ((this buffer) size)
  ;; size in entries...a point would be 3 floats.
  (with-slots ((type    type)
	       (id      id)
	       (vcount  vertex-count)
	       (target  target)
	       (stride  stride)
	       (usage   usage)
	       (loaded? loaded))  this
    
    (when id
      (let ((new-id))

	(! (setf new-id (car (gl:gen-buffers 1)))
	   (when new-id
	     (let ((new-size  (* (cffi:foreign-type-size (qtype this)) size (stride this))))
	       
	       (gl:bind-buffer (target this) new-id)
	       (%gl:Buffer-Data target
				new-size
				(cffi:null-pointer)
				usage)
	       
	       (gl:bind-buffer :copy-write-buffer new-id)
	       (bind this :target :copy-read-buffer)
	       
	       ;; ;; 	   ;; need to min the lengths in bytes...
	       (%gl:copy-buffer-sub-data :copy-read-buffer :copy-write-buffer 0 0 (min (size-in-bytes this) new-size))
	      
	   (gl:delete-buffers (list (id this)))
	   (setf id new-id)
	   (setf vcount size)
	   (gl:bind-buffer target id))))
	   
	(trivial-garbage:cancel-finalization this)
	(remove-uncollected this)
	
	(trivial-garbage:finalize this
				  (let ((id-value id)
					(key (key this)))
				    (lambda ()
				      
				      (remhash key *uncollected*)
				      (sdl2:in-main-thread (:background t)
					(gl:delete-buffers (list id-value))))))))))


(defmethod bind ((this buffer) &key index offset length target)
  "Wrapper around glBindBuffer. Binds this buffer for use."
  (if (or offset length)
      (cl-opengl-bindings:bind-buffer-range  (or target (target this))
					     (or index 0)
					     (id this)
					     (or offset 0)
					     (or length (- (vertex-count this) (or offset 0))))
      (gl:bind-buffer (or target (target this)) (id this))))

(defmethod query-buffer-size ((this buffer) &key)
  (cffi:with-foreign-object (p :int) 
    (!
      (bind this)
      (%gl:get-buffer-parameter-iv :array-buffer :buffer-size p))
    (cffi:mem-aref p :int)))


(defmethod unbind ((this buffer) &key)
  "Wrapper around glBindBuffer with buffer 0, or no buffer."
  (gl:bind-buffer (target this) 0))

(defmethod get-size ((this buffer) &key)
  "Calculates the number of VALUES (stride + vcount) this buffer contains."
  (* (stride this) (vertex-count this)))


(defmethod size-in-bytes ((this buffer))
  "Calculates how many bytes this buffer consists of."
  (* 
   (get-size this)
   (cffi:foreign-type-size (slot-value this 'type))))


(defmethod bind-buffer-to-vertex-array ((this buffer) &key index offset length)
  "Use buffer in shader for the vertex array: The built-in variable gl_Vertex."
  (gl:Enable-Client-State :VERTEX-ARRAY)
  (bind this :index index :offset offset :length length)
  (%gl:vertex-pointer (stride this) (qtype this) 0 (cffi:null-pointer)))


(defmethod bind-buffer-to-normal-array ((this buffer) &key index offset length)
  "Use buffer in shader for the vertex array: The built-in variable gl_Vertex."
  (gl:Enable-Client-State :NORMAL-ARRAY)
  (bind this :index index :offset offset :length length)
  (%gl:normal-pointer (qtype this) 0 (cffi:null-pointer)))

(defmethod unbind-vertex-array ()
  "Stop using the vertex array"
  (gl:disable-Client-State :VERTEX-ARRAY))

(defmethod unbind-normal-array ()
  "Stop using the normal array"
  (gl:Disable-Client-State :NORMAL-ARRAY))

(defmethod bind-buffer-to-attribute-array ((this buffer) (shader shader-program) name &key index offset length)
  "Bind buffer to a shader attribute."

  (let ((id (cdr (get-attribute-id shader name))))
    (when id
      ;; (unless (eq (gethash id *current-shader-attributes*) name)
      ;; 	(setf (gethash id *current-shader-attributes*) name)

      ;;(print (qtype this))

      (gl:enable-vertex-attrib-array id)
      (bind this :index index :offset offset :length length)

      (if (eql (qtype this) :int)
	  (gl:vertex-attrib-ipointer id
				(stride this)
				(qtype this)
				0 (cffi:null-pointer))

	  (gl:vertex-attrib-pointer id
				    (stride this)
				    (qtype this)
				    0 0 (cffi:null-pointer))))))

(defmethod unbind-buffer-attribute-array ((this buffer) (shader-program shader-program) name)
  "Bind buffer to a shader-program attribute."

  (let ((id (cdr (get-attribute-id shader-program name))))
    (when id
      (remhash id *current-shader-attributes*)
      
      (gl:disable-vertex-attrib-array id))))

(defmethod map-buffer ((this buffer) &optional (access :READ-WRITE))
  "Returns a pointer to the buffer data. YOU MUST CALL UNMAP-BUFFER AFTER YOU ARE DONE!
   Access options are: :Read-Only, :Write-Only, and :READ-WRITE. NOTE: Using :read-write is slower than the others. If you can, use them instead."
  (!
    (gl:bind-buffer (target this) (id this))
    (gl:map-buffer (target this) access)))

(defmethod unmap-buffer ((this buffer))
  "Release the pointer given by map-buffer. NOTE: THIS TAKES THE BUFFER OBJECT, NOT THE POINTER! ALSO, DON'T TRY TO RELASE THE POINTER."
  (!
    (gl:bind-buffer (target this) (id this))
    (gl:unmap-buffer (target this))
    (gl:bind-buffer (target this) 0)))

(defmethod unload ((this buffer) &key)
  "Release buffer resources."
  (trivial-garbage:cancel-finalization this)
  (remove-uncollected this)
  (!
    (when (slot-value this 'id)
      (gl:delete-buffers (list (id this)))))
  (setf (slot-value this 'id) nil))

(defmacro with-mapped-buffer ((name buffer &optional (access :READ-WRITE)) &body body)
  "Convenience macro for mapping and unmapping the buffer data.
   Name is the symbol name to use for the buffer pointer."
  `(!
     (let ((,name (clinch::map-buffer ,buffer ,access)))
       (unwind-protect
	    (progn ,@body)
	 (clinch::unmap-buffer ,buffer)))))

(defmethod make-shareable-array ((this buffer) &key size)
  (case (qtype this)
    (:float (make-array (or size (get-size this)) :element-type 'single-float))
    (:unsigned-int (make-array (or size (get-size this)) :element-type `(unsigned-byte ,(* (cffi:FOREIGN-TYPE-size :unsigned-int) 8))))
    (t (cffi:make-shareable-byte-vector (or size (size-in-bytes this))))))

(defmethod pullg ((this buffer) &key index offset size)
  "Returns the buffer's data as a vector array."
  (let* ((full-length (size-in-bytes this))
	 (arr (make-shareable-array this :size size)))
    (cffi:with-pointer-to-vector-data (p arr)
      (!
	(bind this :index index :offset offset :length size)
	(%gl:get-buffer-sub-data (target this)
				 (or offset 0)
				 (or size full-length)
				 p)))
    arr))

(defmethod pushg ((this buffer) (data array) &key)
  "Sets the buffer data from a vector array."
  (cffi:with-pointer-to-vector-data (p data)
    (!
      (bind this)
      (%gl:Buffer-Data (target this)
		       (size-in-bytes this)
		       p
		       (usage this))
      (setf (loaded? this) t)))
  data)

(defmethod (setf !>) ((data array) (this buffer))
  (pushg this data))


;; (defmethod triangle-intersection? ((index-buffer index-buffer) (vertex-buffer buffer) origin ray-dir &key)
;;   "Returns distance u, v coordinates and index of the closest triangle (if any) touched by the given the ray origin and direction and the name of the vertex buffer attribute."
;;   (let ((itype (cffi-type->gl-type (qtype index-buffer)))
;; 	(vtype (cffi-type->gl-type (qtype vertex-buffer))))

;;   (labels ((get-vertex-from-index (indexes vertexes x)
;; 	     (v! (cffi:mem-aref vertexes 
;; 				vtype
;; 				(* 3 (cffi:mem-aref indexes itype x)))
;; 		 (cffi:mem-aref vertexes 
;; 				vtype
;; 				(+ 1 (* 3 (cffi:mem-aref indexes itype x))))
;; 		 (cffi:mem-aref vertexes 
;; 				vtype
;; 				(+ 2 (* 3 (cffi:mem-aref indexes itype x))))))

;;   (with-mapped-buffer (ibuffer index-buffer)
;;     (with-mapped-buffer (vbuffer vertex-buffer)

;;       (loop for i from 0 below (get-size index-buffer) by 3
;; 	   for triangle = (loop for j from i to (+ i 2)
;; 			       collect (get-vertex-from-index (ibuffer vbuffer j)))
;; 	   do (multiple-value-bind (new-distance new-u new-v)
;; 		  (ray-triangle-intersect origin ray-dir (aref triangle 0) (aref triangle 1) (aref triangle 2))
		

      
;;       (labels ((rec (primitives i distance u v index)
;; 		 (multiple-value-bind (new-distance new-u new-v)
		     
;; 		 (ray-triangle-intersect? start dir (first (car primitives)) (second (car primitives)) (third (car primitives)))
;; 	       (when (and new-distance
;; 			  (or (null distance)
;; 			      (< new-distance distance)))
;; 		 (setf distance new-distance
;; 		       u new-u
;; 		       v new-v
;; 		       index i)))
;; 	     (if (cdr primitives)
;; 		 (rec (cdr primitives) (1+ i) distance u v index)
;; 		 (values distance u v index))))
;;     (rec (get-primitive this vertex-name) 0 nil nil nil nil)))

;; (defmethod uv->texcoords ((this buffer) index u v)
;;   )
