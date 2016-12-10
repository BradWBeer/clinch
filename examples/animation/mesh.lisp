;; This is working file as I test features...please don't use. Use tutorial05 instead. 

(ql:quickload :clinch)
(ql:quickload :clinch-freeimage)
(ql:quickload :clinch-classimp)

(use-package :clinch)

(trace classimp::make-classimp-entity)

;; (defparameter scene-path
;; 	   "/home/warweasle/work/tools/assimp/test/models/X/test_cube_compressed.x")

(defparameter scene-path
  "/home/warweasle/work/tools/assimp/test/models/X/Testwuson.X")
;;(defparameter scene-path "/home/warweasle/work/external/assimp/test/models/MD2/sydney.md2")

(defparameter *node* nil)
(defparameter *projection* nil)

(defparameter *import-return-vals* nil)
(defparameter *dummy-root* nil)
(defparameter *animations* nil)
(defparameter *dummy-root* nil)
(defparameter *node-hash* nil)
(defparameter *scene* nil)
(defparameter *base-path* nil)
(defparameter *materials* nil)
(defparameter *meshes* nil)
(defparameter *entities* nil)
(defparameter *node-names* nil)
(defparameter *a1* nil)
(defparameter *toggle-color* nil)
(defparameter *q* nil)

(defun init-test ()
  (setf *node* (make-instance 'clinch:node :parent nil))
  (clinch:translate *node* (clinch:v! 0 0 -2))
  (multiple-value-setq (*dummy-root*
			*animations*
			*node-hash*
			*scene*
			*base-path*
			*materials*
			*meshes*
			*entities*
			*node-names*)
    (clinch::import-scene scene-path))
  (setf *a1* (first *animations*)
	*a2* (second *animations*)
	*a3* (third *animations*))
  (clinch:add-child *node* *dummy-root*)
  
  (setf mesh (elt *meshes* 0))

  (setf entity (car *entities*))
  (setf skelly 
	       (make-instance 'clinch::skeleton :mesh mesh :node-hash *node-names*))
  (setf animator (make-instance 'clinch::node-animator :bones skelly :animation *a1*))
  (play animator)

  (setf (clinch::bones entity) skelly)
  (setf (uniform entity "bones") skelly)
  (setf (attribute entity "weights") (clinch::weights-buffer skelly))
  (setf (attribute entity "boneIDs") (clinch::bone-id-buffer skelly))
  )  

;; Next runs one time before the next on-idle.
(clinch:defevent clinch:*next* ()

  ;; Enable a few opengl features. 
  (gl:enable :blend :depth-test :line-smooth :point-smooth :texture-2d :cull-face)

  ;; Set the blending mode. 
  (%gl:blend-func :src-alpha :one-minus-src-alpha)
  (gl:polygon-mode :front-and-back :fill)

  (gl:clear-color 0 0 1 0)

  ;; run init once
  (init-test))


;; render

(clinch:defevent clinch:*on-idle* ()

  (test-step (nth 0 *animations*) *ticks*)
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  ;;(clinch:render entity :projection *projection*)
  (clinch:render *node* :projection *projection*)
  
  )

(clinch:defevent clinch:*on-mouse-down* (win mouse x y button state clicks ts)

  ;;(format t "win: ~A mouse: ~A x: ~A y: ~A button: ~A state: ~A clicks: ~A ts: ~A~%" win mouse x y button state clicks ts)
  (when *q* 
    (multiple-value-bind (origin ray) (unproject x y (width *viewport*) (height *viewport*) (m4:inverse *projection*))
      (print (clinch::ray-triangles-intersect
	      (clinch::transform-points (pullg (attribute *q* "v")) (transform *node*))
	      (pullg (indexes *q*))
	      origin
	      ray)))))

;; Rotate and translate with mouse
(clinch:defevent clinch:*on-mouse-move* (win mouse state x y xrel yrel ts)
  ;;(format t "x:~A y:~A mouse:~A state:~A~%" x y mouse state)
  (case state
    (1 (clinch:rotate *node*
		      (q:from-fixed-angles (clinch:degrees->radians yrel) (clinch:degrees->radians xrel) 0)))
    
    (2 (clinch:translate *node* (clinch:v! (/ xrel 16) (/ yrel -16) 0)))))

;; on window resize
(clinch:defevent clinch:*on-window-resized* (win width height ts)
  ;;(format t "Resized: ~A ~A~%" width height)
  
  (setf *projection* (clinch::make-perspective-transform (clinch:degrees->radians 45)
							 (/ width height) .1 1000)))
;; zoom in and out
(clinch:defevent clinch:*on-mouse-wheel-move* (win mouse x y ts)
  ;;(format t "win=~A mouse=~A x=~A y=~A ts=~A~%" win mouse x y ts)
  
  (clinch:translate *node* (clinch:v! 0 0 (/ y 1))))

(clinch:init :asynchronous t :init-controllers nil)


(defun find-first-node (root f)
  (block outside
    (clinch:walk-node-tree
     root
     (lambda (n)
       (when (funcall f n)
	 (return-from outside n))))))


(defun find-node-by-name (root name)
  (find-first-node root (lambda (n)
			  (string= (name n) name))))


(defun find-next-non-bone (root)
  (find-first-node root
		   (lambda (n)
		     (not (typep n 'clinch::bone)))))


(defun find-next-bone (root)
  (find-first-node root
		   (lambda (n)
		     (typep n 'clinch::bone))))

(defmethod node-tree ((this node))
	   (cons (name this)
		 (loop for i in (children this)
		      when (typep i 'node)
		      collect (node-tree i))))
	 

(defmethod node-top-map-string ((this node) &optional parent)
  (let ((children 
	 (loop for n in (children this)
	    when (typep n 'node)
	    append (node-top-map-string n this))))
    (if (and parent (name parent))
	(cons (list (name parent) (name this))
	      children )
	children)))

(defmethod node-top-map ((this node) &optional parent)
  (let ((children 
	 (loop for n in (children this)
	    when (typep n 'node)
	    append (node-top-map n this))))
    (if (and parent parent)
	(cons (list parent this)
	      children )
	children)))



(defparameter *test-shader* nil)
(defun get-test-shader (&optional force)
  (if (and (not force) *test-shader* (program *test-shader*))
      *test-shader*
      (setf *test-shader*
	    (make-instance 'clinch:shader-program
			   :name "test-shader"
			   :vertex-shader (alexandria:read-file-into-string
					   (concatenate 'string 
							(directory-namestring
							 (asdf:system-relative-pathname :clinch "clinch.asd"))
							"shaders/test.vert"))
			   :fragment-shader (alexandria:read-file-into-string
					   (concatenate 'string 
							(directory-namestring
							 (asdf:system-relative-pathname :clinch "clinch.asd"))
							"shaders/generic-single-diffuse-light-shader.frag"))))))


(defun test-step (animation time)

 
  ;;(get-keyframe animation (mod time (run-length animation)))


  ;;(update (node ao))
  ;;(update skelly)
  ;;(update animator)
  (render animator)
  )


;;  (setf (uniform entity "bones") (pullg (clinch::bone-buffer skelly))))


(defun recompile-shader () 
  (setf clinch::*generic-single-diffuse-light-animation-shader* nil)
  (setf as (clinch::get-generic-single-diffuse-light-animation-shader))
  (setf (shader-program entity) as))
