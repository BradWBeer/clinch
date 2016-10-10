;;;; animation.lisp
;;;; Please see the licence.txt for the CLinch 

;; vertex weights and bone buffers are constant
;; Entities handle weights and bones with regular buffers.
;; There is also a buffer with the bone transform data
;; (rotation, scaling and translation.)
;;
;; A skeleton is needed.
;; Skeletons keep the bone information separate from
;; everything else.
;; It contains it's own node tree (scene graph) and 
;; vertex weights.
;; It can construct a buffer for the bones and weights
;; or fill in the data for existing ones.
;;
;; An entity-animation needs a skeleton but also keep a list of
;; movements for the bones, including easing functions.
;; It's also able to update/update the animation buffer.
;;
;; An animation object should just be able to animate this fine.
;;
;; Also, each animator should have a copy of the animation 
;; and the root node of the mesh.
;; It will travel down the tree updating the bones.
;; Update calls should also take an alist of animations.
;; I'll work on that after I get regular animations working.

(in-package :clinch)

;; vertex weights buffer is constant
;; Each mesh might need it's own buffer of bones.
;; Also, each animator should have a copy of the animation 
;; and the root node of the mesh.
;; It will travel down the tree updating the bones.
;; Update calls should also take an alist of animations.
;; I'll work on that after I get regular animations working.

;; const int MAX_BONES   = 100;
;; const int MAX_WEIGHTS = 5;

;;
;; struct boneTransform {
;;   vec3 scale;
;;   vec4 rot;
;;   vec3 trans;
;; }

;; struct boneWeight {
;;   int bone;
;;   float weight;
;; }

;; GLSL function to rotate vec3 by quaternion. 
;;
;; vec3 rotate(vec3 vec, vec4 quat) {
;; return vec + 2.0 * cross( cross( vec, quat.xyz ) + quat.w * vec, quat.xyz );
;; }

;;
;; vec3 translate(vec3 vec, vec3 translation) {
;;
;; return vec + translation;
;; 
;;}

;;
;; vec3 scale(vec3 vec, vec3 scale) {
;;
;; return vec * scale;
;; 
;;}

;; vec3 transform(vec3 vec, mat4 bone, boneTransform transform) {
;;
;; return translate(rotate(scale(vec, bone.scale), bone.rotation), bone.translation);
;; 
;;}

;; vec3 merge_vectors(vec3 vec, mat4 bones[], boneWeight weights[], boneTransform transforms[]) {
;;
;; int i;
;; vec3 ret = vec3(0, 0, 0);
;; for(i=0; i<MAX_WEIGHTS && weights[i].weight>0, i++) {
;;
;;   vec3 tmp = bones[weights[i].bone] * vec;
;;   ret += translate(tmp, transforms[weight[i].bone) * weight[i].weight;
;;   
;; }
;;
;; return ret;
;; }

(defclass mesh-animation (animation) 
  ((bone-animations :initform nil
		    :initarg :bone-animations
		    :accessor bone-animations)))
   

(defclass mesh-animator (animator) 
  ((animation :initform (error "There must be an animation for the mesh animator!")
	      :initarg :animation 
	      :reader animation)
    (bone-buffer :initform nil
		:initarg :bone-buffer
		:accessor bone-buffer)))



;; (defmethod translate-weights ((bone classimp:bone))
;;   (map 'list (lambda (x) (cons (classimp:id x) (classimp:weight x))) (classimp:weights bone)))

;; (defmethod add-weights-to-hash-table ((bone classimp:bone) (hash hash-table))
;;   (let ((bone bone))
;;     (loop for (id . weight) in (translate-weights bone)
;;           do (progn

;; 	       (setf (gethash id hash)
;;                      (cons (cons weight bone) (gethash id hash)))))))

;; (defmethod get-mesh-weights ((mesh classimp:mesh) (hash hash-table))
;;   (map nil (lambda (x) (add-weights-to-hash-table x hash))
;;        (classimp:bones mesh))
;;   (loop for i from 0 below (length (classimp:faces mesh))
;;      collect (cons i (sort (gethash i hash)
;; 			   (lambda (x y)
;; 			     (> (car x) (car y)))))))

(labels ((between (start end value)
	   (and (>= value start)
		(<= value end)))
	 
	 (interpolate (start end value)
	   (let ((val (float 
		       (/ (- value start)
			  (- end start)))))
	     (values (- 1 val)
		     val)))

	 (find-between (alst time &optional repeat)
	   (assoc (if repeat 
		      (mod time repeat)
		      time)
		  alst :test (lambda (a x)
			       (let ((start (car x))
				     (end (cdr x)))
				 (if end
				     (between start end a)
				     (>= a start)))))))

  (defun find-animation-mix (alst time &optional repeat)
    (let* ((between (find-between alst time repeat))
	   (start (caar between))
	   (end (cdar between)))
      (if end
	  (multiple-value-bind (lo hi) (interpolate start end time)
	    (values lo hi (cdr between)))
	  (values 1 0 (cdr between))))))


(defmethod translate-vector-key ((this classimp::vector-key))
  (list (slot-value this 'classimp::time)
	(slot-value this 'classimp:value)))

(defun make-animation-list (lst)
  (when lst
    (cons (cons  
	   (cons (caar lst) (caadr lst))
	   (cons (cadar lst) (cadadr lst)))
	  (make-animation-list (cdr lst)))))

;; turns a list of vector keys into a useful list.
(defun vector-keys->list (keys)
  (map 'list #'translate-vector-key keys))

(defun slerp-animation (alst time &optional repeat)
  (multiple-value-bind (s1 s2 vecs) (find-animation-mix alst time repeat)
    (if (cdr vecs)
	(q:slerp (car vecs) (cdr vecs) s2)
	(car vecs))))


(defun get-node-name-hash (node)
  (let ((hash (make-hash-table :test 'equal)))
    (labels ((rec (n)
	       (setf (gethash (classimp:name n) hash) n)
	       (map nil #'rec (classimp:children n))))
      (rec node)
      hash)))
				   
			    
