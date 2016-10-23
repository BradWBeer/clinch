;;;; animation.lisp
;;;; Please see the licence.txt for the CLinch 

(in-package :clinch)

(defclass node-animation (animation) 
  ((node
    :initform nil
    :initarg :node
    :accessor node)))

(defmethod initialize-instance :after ((this node-animation) &key ai-animation node-names)

  (with-accessors ((f frames)
		   (n node)) this
    ;;(when node (setf n node))

    (when (and ai-animation node-names)
      
      (when f (error "Can not merge classimp:animation with existing channels."))
     
      (setf f (decode-channels ai-animation node-names)))))


(defun key->cons (k)
  (cons (ai:key-time k)
	(ai:value k)))

(defun keys->list (k)
  (map 'list #'key->cons k))

;; Not using PRE-STATE or POST-STATE until later...
;; Then I'll use cl:easing...cool stuff.
(defun decode-channel (chan bone-nodes)
  (list (gethash (classimp:node-name chan) bone-nodes)
	(keys->list (ai:position-keys chan))
	(keys->list (ai:scaling-keys chan))
	(keys->list (ai:rotation-keys chan))))


(defun decode-channels (animation bone-nodes)
  (map 'list (lambda (chan)
	       (decode-channel chan bone-nodes))
       (ai:channels animation)))


;; (defmethod get-top-animation-node (this node-names &key)
;;   nil)
   
;; (defmethod get-top-animation-node ((this node) node-names &key)
;;   (format t "~A~%" (gethash (name this) node-names))
;;   (if (gethash (name this) node-names)
;;       this
;;       (loop for n in (children this)
;; 	 when (and (typep n 'node)
;; 		   (get-top-animation-node n node-names))
;; 	 do (return n))))

;; ;; I don't think I need a separate mesh animator...
;; (defclass mesh-animator (animator) 
;;   ((animation :initform (error "There must be an animation for the mesh animator!")
;; 	      :initarg :animation 
;; 	      :reader animation)
;;     (bone-buffer :initform nil
;; 		:initarg :bone-buffer
;; 		:accessor bone-buffer)))



;; (labels ((between (start end value)
;; 	   (and (>= value start)
;; 		(<= value end)))
	 
;; 	 (interpolate (start end value)
;; 	   (let ((val (float 
;; 		       (/ (- value start)
;; 			  (- end start)))))
;; 	     (values (- 1 val)
;; 		     val)))

;; 	 (find-between (alst time &optional repeat)
;; 	   (assoc (if repeat 
;; 		      (mod time repeat)
;; 		      time)
;; 		  alst :test (lambda (a x)
;; 			       (let ((start (car x))
;; 				     (end (cdr x)))
;; 				 (if end
;; 				     (between start end a)
;; 				     (>= a start)))))))

;;   (defun find-animation-mix (alst time &optional repeat)
;;     (let* ((between (find-between alst time repeat))
;; 	   (start (caar between))
;; 	   (end (cdar between)))
;;       (if end
;; 	  (multiple-value-bind (lo hi) (interpolate start end time)
;; 	    (values lo hi (cdr between)))
;; 	  (values 1 0 (cdr between))))))


;; (defmethod translate-vector-key ((this classimp::vector-key))
;;   (list (slot-value this 'classimp::time)
;; 	(slot-value this 'classimp:value)))

;; (defun make-animation-list (lst)
;;   (when lst
;;     (cons (cons  
;; 	   (cons (caar lst) (caadr lst))
;; 	   (cons (cadar lst) (cadadr lst)))
;; 	  (make-animation-list (cdr lst)))))

;; ;; turns a list of vector keys into a useful list.
;; (defun vector-keys->list (keys)
;;   (map 'list #'translate-vector-key keys))

;; (defun slerp-animation (alst time &optional repeat)
;;   (multiple-value-bind (s1 s2 vecs) (find-animation-mix alst time repeat)
;;     (if (cdr vecs)
;; 	(q:slerp (car vecs) (cdr vecs) s2)
;; 	(car vecs))))


;; (defun get-node-name-hash (node)
;;   (let ((hash (make-hash-table :test 'equal)))
;;     (labels ((rec (n)
;; 	       (setf (gethash (classimp:name n) hash) n)
;; 	       (map nil #'rec (classimp:children n))))
;;       (rec node)
;;       hash)))
				   
			    
