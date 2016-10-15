;;; bone.lisp
;;;; Please see the licence.txt for the CLinch 

(in-package #:clinch)

(defmethod make-node ((this classimp:node) &key bone-hash node-name-hash entities bone-count)

  (let* ((children (append (map 'list 
				(lambda (n) 
				  (multiple-value-bind (node id) (get-nodes n
									    :entities entities
									    :bone-hash bone-hash
									    :node-name-hash node-name-hash
									    :bone-count bone-count)
				   (setf bone-count id)
				   node))
				(classimp:children this))
			   (map 'list (lambda (e)
					(nth e entities))
				(classimp:meshes this))))
	 (ret (make-instance 'node 
			     :name (classimp:name this)
			     :matrix (classimp:transform this)
			     :children children)))

    (when node-name-hash 
      (setf (gethash (classimp:name this) node-name-hash) ret))
    (values ret bone-count)))
