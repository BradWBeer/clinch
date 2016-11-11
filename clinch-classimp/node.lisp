;;; bone.lisp
;;;; Please see the licence.txt for the CLinch 

(in-package #:clinch)

(defmethod get-nodes ((this classimp:node) &key node-name-hash entities)
    
  (unless node-name-hash 
    (setf node-name-hash (make-hash-table :test 'equal)))

  (make-node this
	     :node-name-hash node-name-hash
	     :entities entities))


(defmethod make-node ((this classimp:node) &key node-name-hash entities)

  (let* ((children (append (map 'list 
				(lambda (n) 
				  (get-nodes n
					     :entities entities
					     :node-name-hash node-name-hash))
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
    (values ret node-name-hash)))
