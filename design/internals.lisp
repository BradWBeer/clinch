(ql:quickload :alexandria)

(setf *nodes* '((n2 n1)
		(n1 root)
	 	(animation1 timer1 camera)
		(a animation1 camera)
		(b animation1 camera)
		(c animation2 camera)
		(c n3 camera)
		(a n2 camera)  
		(b n2 camera)
		(n3 root)
		(camera n2)
		(k l)
		(l nil)
		(screen a b c)
		))

;; (defun graph->hash (graph)
;;   (let ((ret (make-hash-table)))
;;     (map nil
;; 	 (lambda (tuple)
;; 	   (setf (gethash (car tuple) ret) 
;; 		 (remove-duplicates
;; 		  (append (cdr tuple) (gethash (car tuple) ret)))))
;; 	 graph)
;;     ret))

(defun graph->reverse-hash (graph)
	   (loop 
	      with h = (make-hash-table)
	      for (a . b) in graph
	      do (loop for i in b
		    do (unless (member a (gethash i h))
			 (setf (gethash i h) (cons a (gethash i h)))))
	      finally (return h)))

(defun find-deps-from-hash (item hash found)
  (let ((ret (cons item found)))
    (dolist (i (gethash item hash) ret)
      (unless (member i ret)
	(setf ret (find-deps-from-hash i hash ret))))))

(defun find-deps (item graph &optional found)
  (let ((deps (graph->reverse-hash graph)))
    (find-deps-from-hash item deps found)))

(defun find-all-deps (lst graph &optional found)
  (let ((deps (graph->reverse-hash graph)))
    (dolist (i lst found)
      (setf found
	    (find-deps-from-hash i deps found)))))		     

(defun find-orphans (result graph)
  (remove-duplicates 
   (set-difference (alexandria:flatten graph) 
		   (find-deps result graph))))


(defun update-nodes ()

  ;; First find the topological sort.
  ;; Then go through each item in order.
  ;; For every element, check to see any of its requirements are modified.
  ;; if so then update this item and mark it as changed.

  ;; This should only update the changed items.

  ;; Complication:
  ;;    Some items rely on objects but don't need updated
  ;;    Example: the projection and model-view matrices.
  ;;    For now mark these (projection, MV) as "un-updatable"?

  ;; Complication:
  ;;    All updates/renders are function calls.
  ;;    Do I just use an update/render/etc functions as events?
  ;;    It would also require way to list of dependancies for
  ;;    each object.

  
  )
(defun topological-sort (graph &key (test 'eql))
  "Graph is an association list whose keys are objects and whose
values are lists of objects on which the corresponding key depends.
Test is used to compare elements, and should be a suitable test for
hash-tables.  Topological-sort returns two values.  The first is a
list of objects sorted toplogically.  The second is a boolean
indicating whether all of the objects in the input graph are present
in the topological ordering (i.e., the first value)."
  (let ((entries (make-hash-table :test test)))
    (flet ((entry (vertex)
	     "Return the entry for vertex.  Each entry is a cons whose
              car is the number of outstanding dependencies of vertex
              and whose cdr is a list of dependants of vertex."
	     (multiple-value-bind (entry presentp) (gethash vertex entries)
	       (if presentp entry
		   (setf (gethash vertex entries) (cons 0 '()))))))
      ;; populate entries initially
      (dolist (vertex graph)
	(destructuring-bind (vertex &rest dependencies) vertex
	  (let ((ventry (entry vertex)))
	    (dolist (dependency dependencies)
	      (let ((dentry (entry dependency)))
		(unless (funcall test dependency vertex)
		  (incf (car ventry))
		  (push vertex (cdr dentry))))))))
      ;; L is the list of sorted elements, and S the set of vertices
      ;; with no outstanding dependencies.
      (let ((L '())
	    (S (loop for entry being each hash-value of entries
		  using (hash-key vertex)
		  when (zerop (car entry)) collect vertex)))
	;; Until there are no vertices with no outstanding dependencies,
	;; process vertices from S, adding them to L.
	(do* () ((endp S))
	  (let* ((v (pop S)) (ventry (entry v)))
	    (remhash v entries)
	    (dolist (dependant (cdr ventry) (push v L))
	      (when (zerop (decf (car (entry dependant))))
		(push dependant S)))))
	;; return (1) the list of sorted items, (2) whether all items
	;; were sorted, and (3) if there were unsorted vertices, the
	;; hash table mapping these vertices to their dependants
	(let ((all-sorted-p (zerop (hash-table-count entries))))
	  (values (nreverse L)
		  all-sorted-p
		  (unless all-sorted-p
		    entries)))))))
