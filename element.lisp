
(in-package :clinch)

(defclass element ()
  ((parent   :accessor   parent
	     :initform  nil
	     :initarg  :parent)

   (children :reader   children
	     :initform nil
	     :initarg  :children)
   
   (id       :reader   id
	     :initform nil
	     :initarg  :id)

   (name     :reader   name
	     :initform nil
	     :initarg  :name)
   (attribute :initform nil)))


(defmethod initialize-instance :after ((this element) &key attributes)
  
  (with-slots ((children children)) this
    (loop for c in children do (setf (slot-value c 'parent) this)))
  
  (when attributes (map nil
			  (lambda (x) (setf (attribute this (first x)) (second x)))
			  attributes)))


(defmacro element (&rest args)
  
  (multiple-value-bind (keys children) (clinch::split-keywords args)
    
    `(make-instance 'element ,@keys :children (list ,@children))))
    

(defmethod attribute ((this element) key)
  
  (with-slots ((attr attribute)
	       (parent parent)) this

    (if attr
	(multiple-value-bind (val found?) (gethash key attr)
	  
	  (if found?
	      (values val this)
	      (when parent (attribute parent key))))
	(when parent (attribute parent key)))))



(defmethod (setf attribute) (new-val (this element) key)

  (with-slots ((attr attribute)) this

    (unless attr (setf attr (make-hash-table)))
    (setf (gethash key attr) new-val)))
    
			       

(defmethod (setf children) (children (this element))
  
    (setf (slot-value this 'children) children)

    (loop for e in children
       do (setf (parent e) this)))



(defmethod print-object ((this element) s)

  (format s "(element ")
  (when (name     this)     (format s ":name ~S " (name this)))
  (when (id       this)      (format s ":id ~S "   (id this)))
  (when (children this) (format s "~{~%~S~}" (children this)))
  (format s ")"))



(defmethod update ((this element) &key)
  
  (loop for c in (children this)
       do (update c)))


(defmethod render ((this element) &key)

  (loop for c in (children this)
       do (render c)))

(defmethod unload ((this element) &key) )

(defmethod element-back ((this element))
  (car (last (children this))))

(defmethod element-front ((this element))
  (car (children this)))

(defmethod element-push-back ((this element) (child element))
  )

(defmethod element-get-child ((this element) (i integer))

  )

(defmethod element-get-child ((this element) (e element))

  )

(defmethod element-push-front ((this element))
  )

(defmethod element-pop-back ((this element))
  )

(defmethod element-pop-front ((this element))
  )

(defmethod element-remove-child ((this element) (i integer))

  )

(defmethod element-remove-child ((this element) (e element))

  )

(defmethod element-replace-child ((this element) (child element) (i integer))

  )

(defmethod element-next-sibling ((this element))
  )

(defmethod element-previous-sibling ((this element))
  )

