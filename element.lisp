
(in-package :clinch)

(defclass element ()
  ((parent   :reader    parent
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
   (attribute :initform nil)
   (changed  :reader changed?
	     :initform t)
   (before-render :initform nil
		  :initarg :before-render
		  :accessor before-render)
   (after-render :initform nil
		  :initarg :after-render
		  :accessor after-render)
   (once          :initform nil
		  :initarg :once
		  :accessor once)))


(defmethod initialize-instance :after ((this element) &key attributes parent)
  
  (when (parent this) (element-push-back (parent this) this))

  (with-slots ((children children)) this
    (loop for c in children do (setf (slot-value c 'parent) this)))
  
  (when attributes (map nil
			  (lambda (x) (setf (attribute this (first x)) (second x)))
			  attributes)))



(defmethod (setf changed?) (val (this element))
  "Set this node to update later."
  (setf (slot-value this 'changed) t))


(defmethod (setf parent) ((parent element) (this element))

  (setf (slot-value this 'parent) parent)
  ;(element-push-back (slot-value parent 'children) this)
  )

;; just for testing and example purposes only. 
(defmacro element (&body args)
  
  (multiple-value-bind (keys children) (clinch::split-keywords args)
    
    `(let ((*parent* (make-instance 'element ,@keys :parent *parent*)))
       ,@children
       *parent*)))
    

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
  
  (print "Element's setf called!")
  (setf (slot-value this 'children) children)
  
  (loop for e in children
     if (typep e 'clinch:element)
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

  (when (once this)
    (funcall (once this) this)
    (setf (once this) nil))

  (when (before-render this)
    (funcall (before-render this) this))

  (loop for c in (children this)
       do (render c))

  (when (after-render this)
    (funcall (after-render this) this)))

(defmethod unload ((this element) &key) )

(defmethod element-back ((this element))
  (car (last (children this))))

(defmethod element-front ((this element))
  (car (children this)))

(defmethod element-push-back ((this element) (child element))
  (setf (slot-value this 'children) (append (slot-value this 'children) (list child))))

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

