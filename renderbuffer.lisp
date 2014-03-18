;;;; frambuffer.lisp
;;;; Please see the licence.txt for the CLinch

(in-package #:clinch)


(defclass render-buffer ()
  ((renderbuffer-id
    :reader renderbuffer-id
    :initform nil
    :initarg :renderbuffer-id)
   (width
    :accessor width
    :initarg :width)
   (height
    :accessor height
    :initarg :height)
   (data-format
    :accessor data-format
    :initform :depth-component32 
    :initarg :format)))


(defmethod initialize-instance :after ((this render-buffer) &key)
  
  (unless (slot-value this 'renderbuffer-id)
    (setf (slot-value this 'renderbuffer-id) (car (gl:gen-renderbuffers 1))))

  (let ((o (slot-value this 'renderbuffer-id)))
    (trivial-garbage:finalize this (lambda (m) (gl:delete-buffers (list o)))))

  (update this))

(defmethod update ((this render-buffer) &key)
  (bind this)

  (gl:renderbuffer-storage :renderbuffer (data-format this) (width this) (height this)))

(defmethod bind ((this render-buffer) &key)
  (gl:bind-renderbuffer :renderbuffer (renderbuffer-id this)))


(defmethod unbind ((this render-buffer) &key)
  (gl:bind-renderbuffer :renderbuffer 0))

(defmethod unload ((this render-buffer) &key)
  (trivial-garbage:cancel-finalization this)
  (gl:delete-renderbuffers (list (slot-value this 'renderbuffer-id))))
  


    
    