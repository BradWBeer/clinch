;;;; threads.lisp
;;;; Please see the licence.txt for the CLinch 

(in-package #:clinch)

(defmacro $ (&body body)
  `(bordeaux-threads:make-thread 
    (lambda ()
      ,@body)))

(defun $> (thread)
  (unless (bordeaux-threads:thread-alive-p thread)
    (values (bordeaux-threads:join-thread thread)
	    t)))

