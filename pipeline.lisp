;;;; pipeline.lisp
;;;; Please see the licence.txt for the CLinch 

(in-package #:clinch)

(defmacro make-pipeline (&key init loop uninit)
  `(list
    (lambda (w h) ,@loop)

    (lambda () ,@init)

    (lambda () ,@uninit)))

(defmacro pipeline-get-loop (pipeline)
  `(first ,pipeline))

(defmacro pipeline-get-init (pipeline)
  `(second ,pipeline))

(defmacro pipeline-get-uninit (pipeline)
  `(third ,pipeline))

(defun run-loop (pipeline width height)
    (funcall (pipeline-get-loop pipeline) width height))

(defun run-init (pipeline)
    (funcall (pipeline-get-init pipeline)))

(defun run-uninit (pipeline)
    (funcall (pipeline-get-uninit pipeline)))

