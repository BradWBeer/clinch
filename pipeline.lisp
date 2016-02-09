;;;; pipeline.lisp
;;;; Please see the licence.txt for the CLinch 

(in-package #:clinch)

(defmacro make-pipeline (&key init loop uninit)
  "Creates a pipeline with init, loop and uninit functions.
  :loop adds w h variables which gives width and height respectively."
  `(list
    (lambda (w h) 
      (declare (ignorable w h))
	       ,@loop)

    (lambda () ,@init)

    (lambda () ,@uninit)))

(defmacro pipeline-get-loop (pipeline)
  "Gets the Loop function. (lambda (w h) ....)"
  `(first ,pipeline))

(defmacro pipeline-get-init (pipeline)
  "Gets the init function. (lambda () ...)"
  `(second ,pipeline))

(defmacro pipeline-get-uninit (pipeline)
  "Gets the uninit function. (lambda () ...)"
  `(third ,pipeline))

(defun run-loop (pipeline width height)
  "Run the loop!"
    (funcall (pipeline-get-loop pipeline) width height))

(defun run-init (pipeline)
  "Run init"
    (funcall (pipeline-get-init pipeline)))

(defun run-uninit (pipeline)
  "Run unint"
    (funcall (pipeline-get-uninit pipeline)))

