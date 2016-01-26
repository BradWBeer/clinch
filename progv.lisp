(in-package :clinch)
;; Not used (yet?)

(defmacro dlet (args &body rest)
  `(progv 
       ,@(loop 
	    with xx
	    with yy
	    for (x y) in args
	    do (push x xx)
	    do (push y yy)
	    finally (return (list `',xx `(list ,@yy))))
       
       ,@rest))


(defmacro dlet* (args &body body)
  (labels ((rec (lst)
	     (if (cdr lst)
		 (append `(dlet (,(car lst)))
			 (list (rec (cdr lst))))
		 (append `(dlet (,(car lst)))
			 body))))
    
    (append (rec args))))


(defmacro dlambda ((args) &body body)
  (let ((f (gensym)))
    `(let ((,f (lambda (,args) ,@body)))
       (setf (gethash ,f *object-data*) 
	     '(dlambda (,args) ,@body))
       ,f)))
	   

(defmacro ddefun (name (args) &body body)
  (let ((f (gensym)))
    `(let ((,f (defun ,name (,args) ,@body)))
       (setf (gethash (symbol-function ,f) *object-data*) 
	     '(ddefun ,name (,args) ,@body))
       ,f)))
