;;;; clinch-classimp.lisp
;;;; Please see the licence.txt for the CLinch 

(in-package #:clinch)

(defun load-mesh (path)
  (classimp:with-log-to-stdout ()
    (classimp:import-into-lisp 
     (cffi-sys:native-namestring (truename path))
     :processing-flags '(:ai-Process-Triangulate :ai-Process-Join-Identical-Vertices :ai-Process-Sort-By-P-Type))))

