;;;; state.lisp
;;;; Please see the licence.txt for the CLinch

(in-package #:clinch)

(defparameter *render-states* (make-hash-table))

  ;; (gl:clear-color 0.0 0.0 0.0 0.0)
  ;; (gl:enable :cull-face :blend :depth-test :depth-clamp)
  ;; (gl:blend-func :src-alpha :one-minus-src-alpha)
  ;; (gl:cull-face :back)
  ;; (gl:front-face :ccw)
  ;; (gl:depth-mask :true)
  ;; (gl:depth-func :less)
  ;; (gl:depth-range 0.0 1.0))


;; (defun get-or-set-up-enabled-state (name enable)
;;   (multiple-value-bind (value found?) (gethash name *render-states*)
;;     (if found? 
;; 	(unless (eql enable value)
;; 	  (!gl:enable 
	

;; (defun !enable (value)
  
