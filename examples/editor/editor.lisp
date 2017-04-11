(ql:quickload :clinch)
(ql:quickload :clinch-cairo)
(ql:quickload :clinch-pango)

(use-package :clinch)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Useful functions for arrays (both strings and paragraph arrays)

(defun make-seq (&key (size 0) initial-contents)
  (make-array (if initial-contents 
		  (length initial-contents)
		  size)
	      :initial-contents initial-contents :adjustable t :fill-pointer t))

(defun make-seq-string (&key (size 0) initial-contents)
  (make-array (if initial-contents 
		  (length initial-contents)
		  size)
	      :initial-contents initial-contents :adjustable t :element-type 'character :fill-pointer t))

(defun sref (seq position)
  (cond ((null position) (aref seq 0))
	((eq t position) (aref seq (1- (length seq))))
	(t (aref seq position))))

(defun spush (seq value)
  (vector-push-extend value seq)
  seq)

(defun spop (seq)
  (unless (zerop (length seq))
    (vector-pop seq))
  seq)

(defun slast (seq)
  (aref seq 
	(1- (length seq))))

(defun sinsert (seq value &optional (position t))
  (if (eq t position) 
      (spush seq value)
      (let ((p (if (null position) 0 position)))
	(spush seq (slast seq))
	(loop for i from (- (length seq) 3) downto p
	   do (setf (aref seq (1+ i)) 
		    (aref seq i)))
	(setf (aref seq p) value)))
  seq)

(defun sreplace (seq value &optional (position t))
  (setf (aref seq 
	      (cond ((null position) 0)
		    ((eq t position) (1- (length seq)))
		    (t position)))
	value)
  seq)

(defun sdelete (seq &optional (position t))
  (if (eq t position) 
      (spop seq)
      (let ((p (if (null position) 0 position)))
	(loop for i from p to (- (length seq) 2)
	   do (setf (aref seq i)
		    (aref seq (1+ i))))
	(spop seq)))
  seq)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *default-attributes* '((:size 25)))
(defparameter *text-buffer* nil)
(defparameter *cursor* '(t t))

(clinch:init :init-controllers nil)

(defun draw-buffer ()
  (fast-draw ()
    (clear-cairo-context .5 .5 .5 0)
    (loop for (text . attr) being the elements of *text-buffer* 
       do (pango::print-with-attributes (text) (append *default-attributes* attr)
	    (let* ((index (length text)))
		  
	      (multiple-value-bind (strong weak) 
		  (pango:get-cursor-pos pango::*layout* index)

		(format t "~A~%" (multiple-value-list (pango:get-line-from-position pango::*layout* index)))
		
		(format t "~S ~S~%" strong weak)
		(let ((start-x (first weak))
		      (start-y (second weak))
		      (h (fourth weak)))
		  (cairo:save)
		  (cairo:set-source-rgb 1 0 0) 
		  (cairo:move-to start-x start-y)
		  (cairo:rel-line-to 0 h) 
		  (cairo:stroke)
		  (cairo:restore))))))))
								     
	    ;;(format t "~A~%" (multiple-value-list (pango:get-cursor-pos pango::*layout* (1- (length text)))))))))
					      

(defun new-paragraph (text &optional attributes)
  (setf *text-buffer*
	(spush *text-buffer*
	       (cons (make-seq-string :initial-contents text)
		     attributes)))
  (draw-buffer))

(defun insert-char-into-paragraph (paragraph char &optional (pos t))
  (sinsert 
   (car (sref *text-buffer* paragraph))
   char pos)
  (draw-buffer))

(defun delete-char-in-paragraph (&optional (paragraph t) (pos t))
  (sdelete (car (sref *text-buffer* paragraph)) pos)
  (draw-buffer))


(defun insert-at-cursor (char)
  (let ((p (car *cursor*))
	(pos (cdr *cursor*)))
    (insert-char-into-paragraph 
     (cond ((null p) 0)
	   ((or (eq t p)
		(>= p (length *text-buffer*)))
	    (1- (length *text-buffer*)))
	   (t p))
     char 
     pos)
    (setf (cdr *cursor*)
	  (cond ((null pos) 1)
		((eq t pos) t)
		(t (1+ pos))))))    


;; (defun delete-at-cursor (char)
;;   (let ((p (car *cursor*))
;; 	(pos (cdr *cursor*)))
;;     (delete-char-in-paragraph 
;;      (cond ((null p) 0)
;; 	   ((or (eq t p)
;; 		(>= p (length *text-buffer*)))
;; 	    (1- (length *text-buffer*)))
;; 	   (t p))
;;      char 
;;      pos)
;;     (setf (cdr *cursor*)
;; 	  (cond ((null pos) 1)
;; 		((eq t pos) t)
;; 		(t (1+ pos))))))    


(defevent *on-window-resized* (window width height ts)
  (draw-buffer))

;; (defun Seq- (seq pos)
;;   (cond ((null pos) 

(defun cursor-left ()
  (cond ((null (cdr *cursor*)) nil)
	((eq t (cdr *cursor*)) (setf (cdr *cursor*)
				     (1- (length (car (sref *text-buffer* (car *cursor*)))))))
	((zerop (cdr *cursor*)) (setf (cdr *cursor*)
				      t))
	(t (setf (cdr *cursor*)
		 (1- (cdr *cursor*))))))

(defun cursor-right ()
  )

(defun cursor-up ()
  )

(defun cursor-down ()
  )

(defun cursor-backspace ()
  (sdelete (car (sref *text-buffer* (car *cursor*))) (cdr *cursor*))
  (draw-buffer))

(defun cursor-delete () 
  )

(defun cursor-line-feed () 
  )


;; Key press handler
(defevent *on-key-down* (win keysym state ts)
  (format t "~A ~A ~A ~A~%" win (sdl2:scancode keysym) state ts)

  (case (sdl2:scancode keysym)
    (:scancode-backspace 	 (cursor-backspace))
  ;;   (:scancode-return    	 (cursor-line-feed))
  ;;   (:scancode-delete   	 (cursor-delete))
  ;;   (:scancode-left 	 (cursor-left))
  ;;   (:scancode-right 	 (cursor-right))
  ;;   (:scancode-up 	 (cursor-up))
  ;;   (:scancode-down 	 (cursor-down))))

    ))
    
(defevent *on-text-input* (window char ts)
  (insert-at-cursor (code-char char)))

(! 
  (setf *text-buffer* (make-seq))
  (setf *cursor* '(t . t))
  (gl:clear-color 1 1 1 1)
  (sdl2:start-text-input)
  (new-paragraph "")
  (draw-buffer))
