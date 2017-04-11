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

(defun translate-cursor-to-index (seq cursor)
  (cond ((null cursor) 0)
	((eq t cursor) cursor)
	(t (length seq))))

(defun inc-cursor (seq cursor)
  (cond ((null cursor) 1)
	((eq t cursor) t)
	((= cursor (1- (length seq))) t)
	(t (1+ cursor))))


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
  (if (zerop position) 
      (spush seq value)
      (let ((p position))
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
  (if (>= position (1- (length seq)))
      (spop seq)
      (let ((p position))
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
       for i from 0
       do (clinch::with-print (text (append *default-attributes* attr))
	    (when (= i (car *cursor*))
	      (let* ((index  (cdr *cursor*)))
		
		(format t "~A ~S vs ~S~%" i *cursor* (length (car (sref *text-buffer* (car *cursor*)))))
		
		(multiple-value-bind (strong weak) 
		  (pango:get-cursor-pos pango::*layout* index)
		  (let ((start-x (first strong))
		  	(start-y (second strong))
		  	(h (fourth strong)))

		    (cairo:save)
		    (cairo:set-source-rgb 1 0 0) 
		    (cairo:rel-move-to start-x start-y)
		    ;;   (cairo:rel-line-to 0 h) 
		    ;;   (cairo:stroke)
		    (cairo:restore)))))))))
								     
	    ;;(format t "~A~%" (multiple-value-list (pango:get-cursor-pos pango::*layout* (1- (length text)))))))))
					      

(defun new-paragraph (text &optional attributes position)
  (setf *text-buffer*
	(if (or (null position)
		(= position (1- (length *text-buffer*))))
	    (progn 
	      (setf (car *cursor*) (length *text-buffer*))
	      (setf (cdr *cursor*) 0)
	      (spush *text-buffer*
		     (cons (make-seq-string :initial-contents text)
			 attributes)))
	    (progn
	      (setf (cdr *cursor*) 0)
	      (sinsert *text-buffer* 
		       (cons (make-seq-string :initial-contents text)
			     attributes) 
		       (print (incf (car *cursor*))))))))
	    

(defun insert-char-into-paragraph (paragraph char &optional (pos t))
  (sinsert 
   (car (sref *text-buffer* paragraph))
   char pos)
  (draw-buffer))


(defevent *on-window-resized* (window width height ts)
  (draw-buffer))


(defun delete-char-in-paragraph (&optional (paragraph t) (pos t))
  (sdelete (car (sref *text-buffer* paragraph)) pos)
  (draw-buffer))


(defun insert-at-cursor (char)
  (let ((p (car *cursor*))
	(pos (cdr *cursor*)))
    (incf (cdr *cursor*))
    (insert-char-into-paragraph p char pos)))


(defun cursor-left ()
  (when (> (cdr *cursor*) 0)
    (decf (cdr *cursor*))
    (draw-buffer)))

(defun cursor-right ()
  (setf (cdr *cursor*) (min (1+ (cdr *cursor*)) (length (car (sref *text-buffer* (car *cursor*))))))
  (draw-buffer))


(defun cursor-up ()
  )

(defun cursor-down ()
  )

(defun cursor-backspace ()
  (when (> (cdr *cursor*) 0)
    (sdelete (car (sref *text-buffer* (car *cursor*))) (cdr *cursor*))
    (decf (cdr *cursor*))
    (draw-buffer)))

(defun cursor-delete () 
  )

(defun cursor-line-feed ()
  (format t "Cursor line feed!~%");
  (new-paragraph "")
  (draw-buffer))

;; Key press handler
(defevent *on-key-down* (win keysym state ts)
  ;;(format t "~A ~A ~A ~A~%" win (sdl2:scancode keysym) state ts)

  (case (sdl2:scancode keysym)
    (:scancode-backspace 	 (cursor-backspace))
    (:scancode-return    	 (cursor-line-feed))
  ;;   (:scancode-delete   	 (cursor-delete))
    (:scancode-left 	 (cursor-left))
    (:scancode-right 	 (cursor-right))
  ;;   (:scancode-up 	 (cursor-up))
  ;;   (:scancode-down 	 (cursor-down))))

    ))
    
(defevent *on-text-input* (window char ts)
  (insert-at-cursor (code-char char)))

(! 
  (setf *text-buffer* (make-seq))
  (setf *cursor* '(0 . 0))
  (gl:clear-color 1 1 1 1)
  (sdl2:start-text-input)
  (new-paragraph "")
  (draw-buffer))
