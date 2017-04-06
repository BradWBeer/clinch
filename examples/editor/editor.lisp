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
(defparameter *caret* '(t t))

(clinch:init :init-controllers nil)

(defun draw-buffer ()
  (fast-draw ()
    (clear-cairo-context .5 .5 .5 0)
    (loop for (text . attr) being the elements of *text-buffer* 
       do (print-text-with-attributes text (append *default-attributes* attr)))))

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


(defun insert-at-caret (char)
  (let ((p (car *caret*))
	(pos (cdr *caret*)))
    (insert-char-into-paragraph 
     (cond ((null p) 0)
	   ((or (eq t p)
		(>= p (length *text-buffer*)))
	    (1- (length *text-buffer*)))
	   (t p))
     char 
     pos)
    (setf (cdr *caret*)
	  (cond ((null pos) 1)
		((eq t pos) t)
		(t (1+ pos))))))    


;; (defun delete-at-caret (char)
;;   (let ((p (car *caret*))
;; 	(pos (cdr *caret*)))
;;     (delete-char-in-paragraph 
;;      (cond ((null p) 0)
;; 	   ((or (eq t p)
;; 		(>= p (length *text-buffer*)))
;; 	    (1- (length *text-buffer*)))
;; 	   (t p))
;;      char 
;;      pos)
;;     (setf (cdr *caret*)
;; 	  (cond ((null pos) 1)
;; 		((eq t pos) t)
;; 		(t (1+ pos))))))    



(defevent *on-window-resized* (window width height ts)
  (draw-buffer))

;; Key press handler
(defevent *on-key-down* (win keysym state ts)
  (format t "~A ~A ~A ~A~%" win (sdl2:scancode keysym) state ts)
  (cond ((eql (sdl2:scancode keysym) :scancode-backspace)
	 (delete-char-in-paragraph))))
	 
  (defevent *on-text-input* (window char ts)
  (insert-at-caret (code-char char)))

(! 
  (setf *text-buffer* (make-seq))
  (setf *caret* '(t . t))
  (gl:clear-color 1 1 1 1)
  (sdl2:start-text-input)
  (new-paragraph "")
  (draw-buffer))
