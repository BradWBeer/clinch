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
(defparameter *caret* nil)

(clinch:init :init-controllers nil)

(! (gl:clear-color 1 1 1 1)
   (sdl2:start-text-input))


(defun draw-buffer ()
  (fast-draw ()
    (clear-cairo-context)
    (loop for (text . attr) in *text-buffer* 
	 do (print-text-with-attributes text (append *default-attributes* attr)))))

(defun new-paragraph (text &optional attributes)
  (setf *text-buffer*
	(append *text-buffer*
		(list (cons text attributes))))
  (draw-buffer))

;;(defun delete-paragraph (paragraph)
  

(defun insert-char (string char &optional (pos t))
  (when (characterp char)
      (setf char (princ-to-string char)))
  
  (cond ((null pos) (concatenate 'string char string))
	((eq t pos) (concatenate 'string string char))
	(t (concatenate 'string 
			(subseq string 0 pos) 
			char
			(subseq string pos)))))


(defun insert-char-into-paragraph (paragraph char &optional (pos t))
  (setf (car (nth paragraph *text-buffer*))
	(insert-char (car (nth paragraph *text-buffer*))
		     char pos))
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
    
(defevent *on-window-resized* (window width height ts)
  (draw-buffer))

(defevent *on-text-input* (window char ts)
  (insert-at-caret (code-char char)))
