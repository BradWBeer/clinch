(ql:quickload :clinch)
(ql:quickload :clinch-cairo)
(ql:quickload :clinch-pango)

(use-package :clinch)

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
