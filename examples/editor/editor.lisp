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

(defun sconcat-string (seq1 seq2)
  (make-seq-string :initial-contents (concatenate '(VECTOR CHARACTER) seq1 seq2)))

(defun sconcat (seq1 seq2)
  (make-seq-string :initial-contents (concatenate 'VECTOR seq1 seq2)))

(defun ssplit (seq pos)
  (values 
   (make-seq-string :initial-contents (subseq seq 0 pos)) 
   (make-seq-string :initial-contents (subseq seq pos))))
  


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *default-attributes* '((:size 25)))
(defparameter *text-buffer* nil)

(defparameter *buffer-text-ranges* nil)
(defparameter *cursor-pixel-position* nil)
(defparameter *cursor* '(t t))

(clinch:init :init-controllers nil)

(defun draw-buffer (&key reposition-x)
  
  (fast-draw ()
    (cairo:move-to 0 0)
    (clear-cairo-context .5 .5 .5 0)
    (let ((lines-data nil))
      (loop 
	 for (text . attr) being the elements of *text-buffer*
	 for i from 0
	 do (clinch::with-print (text (append *default-attributes* attr))
	      (let ((layout-data (pango::get-layout-lines-data pango::*layout*)))
		(setf lines-data (cons layout-data lines-data))

		(when (and reposition-x (car reposition-x))
		  (let ((p (first reposition-x))
			(l (second reposition-x))
			(x (third  reposition-x)))
		    (when (= i p)
		      (setf (car *cursor*) p)
		      (multiple-value-bind (index trailing)
			  (pango::layout-line-x-to-index
			   (pango::pango_layout_get_line pango::*layout* l)
			   (* pango::PANGO_SCALE (round x)))
			(print (list index trailing))
			(setf (cdr *cursor*) (+ index trailing))))))
		
		(when (= i (car *cursor*))
		  (let* ((index  (cdr *cursor*)))
		    
		    (multiple-value-bind (strong weak) 
			(pango:get-cursor-pos pango::*layout* index)
		      ;;(format t "~A ~A~%" strong weak)
		      
		      (let ((start-x (first strong))
			    (start-y (second strong))
			    (h (fourth strong))
			    (point (multiple-value-list (cairo:get-current-point))))

			(setf *cursor-pixel-position* (map 'list #'+ strong point))
			(when reposition-x
			  (setf (car *cursor-pixel-position*) (third reposition-x)))
			
			(cairo:save)
			
			;; (cairo:set-source-rgba 0 0 0 .5)
			;; (cairo:rectangle (first (car scrap)) 0 (third (car scrap)) (fourth (car scrap)))
			;; (cairo:fill-path)
			;; (cairo:move-to (car point) (cadr point))
			
			(cairo:set-source-rgb 1 0 0) 
			(cairo:rel-move-to start-x start-y)
			(cairo:rel-line-to 0 h) 
			(cairo:stroke)
			(cairo:restore)
			(cairo:move-to (car point) (cadr point))
			;;(format t "p2 ~A~%" (multiple-value-list (cairo:get-current-point)))
			))))
		(setf *buffer-text-ranges* (reverse lines-data))))))))
		
								   
	    ;;(format t "~A~%" (multiple-value-list (pango:get-cursor-pos pango::*layout* (1- (length text)))))))))
					      

(defun new-line (text &optional attributes position)
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
  (if (zerop (cdr *cursor*))
      (unless (zerop (car *cursor*))
	(decf (car *cursor*))
	(setf (cdr *cursor*) (length (car (sref *text-buffer* (car *cursor*)))))
	(draw-buffer))
      (progn (decf (cdr *cursor*))
	     (draw-buffer))))

(defun cursor-right ()
  (if (= (cdr *cursor*) 
	 (length (car (sref *text-buffer* (car *cursor*)))))
      (when (< (car *cursor*) (1- (length *text-buffer*)))
	(incf (car *cursor*))
	(setf (cdr *cursor*) 0)
	(draw-buffer))
      (progn 
	(setf (cdr *cursor*) (min (1+ (cdr *cursor*)) (length (car (sref *text-buffer* (car *cursor*))))))
	(draw-buffer))))


(defun find-line-by-position (line pos)
  (loop
     for x from 0
     for l in (nth line *buffer-text-ranges*)
     when (and (>= pos (nth 0 l))
	       (< pos (+ (nth 0 l) (nth 1 l))))
     return (values x l)
       finally (return (values x l))))

(defun cursor-home ()
  (multiple-value-bind (i line) (find-line-by-position (car *cursor*) (cdr *cursor*))
    (setf (cdr *cursor*) 
	  (nth 0 line)))
  (draw-buffer))

(defun cursor-end ()
  (multiple-value-bind (i line) (find-line-by-position (car *cursor*) (cdr *cursor*))
    (setf (cdr *cursor*) 
	  (+ (nth 0 line) (nth 1 line))))
  (draw-buffer))

(defun get-previous-line (paragraph line lines-data)
  (if (<= line 0) 
      ;; get previous paragraph
      (when (> paragraph 0)
	(let ((new-para (1- paragraph)))
	  (values new-para
		  (1- (length (nth new-para lines-data))))))
      (values paragraph (1- line))))

(defun get-next-line (paragraph line lines-data)
  (let ((p (nth paragraph lines-data)))
    (if (< line (1- (length p)))
	(values paragraph (1+ line))
	(when (< paragraph (length lines-data))
	  (values (1+ paragraph) 0)))))

(defun get-cursor-line-number (cursor lines-data)
  (let* ((line (car cursor))
	 (pos  (cdr cursor))
	 (para (nth line lines-data)))
    (or 
     (loop 
	for x from 0
	for (start len . rest) in para
	if (and (>= pos start)
		(<  pos (+ start len)))
	return x)
     (1- (length para)))))
	

(defun cursor-up ()
  (draw-buffer :reposition-x 
	       (multiple-value-bind (p l)
		   (get-previous-line (car *cursor*) 
				      (get-cursor-line-number *cursor* *buffer-text-ranges*)
				      *buffer-text-ranges*)
		 (list p l (car *cursor-pixel-position*)))))

(defun cursor-down ()
  (draw-buffer :reposition-x
	       (multiple-value-bind (p l)
		   (get-next-line (car *cursor*) 
				  (get-cursor-line-number *cursor* *buffer-text-ranges*)
				  *buffer-text-ranges*)
		 (list p l (car *cursor-pixel-position*)))))

(defun cursor-backspace ()
  (if (> (cdr *cursor*) 0)
      (progn (decf (cdr *cursor*))
	     (sdelete (car (sref *text-buffer* (car *cursor*))) (cdr *cursor*))	   
	   (draw-buffer))
    (when (< 0 (car *cursor*))
      (let ((first (car (sref *text-buffer* (1- (car *cursor*)))))
	    (second (car (sref *text-buffer* (car *cursor*)))))

	(setf (car (aref *text-buffer* (1- (car *cursor*))))
	      (sconcat first second))
	
	(sdelete *text-buffer* (car *cursor*))
	(decf (car *cursor*))
	(setf (cdr *cursor*) (length first))
	(draw-buffer)))))

(defun cursor-delete ()
  (if (< (cdr *cursor*) (length (car (sref *text-buffer* (car *cursor*)))))
      (progn (sdelete (car (sref *text-buffer* (car *cursor*))) (cdr *cursor*))
	     (draw-buffer))
      (when (< (car *cursor*) (1- (length *text-buffer*)))
	(setf (car (sref *text-buffer* (car *cursor*)))
	      (sconcat (car (sref *text-buffer* (car *cursor*)))
		       (car (sref *text-buffer* (1+ (car *cursor*))))))
	(sdelete *text-buffer* (1+ (car *cursor*)))
	(draw-buffer))))

(defun cursor-line-feed ()
  (format t "Cursor line feed!~%");
  (multiple-value-bind (first second) (ssplit (car (sref *text-buffer* (car *cursor*)))
					      (cdr *cursor*))
    (setf (car (sref *text-buffer* (car *cursor*)))
	  first)
    (sinsert *text-buffer* (list second) (1+ (car *cursor*)))
    (incf (car *cursor*))
    (setf (cdr *cursor*) 0))
  (draw-buffer))

;; Key press handler
(defevent *on-key-down* (win keysym state ts)
  ;;(format t "~A ~A ~A ~A~%" win (sdl2:scancode keysym) state ts)

  (case (sdl2:scancode keysym)
    (:scancode-backspace 	 (cursor-backspace))
    (:scancode-return    	 (cursor-line-feed))
    (:scancode-delete   	 (cursor-delete))
    (:scancode-home              (cursor-home))
    (:scancode-end              (cursor-end))
    (:scancode-left 	 (cursor-left))
    (:scancode-right 	 (cursor-right))
    (:scancode-up 	 (cursor-up))
    (:scancode-down 	 (cursor-down))))

    ))
    
(defevent *on-text-input* (window char ts)
  (insert-at-cursor (code-char char)))

(! 
  (setf *text-buffer* (make-seq))
  (setf *cursor* '(0 . 0))
  (gl:clear-color 1 1 1 1)
  (sdl2:start-text-input)
  (new-line "")
  (draw-buffer))
