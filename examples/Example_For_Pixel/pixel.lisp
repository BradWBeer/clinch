(ql:quickload :clinch)
(ql:quickload :clinch-cairo)
(ql:quickload :clinch-pango) 
(ql:quickload :clinch-freeimage)

(use-package :clinch)

(defconstant +max-axis+ 32767)
(defparameter +axis-dead-zone+ .40)
(defparameter *controller-vectors* (make-hash-table))

(defparameter *weasle* nil)
(defparameter *weasle-node* nil)
(defparameter *weasle-max-velocity* 3)

(defparameter *key-watch-list*
  '(:scancode-left
    :scancode-right
    :scancode-up
    :scancode-down
    :scancode-space))

(defparameter *key-state* nil)

(defun add-to-key-state (scancode)
  (unless (member scancode *key-state*)
    (push scancode *key-state*)))

(defun remove-from-key-state (scancode)
  (when (member scancode *key-state*)
    (setf *key-state* (delete scancode *key-state*))))

(defun key-down? (scancode)
  (member scancode *key-state*))


;; (defun add-key-to-watchlist (key)
;;(print (sdl2:scancode keysym))  
;;   (sdl2:scancode-symbol 79)


;; Next runs one time before the next on-idle.
(clinch:defevent clinch:*next* ()

  ;; Enable a few opengl features. 
  ;; (gl:enable :blend :depth-test :line-smooth :point-smooth :texture-2d :cull-face)

  ;; ;; Set the blending mode. 
  ;; (%gl:blend-func :src-alpha :one-minus-src-alpha)
  ;; (gl:polygon-mode :front-and-back :fill)

  (gl:clear-color .5 .5 .5 0)

  (setf *weasle-node* (make-instance 'node))
  (setf *weasle*
	(make-quad-for-image "/home/warweasle/work/lisp/weasle.png"
			     :height 180
			     :parent *weasle-node*))

  (add-child *root* *weasle-node*))

(defun move-weasle ()
  (let ((up (key-down? :scancode-up))
	(down (key-down? :scancode-down))
	(left (key-down? :scancode-left))
	(right (key-down? :scancode-right)))
    (if (or up down left right)
	(translate *weasle-node*
		   (v3:*s
		    (v3:+ (calculate-joysticks)
			  (v3:normalize 
			   (v3:+ (if up    (v!  0  1 0) (v! 0 0 0))
				 (if down  (v!  0 -1 0) (v! 0 0 0))
				 (if left  (v! -1  0 0) (v! 0 0 0))
				 (if right (v!  1  0 0) (v! 0 0 0)))))
		    (float *weasle-max-velocity*)))
	(translate *weasle-node*
		   (v3:*s (calculate-joysticks) (float *weasle-max-velocity*))))))
     
;; render

(clinch:defevent clinch:*on-idle* ()

  (move-weasle)
  

  (gl:clear :color-buffer-bit :depth-buffer-bit)
  ;; (test-step (nth 0 *animations*) *ticks*)
  ;; (gl:clear :color-buffer-bit :depth-buffer-bit)
  ;; ;;(clinch:render entity :projection *projection*)
  ;; (clinch:render *node* :projection *projection*)
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  (clinch:render *root* :projection *ortho-projection*))

(defevent *on-key-down* (win keysym state ts)
  (add-to-key-state (sdl2:scancode keysym)))

(defevent *on-key-up* (win keysym state ts)
  (remove-from-key-state (sdl2:scancode keysym)))


;; on window resize
(clinch:defevent clinch:*on-window-resized* (win width height ts)
  ;;(format t "Resized: ~A ~A~%" width height)
  
  ;; (setf *projection* (clinch::make-perspective-transform (clinch:degrees->radians 45)
  ;; 							 (/ width height) .1 1000))
  )


;; zoom in and out
(clinch:defevent clinch:*on-mouse-wheel-move* (win mouse x y ts)
  (format t "win=~A mouse=~A x=~A y=~A ts=~A~%" win mouse x y ts)
  
  (!S *root* (+ 1 (/ y 10)) (+ 1 (/ y 10)) 1))

(defun calculate-joysticks ()
  (let ((sum (v! 0 0 0)))
    (maphash (lambda (k v)
	       (when v
		 (setf sum
		       (v3:+ sum v))))
	     *controller-vectors*)
    (if (> (v3:length-squared sum) 1)
	(v3:normalize sum)
	sum)))

;; 2 is right-Y and 3 is right-X
(defevent *on-controller-axis-move* (controller-id axis-id position timestamp)

  (let ((pos (float (/ position +max-axis+))))
    (if (> (abs pos) +axis-dead-zone+)
	;;(format t "controller=~A axis=~A pos=~A~%" controller-id axis-id 
	(setf (gethash (+ (* controller-id 100) axis-id) *controller-vectors*)
	      (if (evenp axis-id)
		  (v! pos 0 0)
		  (v! 0 (- pos) 0)))
	(setf (gethash (+ (* controller-id 100) axis-id) *controller-vectors*)
	      nil))))


  ;; (when (zerop controller-id)
  ;;   (let ((pos (float (/ position +max-axis+))))
  ;;     (case axis-id

  ;; 	(5 nil)
  ;; 	(4 nil)
  ;; 	(3 (setf (aref +joystick-1+ 1) (- pos)))
  ;; 	(2 (setf (aref +joystick-1+ 0) pos))
  ;; 	(1 (setf (aref +joystick-2+ 1) (- pos)))
  ;; 	(0 (setf (aref +joystick-2+ 0) pos))
  ;; 	(t (format t "axis: ~A ~A~%" axis-id pos))))))

;; (defevent *on-controller-button-down* (controller-id button ts)
;;   (when (= button 10)
;;     (let ((anna (entity-value :anna :body))
;; 	  (ring (entity-value :ring :body)))

;;       (setf (squirl:body-position ring) (squirl:body-position anna))
;;       (setf (squirl:body-velocity ring)
;; 	    (+
;; 	     (squirl:body-velocity (entity-value :anna :body))
;; 	     (* 1500 (v->complex (v2:normalize (entity-value :anna :direction)))))))))



(init)
