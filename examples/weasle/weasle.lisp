(ql:quickload :clinch)
(ql:quickload :clinch-freeimage)
(ql:quickload :sdl2-mixer)

(use-package :clinch)

;; Sound stuff
(defparameter *current-volume* 128)
(defparameter *sound-effect* nil)

;; Joystick stuff
(defconstant +max-axis+ 32767)
(defparameter +axis-dead-zone+ .40)
(defparameter *controller-vectors* (make-hash-table))

;; Game stuff
(defparameter *weasle* nil)
(defparameter *weasle-node* nil)
(defparameter *weasle-max-velocity* 300) ;; in pixels/second

;; Keyboard handling
(defparameter *key-state* nil)

(defun add-to-key-state (scancode)
  (unless (member scancode *key-state*)
    (push scancode *key-state*)))

(defun remove-from-key-state (scancode)
  (when (member scancode *key-state*)
    (setf *key-state* (delete scancode *key-state*))))

(defun key-down? (scancode)
  (member scancode *key-state*))

;; Like an init function
;; Next runs one time before the next on-idle.
(clinch:defevent clinch:*next* ()

  ;; Set up sdl2-mixer
  (sdl2-mixer:init :ogg)
  (sdl2-mixer:open-audio 22050 :s16sys 1 1024)
  (sdl2-mixer:allocate-channels 1)
  (setf *sound-effect* (sdl2-mixer:load-wav (asdf:system-relative-pathname 'clinch "examples/weasle/sample.ogg")))

  ;; Enable a few opengl features. 
  (gl:enable :blend :depth-test :line-smooth :point-smooth :texture-2d :cull-face)

  ;; Set the blending mode. 
  (%gl:blend-func :src-alpha :one-minus-src-alpha)
  (gl:polygon-mode :front-and-back :fill)

  ;; Only sets the color, doesn't clear anything
  (gl:clear-color .5 .5 .5 0)

  ;; Create a node to position the weasle sprite.
  (setf *weasle-node* (make-instance 'node))

  ;; Load an image, resize it, and connect it to its node.
  (setf *weasle*
	(make-quad-for-image (namestring (asdf:system-relative-pathname 'clinch "examples/weasle/weasle.png"))
			     :height 180
			     :width 129
			     :parent *weasle-node*))

  ;; Add node to root node so it's rendered.
  (add-child *root* *weasle-node*))

;; Logic which combines keyboard and joystick input...it's buggy.
;; The (v3: functions are in rtg-math and deal with 3 value vectors.
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
		    (float (* *delta-ticks* *weasle-max-velocity* 1/1000)))))
	(translate *weasle-node*
		   (v3:*s (calculate-joysticks) (float (* *delta-ticks* *weasle-max-velocity* 1/1000))))))
     
;; Render loop
(clinch:defevent clinch:*on-idle* ()

  (move-weasle)

  (gl:clear :color-buffer-bit :depth-buffer-bit)
  (clinch:render *root* :projection *ortho-projection*))

;; Key press handler
(defevent *on-key-down* (win keysym state ts)
  (add-to-key-state (sdl2:scancode keysym)))

;; Key up handler
(defevent *on-key-up* (win keysym state ts)
  (remove-from-key-state (sdl2:scancode keysym)))

;; Window resize handler
;; I didn't want to change the projection so I do nothing...not default behavior.
(clinch:defevent clinch:*on-window-resized* (win width height ts)
  )

;; zoom in and out
(clinch:defevent clinch:*on-mouse-wheel-move* (win mouse x y ts)

  ;; !S == (scale *weasle-node* (v! ....))
  (!S *weasle-node* (+ 1 (/ y 10)) (+ 1 (/ y 10)) 1))


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

;; Controller axis move handler
;; It seems even joysticks are horizontal so I just went with it. 
(defevent *on-controller-axis-move* (controller-id axis-id position timestamp)
  (let ((pos (float (/ position +max-axis+))))
    (if (> (abs pos) +axis-dead-zone+)
	(setf (gethash (+ (* controller-id 100) axis-id) *controller-vectors*)
	      (if (evenp axis-id)
		  (v! pos 0 0)
		  (v! 0 (- pos) 0)))
	(setf (gethash (+ (* controller-id 100) axis-id) *controller-vectors*)
	      nil))))

;; Button click handler
(defevent *on-controller-button-down* (controller-id button ts)
  (sdl2-mixer:play-channel 0 *sound-effect* 0))

;; Start clinch...
(init)
