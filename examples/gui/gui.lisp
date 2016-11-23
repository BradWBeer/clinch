;; This is working file as I test features...please don't use. Use tutorial05 instead.

(ql:quickload :clinch)
(ql:quickload :clinch-cairo)
(ql:quickload :clinch-pango)
(ql:quickload :clinch-freeimage)
(use-package :clinch)

(defparameter *node* nil)
(defparameter *projection* nil)
(defparameter *q* nil)
(defparameter *buttons* nil)


(defun init-test ()
  (setf *node* (make-instance 'clinch:node :parent nil))
  (clinch:translate *node* (clinch:v! 0 0 -2))
  ;; (setf *q* (make-quad-and-texture 200 200))
  ;; (add-child *node* *q*)

  ;; Enable a few opengl features.
  (gl:enable :blend :depth-test :line-smooth :point-smooth :texture-2d :cull-face)

  ;; Set the blending mode.
  (%gl:blend-func :src-alpha :one-minus-src-alpha)
  (gl:polygon-mode :front-and-back :fill)
  (gl:clear-color .8 .8 .8 0))

;; Next runs one time before the next on-idle.
(clinch:defevent clinch:*next* ()

  ;; Enable a few opengl features.
  (gl:enable :blend :depth-test :line-smooth :point-smooth :texture-2d :cull-face)

  ;; Set the blending mode.
  (%gl:blend-func :src-alpha :one-minus-src-alpha)
  (gl:polygon-mode :front-and-back :fill)

  (gl:clear-color 0 0 1 0)

  ;; run init once
  (init-test))

(clinch:defevent clinch:*on-idle* ()

  (gl:clear :color-buffer-bit :depth-buffer-bit)
  ;;(clinch:render entity :projection *projection*)
  (clinch:render *node* :projection *projection*)

  )

(clinch:defevent clinch:*on-mouse-down* (win mouse x y button state clicks ts)

  (format t "win: ~A mouse: ~A x: ~A y: ~A button: ~A state: ~A clicks: ~A ts: ~A~%" win mouse x y button state clicks ts)
  (loop for n in *buttons*
     do (print (check-intersect (car (children n)) n x y *viewport* *projection*))))

;; Rotate and translate with mouse
(clinch:defevent clinch:*on-mouse-move* (win mouse state x y xrel yrel ts)

  ;; (case state
  ;;   ;; (1 (clinch:rotate *node*
  ;;   ;;                     (q:from-fixed-angles (clinch:degrees->radians yrel) (clinch:degrees->radians xrel) 0)))

  ;;   (2 (clinch:translate *node* (clinch:v! (/ xrel 16) (/ yrel -16) 0)))))
  )

;; on window resize
(clinch:defevent clinch:*on-window-resized* (win width height ts)
  (format t "Resized: ~A ~A~%" width height)

  (setf *projection* (clinch::make-perspective-transform (clinch:degrees->radians 45)
                                                         (/ width height) .1 1000)))

(clinch:defevent clinch:*on-key-down* (win keysym state ts)
  )


(clinch:defevent clinch:*text-editing* (win text ts)
  (format t "text editing: ~A ~A ~A~%" win text ts))

(clinch:defevent clinch:*on-text-input* (win text ts)
  (format t "on-text-event: ~A ~A ~A ~%" win (code-char text) ts))


;; zoom in and out
(clinch:defevent clinch:*on-mouse-wheel-move* (win mouse x y ts)
  ;;(format t "win=~A mouse=~A x=~A y=~A ts=~A~%" win mouse x y ts)

  (clinch:translate *node* (clinch:v! 0 0 (/ y .1))))

(clinch:init :asynchronous t :init-controllers nil)

(defun check-intersect (quad node x y viewport projection)
  (multiple-value-bind (origin ray) (unproject x y (width viewport) (height viewport) (m4:inverse projection))
    (clinch::ray-triangles-intersect (clinch::transform-points
                                      (pullg (attribute quad "v"))
                                      (transform *node*))
                                     (pullg (indexes *q*))
                                     origin
                                     ray)))

(defun add-quad (width height)
  (let ((node (make-instance 'node)))
    (add-child node (make-quad-and-texture width height))
    (push node *buttons*)
    (add-child *node* node)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun draw-button (quad text &key
                                (width 20)
                                (line '(0 0 0 0))
                                (fill '(0 0 0 0))
                                (background '(0 0 0 0))
                                (foreground '(.75 .75 .75 1)))
  (with-context-for-mapped-texture (:texture (uniform quad "t1") :width-var w :height-var h)
    (let* ((x 25.6)
           (y 25.6)
           (line-width 20)
           (lw/2 (/ line-width 2))
           (aspect 1.0)
           (corner-radius (/ h 5))
           (radius (/ corner-radius aspect))
           (degrees (/ +pi+ 180)))

      (apply #'clear-cairo-context background)

      (cairo:new-path)
      (cairo:arc (+ radius line-width)
                 (+ radius line-width)
                 radius
                 (degrees->radians 180)
                 (degrees->radians -90))
      (cairo:arc (- w (+ radius line-width))
                 (+ radius line-width)
                 radius
                 (degrees->radians 270)
                 (degrees->radians 0))

      (cairo:arc (- w (+ radius line-width))
                 (- h (+ radius line-width))
                 radius
                 (degrees->radians 0)
                 (degrees->radians 90))

      (cairo:arc (+ radius line-width)
                 (- h (+ radius line-width))
                 radius
                 (degrees->radians 90)
                 (degrees->radians 180))
      (cairo:close-path)

      (apply #'cairo:set-source-rgba fill)
      (cairo:fill-preserve)

      (apply #'cairo:set-source-rgba line)
      (cairo:set-line-width width)



      (cairo:stroke)

      (apply #'cairo:set-source-rgba foreground)
      (cairo:move-to 0 (/ h 3))
      (cairo:move-to 0 0)
      (position-text `("span" (("font_desc" "Mono 70")); "Century Schoolbook L Roman bold 50"))
                              ,text)
                     :width w
                     :height h
                     :%y .4
                     :alignment :pango_align_center))))

(defun position-text (text &key (width nil) (height nil) (wrap :pango_wrap_word) (alignment :PANGO_ALIGN_CENTER) (%x .5) (%y .5))
  (pango::print-with-markup (text :width width :wrap wrap :alignment alignment)
    (multiple-value-bind (xt yt wt ht) (pango:get-layout-extents)
      (let* ((x-end (- width wt))
             (y-start yt)
             (y-end (- height ht))

             (x-pos (+ (- xt)
                       (* %x x-end)))

             (y-pos (+ (* %y
                          (- y-end y-start))
                       y-start)))
        (cairo:move-to x-pos y-pos)))))
