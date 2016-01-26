;;;; window.lisp
;;;; Please see the licence.txt for the CLinch 

(in-package #:clinch)

;; (defun init ()
;;   (kit.sdl2:start)
;;   (format t "Opening game controllers. ~A ~%"
;; 	  (sdl2-ffi.functions::sdl-game-controller-add-mappings-from-rw
;; 	   (sdl2::sdl-rw-from-file  (make-local-path "gamecontrollerdb.txt") "rw") 1)))

;; (defun uninit () 
;;   (kit.sdl2:quit))

;; (defclass window (kit.sdl2:gl-window)
;;   ((start-time :initform (get-internal-real-time))
;;    (frames :initform 0)
;;    (FPS :reader FPS)))

;; (defmethod initialize-instance :after ((w window) &key &allow-other-keys)
;;   ;; GL setup can go here; your GL context is automatically active,
;;   ;; and this is done in the main thread.
;;   )

;; (defmethod render :after ((window window) &key)
;;   (with-slots (start-time frames) window
;;     (incf frames)
;;     (let* ((current-time (get-internal-real-time))
;;            (seconds (/ (- current-time start-time) internal-time-units-per-second)))
;;       (when (> seconds 5)
;;         (format t "FPS: ~A~%" (float (setf (slot-value this 'FPS) (/ frames seconds))))
;;         (setf frames 0)
;;         (setf start-time (get-internal-real-time))))))


;; (defmethod close-window ((window window))
;;   (format t "Bye!~%")
;;   ;; To _actually_ destroy the GL context and close the window,
;;   ;; CALL-NEXT-METHOD.  You _may_ not want to do this, if you wish to
;;   ;; prompt the user!
;;   (call-next-method))

;; (defmethod mousewheel-event ((window window) ts x y)
;;   (with-slots (rotation) window
    
;;     ;(render window)
;;     ))

;; (defmethod textinput-event ((window window) ts text)
;;   (format t "You typed: ~S~%" text))

;; (defmethod keyboard-event ((window window) state ts repeat-p keysym)
;;   (let ((scancode (sdl2:scancode keysym)))
;;     (when (sdl2:scancode= scancode :scancode-escape) (close-window window))
;;     (unless repeat-p
;;       (format t "~A ~S ~S~%" state scancode (sdl2:scancode-name scancode)))))

;; (defmethod mousebutton-event ((window window) state ts b x y)
;;   (format t "~A button: ~A at ~A, ~A~%" state b x y))

;; (defmethod mousemotion-event ((window window) ts mask x y xr yr)
;;   (when (> mask 0)
;;     (format t "Mouse motion, button-mask = ~A at ~A, ~A~%" mask x y)))

;; (defmethod controller-added-event ((window window) c)
;;   (format t "Added ~A (id=~A)~%" c (sdl2:game-controller-instance-id c)))

;; (defmethod controller-removed-event ((window window) c)
;;   (format t "Removed ~A (id=~A)~%" c (sdl2:game-controller-instance-id c)))

;; (defmethod controller-axis-motion-event ((window window) c ts axis value)
;;   (format t "ID ~A, Axis ~A, Value ~A~%"
;;           (sdl2:game-controller-instance-id c) axis value))

;; (defmethod controller-button-event ((window window) c state ts button)
;;   (format t "ID ~A, Button ~A, State ~S~%"
;;           (sdl2:game-controller-instance-id c) button state))

;; (defmethod window-event ((window window) type timestamp data1 data2)

;;   )

