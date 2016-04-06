;;;; window.lisp
;;;; Please see the licence.txt for the CLinch 

(in-package #:clinch)

;;;; window.lisp
(defparameter *window* nil)
(defparameter *context* nil)

(defparameter *inited* nil)
(defparameter *running* nil)

(defparameter *fbo* nil)
(defparameter *viewport* nil)

(defmacro defevent (event args &body body)
  "Creates and updates an event handler."
  `(setf ,event (lambda ,args
		  ,@body)))

(defparameter *controllers* nil
  "An alist of discovered controllers. Format: (controller-id . sdl-controller-handle)")
(defparameter *haptic* nil
  "An alist of discovered haptic feedback devices. Format: (controller-id . sdl-haptic-device-handle)")

(defparameter *next* nil
  "Runs before the next on-idle call. No arguments.")
(defparameter *on-window-size-changed* nil
  "Always called when window size changes. Arguments (window width height timestamp)")
(defparameter *on-window-resized* nil
  "Always called when window size changes. Arguments (window width height timestamp)")
(defparameter *on-window-hidden* nil
  "Called when window is hidden. Arguments (window timestamp)")
(defparameter *on-window-exposed* nil
  "Called when window is exposed and need redrawn. Arguments (window timestamp)")
(defparameter *on-window-moved* nil
  "Called when the window has been moved. Arguments (window x y timestamp)")
(defparameter *on-window-minimized* nil
  "Called when window is minimized. Arguments (window timestamp)")
(defparameter *on-window-maximized* nil
  "Called when window is maximized. Arguments (window timestamp)")
(defparameter *on-window-restored* nil
  "Called when window is restored to normal position and size. Arguments (window timestamp)")
(defparameter *on-window-enter* nil
  "Called when window gains mouse focus. Arguments (window timestamp)")
(defparameter *on-window-leave* nil
  "Called when window is loses mouse focus. Arguments (window timestamp)")
(defparameter *on-window-focus-gained* nil
  "Called when window gains focus. Arguments (window timestamp)")
(defparameter *on-window-focus-lost* nil
  "Called when window loses focus. Arguments (window timestamp)")
(defparameter *on-window-close* nil
  "Called when window is closing. Arguments (window timestamp)")

(defparameter *on-key-down* nil
  "Called when a key is pressed. Arguments (win keysym state ts)")
(defparameter *on-key-up* nil
  "Called when a key is released. Arguments (win keysym state ts)")

(defparameter *on-mouse-move* nil
  "Called when mouse is moved. Arguments (win mouse state x y xrel yrel ts)")
(defparameter *on-mouse-down* nil
  "Called when mouse button is pressed. Arguments: (win mouse x y button state clicks ts)")
(defparameter *on-mouse-up* nil
  "Called when mouse button is released. Arguments: (win mouse x y button state clicks ts)")
(defparameter *on-mouse-click* nil
  "Called when mouse button is released. Arguments: (win mouse x y button state clicks ts)") ;; This isn't correct. !!!
(defparameter *on-mouse-double-click* nil)
(defparameter *on-mouse-wheel-move* nil
  "Called when the mouse wheel is moved. Arguments: (win mouse x y ts)")

(defparameter *on-controller-button-down* nil
  "Called when a controller's button is pressed. Arguments: (controller-id axis-id value timestamp)")
(defparameter *on-controller-button-up* nil
  "Called when a controller's button is released. Arguments: (controller-id axis-id value timestamp)")
(defparameter *on-controller-added* nil
  "Called when a new controller is discovered. Arguments: (controller-id axis-id value timestamp)")
(defparameter *on-controller-removed* nil
  "Called when a controller is removed. Arguments: (window data1 data2 timestamp)")
(defparameter *on-controller-remapped* nil
  "Called when a controller is remapped. Arguments: (window data1 data2 timestamp)")
(defparameter *on-controller-axis-move* nil
  "Called when a controller's axis moves. Arguments (controller-id axis-id position timestamp)")

(defparameter *on-idle* nil
  "Called when there are no pending events. Take no arguments.")
(defparameter *on-quit* nil
  "Called when clinch is about to exit. Take no arguments.")

(defmacro with-main (&body body)
  "Enables REPL access via UPDATE-SWANK in the main loop using SDL2. Wrap this around
the sdl2:with-init code."
  ;;TODO: understand this. Without this wrapping the sdl:with-init the sdl thread
  ;; is an "Anonymous thread" (tested using sb-thread:*current-thread*), while applying
  ;; this makes *current-thread* the same as the one one when queried directly from the
  ;; REPL thread: #<SB-THREAD:THREAD "repl-thread" RUNNING {adress...}>
  `(sdl2:make-this-thread-main
    (lambda ()
      ;; does work on linux+sbcl without the following line:
      #+sbcl (sb-int:with-float-traps-masked (:invalid) ,@body)
      #-sbcl ,@body)))

(defmacro continuable (&body body)
  "Helper macro that we can use to allow us to continue from an
error. Remember to hit C in slime or pick the restart so errors don't kill the app."
  `(restart-case (progn ,@body) (continue () :report "Continue")))

(defun update-swank ()
  "Called from within the main loop, this keep the lisp repl
working while cepl runs"
  (continuable
   (let ((connection (or swank::*emacs-connection* (swank::default-connection))))
     (when connection
       (swank::handle-requests connection t)))))

(defmacro fire (loc &rest args)
  `(continuable
    (and ,loc (funcall ,loc ,@args))))

(defun ensure-cepl-compatible-setup ()
  (unless (>= (gl:major-version) 3)
    (error "Clinch requires OpenGL 3.1 or higher. Found: ~a.~a"
	   (gl:major-version) (gl:minor-version))))

(defun set-default-gl-options ()
  (print "Setting default options")
  (gl:clear-color 0.0 0.0 0.0 0.0)
  (gl:enable :cull-face)
  (gl:cull-face :back)
  (gl:front-face :ccw)
  (gl:enable :depth-test)
  (gl:depth-mask :true)
  (gl:depth-func :less)
  (gl:depth-range 0.0 1.0)
  (gl:enable :depth-clamp))


(defun init-controllers ()
  (setf *controllers* nil
	*haptic* nil)

  (format t "Opening game controllers. ~A ~%"
	  (sdl2-ffi.functions::sdl-game-controller-add-mappings-from-rw
	   (sdl2::sdl-rw-from-file 
	    (concatenate 'string 
			 (directory-namestring
			  (asdf:system-relative-pathname :clinch "clinch.asd"))
			 "SDL_GameControllerDB/gamecontrollerdb.txt")
	    "rw") 1))
  (finish-output)
  ;; open any game controllers

  (loop for i from 0 upto (- (sdl2:joystick-count) 1)
     do (when (sdl2:game-controller-p i)
	  (format t "Found gamecontroller: ~a~%"
		  (sdl2:game-controller-name-for-index i))
	  
	  (let* ((gc (sdl2:game-controller-open i))
		 (joy (sdl2:game-controller-get-joystick gc)))
	    (setf *controllers* (acons i gc *controllers*))
	    (when (sdl2:joystick-is-haptic-p joy)
	      (let ((h (sdl2:haptic-open-from-joystick joy)))
		(setf *haptic* (acons i h *haptic*))
		(sdl2:rumble-init h))))))
  
  (format t "Controlers found: ~A~%" *controllers*))

(defun uninit-controllers ()
  (format t "Closing opened game controllers.~%")
  (finish-output)
  ;; close any game controllers that were opened
  ;; as well as any haptics
  (loop for (i . controller) in *controllers*
     do (progn
	  (format t "sdl2:haptic-close~%")
	  (sdl2:haptic-close (cdr (assoc i *haptic*)))
	  
	  (format t "sdl2:game-controller-close~%")
	  (sdl2:game-controller-close controller))))

(defun main-loop (win gl-context w h &optional asynchronous)
  ;;(declare (optimize (speed 3)))

  (setf *viewport* (make-instance 'viewport :x 0 :y 0 :width w :height h))
			  
  (fire *next*)
  (setf *next* nil)

  (fire *on-window-size-changed* win w h nil)
  (fire *on-window-resized* win w h nil)

  (sdl2:with-event-loop (:method :poll)
    
    (:keydown
     (:window-id win :state state :keysym keysym :timestamp ts)
     (fire *on-key-down* win keysym state ts))

    (:keyup
     (:window-id win :state state :keysym keysym :timestamp ts)
     (fire *on-key-up* win keysym state ts))
    
    (:controlleraxismotion
     (:which controller-id :axis axis-id :value value :timestamp ts)
     (fire *on-controller-axis-move* controller-id axis-id value ts))
    
    (:controllerbuttondown
     (:which controller-id :button button :timestamp ts)
     (fire *on-controller-button-down* controller-id button ts))

    (:controllerbuttonup
     (:which controller-id :button button :timestamp ts)
     (fire *on-controller-button-up* controller-id button ts))

    (:controlleradded 
     (:window-id win :data1 d1 :data2 d2 :timestamp ts)
     (fire *on-controller-added* win d1 d2 ts))
    
    (:controllerremoved
     (:window-id win :data1 d1 :data2 d2 :timestamp ts)
     (fire *on-controller-removed* win d1 d2 ts))

    (:controllerremapped
     (:window-id win :data1 d1 :data2 d2 :timestamp ts)
     (fire *on-controller-remapped* win d1 d2 ts))
    
    (:mousemotion
     (:window-id win :which mouse :state state :x x :y y :xrel xrel :yrel yrel :timestamp ts)
     (declare (ignore x))
     (fire *on-mouse-move* win mouse state x y xrel yrel ts))

    (:mousebuttondown
     (:window-id win :which mouse :x x :y y :button button :state state :clicks clicks :timestamp ts)
     (declare (ignore x))
     (fire *on-mouse-down* win mouse x y button state clicks ts))

    (:mousebuttonup
     (:window-id win :which mouse :x x :y y :button button :state state :clicks clicks :timestamp ts)
     (declare (ignore x))
     (fire *on-mouse-up* win mouse x y button state clicks ts))

    (:mouseclick
     (:window-id win :data1 d1 :data2 d2 :timestamp ts)
     (fire *on-mouse-click* win d1 d2 ts)) ;;; FIX THIS!!!

    (:mousedoubleclick
     (:window-id win :data1 d1 :data2 d2 :timestamp ts)
     (fire *on-mouse-double-click* win d1 d2 ts)) ;;; FIX THIS!!!

    (:mousewheel
     (:window-id win :which mouse :x x :y y :timestamp ts) 
     (declare (ignore x))
     (fire *on-mouse-wheel-move* win mouse x y ts))
    
    (:windowevent
     (:event raw-event :window-id win :data1 d1 :data2 d2 :timestamp ts)
     (let ((event (autowrap:enum-key 'sdl2-ffi:sdl-window-event-id raw-event)))
       (cond
	 ((eql event :size-changed)
	  (quick-set *viewport* 0 0 d1 d2)
	  (fire *on-window-size-changed* win d1 d2 ts))
	 ((eql event :resized)
	  (quick-set *viewport* 0 0 d1 d2)
	  (fire *on-window-resized* win d1 d2 ts))
	 ((eql event :hidden) (fire *on-window-hidden* win ts))
	 ((eql event :exposed) (fire *on-window-exposed* win ts))
	 ((eql event :moved) (fire *on-window-moved* win d1 d2 ts))
	 ((eql event :minimized) (fire *on-window-minimized* win ts))
	 ((eql event :maximized) (fire *on-window-maximized* win ts))
	 ((eql event :restored) (fire *on-window-restored* win ts))
	 ((eql event :enter) (fire *on-window-enter* win ts))
	 ((eql event :leave) (fire *on-window-leave* win ts))
	 ((eql event :focus-gained) (fire *on-window-focus-gained* win ts))
	 ((eql event :focus-lost) (fire *on-window-focus-lost* win ts))
	 ((eql event :close) (fire *on-window-close* win ts)
	  (print "Done...")
	  ))))
    
    (:idle ()  
	   (if *running*
	       (progn
		 (fire *next*)
		 (setf *next* nil)
		 (fire *on-idle*))
	       (sdl2:push-event :quit))
	   (gl:flush)
	   (sdl2:gl-swap-window win)

	   ;; Doesn this make any sense here?
	   (unless asynchronous (update-swank))
	   )
    
    (:quit ()
	   (fire *on-quit*)
	   t)))


(defun init (&key
	       (asynchronous t)
	       (init-controllers t)
	       (width 800)
	       (height 600)
	       (title "Clinch Program")
	       (fullscreen nil)
	       (no-frame nil)
	       (alpha-size 8)
	       (depth-size 32)
	       (stencil-size 32)
	       (red-size 8)
	       (green-size 8)
	       (blue-size 8)
	       (buffer-size 0)
	       (double-buffer t)
	       (hidden nil)
	       (resizable t))
  "Creates Clinch's window in it's own thread. 
 Use ! (wait and return a value from main thread) or 
 Use !! (return immediately with a nil."
  (if asynchronous 
      (bordeaux-threads:make-thread
       (lambda ()
	 (_init :asynchronous asynchronous
		:init-controllers init-controllers
		:width width 
		:height height 
		:title title 
		:fullscreen fullscreen
		:no-frame no-frame
		:alpha-size alpha-size
		:depth-size depth-size 
		:stencil-size stencil-size 
		:red-size red-size 
		:green-size green-size 
		:blue-size blue-size 
		:buffer-size buffer-size
		:double-buffer double-buffer
		:hidden hidden 
		:resizable resizable))
       :name "Main Clinch Thread"
       :initial-bindings
       (cons (cons '*standard-output* *standard-output* )
	     (cons (cons '*standard-input* *standard-input*)
		   bordeaux-threads:*default-special-bindings*)))
      (_init :asynchronous asynchronous
	     :init-controllers init-controllers
	     :width width 
	     :height height 
	     :title title 
	     :fullscreen fullscreen
	     :no-frame no-frame
	     :alpha-size alpha-size
	     :depth-size depth-size 
	     :stencil-size stencil-size 
	     :red-size red-size 
	     :green-size green-size 
	     :blue-size blue-size 
	     :buffer-size buffer-size
	     :double-buffer double-buffer
	     :hidden hidden 
	     :resizable resizable)))

(defun _init (&key
		(asynchronous t)
		(init-controllers t)
		(width 800)
		(height 600)
		(title "Clinch")
		(fullscreen nil)
		(no-frame nil)
		(alpha-size 8)
		(depth-size 32)
		(stencil-size 32)
		(red-size 8)
		(green-size 8)
		(blue-size 8)
		(buffer-size 0)
		(double-buffer t)
		(hidden nil)
		(resizable t))

  (unless *running* 
    (let ((local-stdout *standard-output*)
	  (local-input *standard-input*))
      
      (with-main

	  (unless *inited*
	    (sdl2:with-init (:everything)
	      
	      (let ((*standard-output* local-stdout)
		    (*standard-input* local-input))
		
		(setf *running* t)
		
		(format t "Using SDL Library Version: ~D.~D.~D~%"
			sdl2-ffi:+sdl-major-version+
			sdl2-ffi:+sdl-minor-version+
			sdl2-ffi:+sdl-patchlevel+)
		(finish-output)
		(when init-controllers (init-controllers))

		(sdl2:with-window (win :w width :h height :title title
				       :flags `(:shown :opengl :resizable
						       ,@(remove nil `(:shown :opengl
									      ,(when fullscreen :fullscreen-desktop)
									      ,(when resizable :resizable)
									      ,(when no-frame :borderless)
									      ,(when hidden :hidden)))))

		  (sdl2:gl-set-attr :context-profile-mask 1)
		  (sdl2:gl-set-attr :alpha-size alpha-size)
		  (sdl2:gl-set-attr :depth-size depth-size)
		  (sdl2:gl-set-attr :stencil-size stencil-size)
		  (sdl2:gl-set-attr :red-size red-size)
		  (sdl2:gl-set-attr :green-size green-size)
		  (sdl2:gl-set-attr :blue-size blue-size)
		  (sdl2:gl-set-attr :buffer-size buffer-size)
		  (sdl2:gl-set-attr :doublebuffer (if double-buffer 1 0))

		  (setf *uncollected*  (trivial-garbage:make-weak-hash-table :weakness :key-or-value))
		  (setf *dependents*  (trivial-garbage:make-weak-hash-table :weakness :key-or-value))

		  (sdl2:with-gl-context (gl-context win)

		    (setf *window* win
			  *context* gl-context)	
		    
		    (ensure-cepl-compatible-setup)
		    (set-default-gl-options)

		    ;; basic window/gl setup
		    (format t "Setting up window/gl.~%")
		    (finish-output)
		    (sdl2:gl-make-current win gl-context)
		    (gl:viewport 0 0 width height)
		    (gl:clear :color-buffer)
		    (format t "Beginning main loop.~%")
		    (finish-output)

		    (main-loop win gl-context width height asynchronous)
		    (unload-all-uncollected)
		    (setf *running* nil
			  *inited* nil
			  *generic-single-texture-shader* nil
			  *identity-texture* nil))))))))))

(defun uninit ()
  (with-main
      (setf *running* nil
	    *inited* nil)

      (uninit-controllers) 
    (sdl2:push-event :quit)))

