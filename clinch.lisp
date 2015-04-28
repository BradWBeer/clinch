(in-package #:clinch)


(defun split-keywords (lst &optional keys objects)
  (cond 
    ((or (null lst)
	 (and (keywordp (first lst))
	      (null (cdr lst))))
     (values keys (reverse objects)))
    
    ((and (first lst) 
	  (second lst)
	  (keywordp (first lst))) 
     (push (second lst) keys)
     (push (first lst) keys)
     (split-keywords (cddr lst) keys objects))
    
    (t 
     (push (first lst) objects) 
     (split-keywords (cdr lst) keys objects))))

(defun transform-tree (tester transformer tree)
  (cond ((consp tree)
	 ;; it's a cons. process the two subtrees.
	 (destructuring-bind (left . right) tree
	   (cons
	    ;; process left subtree.
	    (if (funcall tester left)
		(funcall transformer left)
		;; nothing to transform here. move on down the left side.
		(if (consp left)
		    (transform-tree tester transformer left)
		    left))
	    ;; process right subtree.
	    (transform-tree tester transformer right))))
	;; it's not a cons. test it.
	((funcall tester tree)
	 (funcall transformer tree))
	;; it failed the test. leave it alone.
	(t tree)))


;; (defmacro continuable (&body body)
;;   "Helper macro that we can use to allow us to continue from an
;; error. Remember to hit C in slime or pick the restart so errors don't kill the app."
;;   `(restart-case (progn ,@body) (continue () :report "Continue")))


;; (defun update-swank ()
;;   "Called from within the main loop, this keep the lisp repl
;; working while cepl runs"
;;   (continuable
;;     (let ((connection (or swank::*emacs-connection* (swank::default-connection))))
;;       (when connection
;; 	(swank::handle-requests connection t)))))
