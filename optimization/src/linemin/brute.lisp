;;;; Author: Benjamin Lambert (ben@benjaminlambert.com)

(declaim (optimize (debug 3)))
(in-package :optimization)
(cl-user::file-summary "1-dimensional line minimization methods for optimization")

(defun linemin-brute (f ax bx cx &key (max-iterations 200) (tolerance *default-linemin-tolerance*) (interval 5.0d0))
  "Golden section search from Press, et al.
   F is a 1-d function, and ax, bx, and cx are bracketing points."
  (declare (ignore bx max-iterations tolerance))
  (let ((min-f (funcall f 0.0)) ;; on 0.0, or on bx??
	(argmin (list 0.0d0))
	(points '())
	(intervals (bl::intervals interval ax cx)))
    (when (< cx ax)
      (rotatef ax cx))
    (dolist (x intervals)
      ;; Evaluate the function
      (let* ((fx (funcall f x)))
	(push (list x fx) points)                ;; Save the data point for possible graphing later
	;; Check if we've found a new minimum
	(cond ((< fx min-f)                      ;; if it's strictly less than the best, then it's our new best
	       (setf min-f fx)
	       (setf argmin (list x)))
	      ((= fx min-f)                      ;; if it's just as good as the best, save the value for x
	       (push x argmin)))))
    (if (= (length argmin) 1)
	(setf argmin (first argmin)) ;; Save the minimum we found, 
	(setf argmin (blambert-util::list-average argmin))) ;; or average the several minima we found
    (values argmin min-f (length intervals) points)))

