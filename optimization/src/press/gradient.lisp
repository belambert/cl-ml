;;;; Author: Benjamin E. Lambert (ben@benjaminlambert.com)

(declaim (optimize (debug 3)))
(in-package :optimization)
(cl-user::file-summary "Function mostly copied from Press, et al. These may be officially copy-righted meaning we can't use them as-is (for commercial purposes?) (Only has POWELL)")

(defun finite-difference-gradient (f x &key (epsilon *default-gradient-epsilon*))
  "This one is from Press, et al.  Don't use it.
   Epsilon wants to be a fractional value here... *not* what we want...?
   Compute the gradient of the function at point X, using finite differences.
   This requires n function evaluations."
  (let* ((n (length x))
	 (f-x (funcall f x))
	 (xh (copy-seq x))
	 (gradient (make-array n :element-type 'double-float :initial-element 0.0d0)))
    (loop for j from 0 below n
       for xj = (aref x j) ;; this is "temp"
       for run = (* epsilon (abs xj)) do ;; this is "h"
	 (when (= run 0.0d0)
	   (setf run epsilon))
	 (setf run (coerce run 'double-float))
	 (setf (aref xh j) (+ xj run))
	 (let ((f-h (funcall f xh)))
	   (setf (aref xh j) xj) ;; reset xh[j] to it's original value, for the next iteration
	   (setf (aref gradient j) (/ (- f-h f-x) run)))) ;; but save the rise over run into the gradient
    (report-gradient gradient epsilon)
    gradient))
