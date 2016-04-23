;;;; Author: Ben Lambert
;;;; ben@benjaminlambert.com

(in-package :optimization)

(defun coordinate-descent (f n &key x (tolerance *default-tolerance*) 
				 (linemin-tolerance *default-linemin-tolerance*)
				 (linemin *default-linemin*)
				 (linemin-lower *default-linemin-lower*)
				 (linemin-upper *default-linemin-upper*)
				 (max-iter *default-max-iter*) feature-mask folder verbose)
  "Minimize the given function by performing optimizations along each of the n dimensions.
   x is the starting point-- which defaults to the origin."
  (with-optimization
    (ensure-origin x n)
    (setf f (wrap-function-with-counter f))
    (let* ((directions (get-coordinate-directions n :feature-mask feature-mask))
	   (min (direction-set-minimization-iterative f directions x :tolerance tolerance
						      :linemin-tolerance linemin-tolerance :linemin linemin :max-iter max-iter :linemin-lower linemin-lower :linemin-upper linemin-upper :folder folder
						      :verbose verbose)))
      min)))
