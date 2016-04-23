;;;; Author Benjamin E. Lambert (ben@benjaminlambert.com)

(declaim (optimize (debug 3)))
(in-package :optimization)
(cl-user::file-summary "Multidimensional numerical optimization")

(defun conjugate-gradient-descent (f n &key x df (gradient-epsilon *default-gradient-epsilon*) (max-iter *default-max-iter*) (tolerance *default-tolerance*) (linemin-tolerance *default-linemin-tolerance*) (linemin *default-linemin*)(linemin-lower *default-linemin-lower*) (linemin-upper *default-linemin-upper*) feature-mask (gradient-type *default-gradient-type*) folder verbose)
  "G-tolerance is the convergence criteria for the zero gradient text."
  (with-optimization
    (setf f (wrap-function-with-counter f))
    (ensure-origin x n)
    (ensure-gradient df f gradient-epsilon :feature-mask feature-mask :gradient-type gradient-type)
    (let* ((p (copy-seq x)) ;; the starting point
	   (g (get-zero-vector n (array-element-type x))) ;; 2 new vectors
	   (h (get-zero-vector n (array-element-type x))) ;; 2 new vectors
	   (fp-prev nil)
	   (fp (funcall f p))     ;; the value of the function at point p
	   (xi (funcall df p)) ;; the gradient at point p
	   (x-history '()))
      ;; Initializations
      (dotimes (j n)
	(setf (aref g j) (- (aref xi j)))
	(setf (aref h j) (- (aref g j))) ;; same number going to two places
	(setf (aref xi j) (aref h j)))
      ;; Loop over iterations...
      (loop for i from 1 to max-iter 
	 for xi-normalized = (normalize-vector xi) do
	   (when verbose (format-section t "Conjugate gradient decent iteration #~:D~%" i))
	 ;; Minimize from point p in the direction of xi
	   (push p x-history)
	   (when verbose (print-vector-history x-history xi))
	 ;; Minimize along the normalized vector, so the lambda is on the [-200, 200]-ish scale
	   (setf p (line-minimization f p xi-normalized :tolerance linemin-tolerance :linemin linemin
				      :lower linemin-lower :upper linemin-upper :iteration i :folder folder :verbose verbose))
	   (when verbose (format t "Direction# = ~5:D~%" i))
	 ;; Evaluate the function at the new point p
	   (setf fp-prev fp)
	   (setf fp (funcall f p))
	   (when (converged-p fp fp-prev tolerance)
	     (return))
	 ;; Compute the gradient at the new point
	   (setf xi (funcall df p))
	   ;; TODO - We could also check for convergence on the gradient here... (see above).
	   ;; This is where we construct the next direction to search in
	   (let ((gg 0.0d0)
		 (dgg 0.0d0))
	     (dotimes (j n)
	       (incf gg (expt (aref g j) 2))
	       ;;(incf dgg (expt (aref xi j) 2))  ;; Fletcher-Reeves
	       (incf dgg (* (+ (aref xi j) (aref g j)) (aref xi j)))) ;; Polak-Ribiere
	     (when (= gg 0.0d0) ;; Unlikely.  If gradient is already zero, then we are already done.
	       (return))
	     (let ((gam (/ dgg gg)))
	       (dotimes (j n)
		 (setf (aref g j) (- (aref xi j)))
		 (setf (aref h j) (+ (aref g j) (* gam (aref h j))))
		 (setf (aref xi j) (aref h j))))))
      p)))

