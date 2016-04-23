;;;; Author: Benjamin Lambert (ben@benjaminlambert.com)

(declaim (optimize (debug 3)))
(in-package :optimization)
(cl-user::file-summary "Approximating gradient computation, with finite differences")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Defaults ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl-user::section "Defaults")

(defparameter *default-gradient-type* :forward
  "The default method for approximating the gradient with finite differences -- either :forward or :central")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Finite difference derivatives ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl-user::section "Finite difference derivatives")

;; We may want something smarter for the epsilon for functions that aren't completely smooth...

(defun finite-difference-gradient-function (f &key (epsilon *default-gradient-epsilon*) feature-mask (type :forward))
  "Get the function that effectively computes the gradient of the given function.
   This is helpful/necessary for when the gradient can't be computed analytically."
  (case type
    (:forward (lambda (x) (forward-difference-gradient f x :epsilon epsilon :feature-mask feature-mask))) ;; twice as fast...
    (:central (lambda (x) (central-difference-gradient f x :epsilon epsilon :feature-mask feature-mask)))
    (otherwise (error "Unknown gradient type: ~A" type))))

(defun forward-difference-gradient (f x &key (epsilon *default-gradient-epsilon*) feature-mask verbose)
  "Compute the gradient of the function at point X, by evaluting the function at point x,
   and also x + epsilon for each dimension.  Requires about n function evaluations.
   
   This only needs about half the function evaluations as the 'central' version
   b/c each dimension can share the same 'starting' point."
  ;; Make a copy of x since we'll be modifying it
  (when (not (typep x 'sequence))
    (setf x (list x)))
  (setf x (copy-seq x))
  (let* ((n (length x))
	 (fx (funcall f x)) ;; the starting point
	 (gradient (make-array n :element-type 'double-float :initial-element 0.0d0))) ;; a new vector in which to put the gradient
    (when verbose (format t "Approximating gradient at ~:d-dimensional point with forward differences. Effective dimensions: ~:D~%" n (count-if 'identity feature-mask)))
    (loop for j from 0 below n
       for xj = (aref x j)
       for xje = (+ (aref x j) epsilon) do
	 (setf (aref x j) xje)
	 ;; Compute the value of the function
	 (when (or (not feature-mask) (elt feature-mask j))
	   (let ((f-xje (funcall f x)))
	     (setf (aref x j) xj) ;; reset x[j] to it's original value, for the next iteration
	     (setf (aref gradient j) (/ (- f-xje fx) (- xje xj))))))
    (when verbose (report-gradient gradient epsilon :feature-mask feature-mask))
    gradient))

(defun central-difference-gradient (f x &key (epsilon *default-gradient-epsilon*) feature-mask verbose)
  "Compute the gradient of the function at point X, by evaluting the function at point x,
   and also x + epsilon, and x - epsilon for each dimension.  Requires about 2n function evaluations,
   but is more accurate than the forward difference gradient."
  ;; Make a copy of x since we'll be modifying it
  (setf x (copy-seq x))
  (let* ((n (length x))
	 (gradient (make-array n :element-type 'double-float :initial-element 0.0d0)) ;; a new vector in which to put the gradient
	 (fxj+e nil)
	 (fxj-e nil))
    (when verbose (format t "Approximating gradient at ~:d-dimensional point with central differences. Effective dimensions: ~:D~%" n (count-if 'identity feature-mask)))
    (loop for j from 0 below n
       for xj = (aref x j)
       for xj+e = (+ (aref x j) epsilon) 
       for xj-e = (- (aref x j) epsilon) do
	 (when (or (not feature-mask) (elt feature-mask j))
	   (setf (aref x j) xj+e)
	   (setf fxj+e (funcall f x))
	   (setf (aref x j) xj-e)
	   (setf fxj-e (funcall f x))
	   ;; reset x_j
	   (setf (aref x j) xj)
	   ;; compute and save the gradient for this dimension
	   (setf (aref gradient j) (/ (- fxj+e fxj-e) (- xj+e xj-e)))))
    (when verbose (report-gradient gradient epsilon :feature-mask feature-mask))
    gradient))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Finite difference derivatives ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl-user::section "Printing the gradient")

(defun report-gradient (x epsilon &key (print-gradient t) feature-mask)
  "Print a number of statistics about the current gradient."
  (setf x (coerce x 'list))
  (let ((n (length x))
	(zero-count (count-if 'zerop x)))
    (format t "Approximated gradient of ~:d-dimensional vector with epsilon ~f.~%" n epsilon)
    (format t "  Gradient norm (|x|) =    ~f~%" (vector-norm x))
    (format t "  # of δf/δxi = 0.0:       ~:D~%" zero-count)
    (when feature-mask
      (format t "  δf/δxi zeroed by mask:   ~:D~%" (count-if 'identity (coerce feature-mask 'list))))
    (format t "  min(δf/δxi) =             ~f~%" (apply 'min x))
    (format t "  max(δf/δxi) =             ~f~%" (apply 'max x))
    (format t "  avg(δf/δxi) =             ~f~%" (list-average (coerce x 'list)))
    (when print-gradient
      (format t "  ~:D non-zero partial derivatives:~%" (- n zero-count))
      (dotimes (i n)
	(unless (zerop (elt x i))
	  (format t "    i=~6d     δf/δx_i = ~10F~%" i (elt x i)))))
    (format t "*************************************************~%")
    (force-output)))
