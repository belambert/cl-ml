;;;; Author: Ben Lambert (ben@benjaminlambert.com)

(declaim (optimize (debug 3)))
(in-package :optimization)
(cl-user::file-summary "Misc helper functions and parameters for optimization")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Various global parameters ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl-user::section "Various global parameters")

;; We probably want this to be relatively bigger than for other problems?
(defparameter *default-gradient-epsilon* 1.0d-3
  "How far to jump to compute the gradient.")

(defparameter *default-max-iter* 200
  "Default maximum number of iterations to run optimization for.")

(defparameter *default-linemin-tolerance* 3d-8
  "Default convergence tolerance for the line searches.")

(defparameter *default-tolerance* 1.0d-8
  "Tolerance... for all the multi-dimensional optimizers")

;; These may not always be very meaningful... since they will multiply gradient vectors which might have very difference scales
(defparameter *default-linemin-lower* -200.0d0
  "The default lower-bound/starting point for the line searches -- in many of the searches these will be multiplied by the gradient and so scaled much larger or smaller.")

(defparameter *default-linemin-upper* 200.0d0
  "The default upper-bound/starting point for the line searches -- in many of the searches these will be multiplied by the gradient and so scaled much larger or smaller.")

(defparameter *default-linemin* :golden-section
  "Which line minimizer to use by default.  Current options include... :golden-section, :brent, or :dbrent.
   :brent seems to be be the fastest, and :dbrent is likely incomplete.  However, golden section is the most predictable
   for relatively flat search spaces -- it will end up in the middle, rather than 'pulling' to one side.")

(defconstant +epsilon+ 1d-18
  "A very small number used to prevent divide by zero errors.  Used when bracketing a minimum, and checking for convergence.
   However, this number (probably) shouldn't be changed.")

(defconstant +phi+ (/ (+ 1 (sqrt 5)) 2 )
  "Phi - the golden ration -- about 1.61..")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;; Misc ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl-user::section "Misc")

(defun square (x)
  "Simply square the given number, a convenience function to make
   the other code in this file more readable."
  (expt x 2))

(defun get-zero-vector (n &optional (element-type 'double-float))
  "Get a vector at (0.0 0.0 ...) for n dimensions."
  (make-array n :element-type element-type :initial-element (coerce 0 element-type)))

(defmacro ensure-origin (origin n)
  "Make sure we have a starting point"
  `(unless ,origin
    (setf ,origin (get-zero-vector ,n))))

(defmacro ensure-gradient (df f gradient-epsilon &key feature-mask gradient-type)
  "Make sure we have a gradient function"
  `(unless ,df
    (setf ,df (finite-difference-gradient-function ,f :epsilon ,gradient-epsilon :feature-mask ,feature-mask :type ,gradient-type))))

(defvar *function-evaluations* 0
  "Global variable to keep track of how many times we call the function we're optimizing.")

(defun increment-function-evaluations (&key (n 1) (interval 1000))
  "Convenience function that increments the function evaluation counter, and reports the count
   at regular intervals."
  (incf *function-evaluations* n)
  (when (= (mod *function-evaluations* interval) 0)
    (format t "Evaluated function ~:d times.~%" *function-evaluations*)
    (force-output)))

(defun wrap-function-with-counter (f)
  "Return a version f that increments the f that automatically updates the function
   evaluation count each time the function is called."
  (lambda (x)
    (increment-function-evaluations)
    (funcall f x)))

(defmacro with-optimization (&body body)
  "Macro that resets the function evaluation counter at the beginning and prints
   the number of evaluations at the end."
  `(prog2 (setf *function-evaluations* 0)
       (progn ,@body)
     (format-now t "# of function evaluations: ~:d~%" *function-evaluations*)))

(defun print-vector (x &key (verbose t))
  "Print some general information about the given vector, and, when verbose=true,
   also print all the non-zero elements of the vector."
  (setf x (coerce x 'list))
  (let ((n (length x))
	(zero-count (count-if 'zerop x)))
    (format t "~:d-dimensional vector:~%" n)
    (format t "  vector norm (|x|) =   ~A~%" (vector-norm x))
    (format t "  # of xi = 0.0:        ~:D~%" zero-count)
    (format t "  min(xi) =             ~f~%" (apply 'min x))
    (format t "  max(xi) =             ~f~%" (apply 'max x))
    (format t "  avg(xi) =             ~f~%" (list-average (coerce x 'list)))
    (when verbose
      (format t "  ~:D non-zero components:~%" (- n zero-count))
      (dotimes (i n)
	(unless (or (zerop (elt x i)) (nanp (elt x i)))
	  (format t "    i=~6d x_i = ~10F~%" i (elt x i)))))
    (format t "*************************************************~%")
    (force-output)))


