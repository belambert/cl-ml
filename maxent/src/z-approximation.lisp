;;;; Author: Benjamin E. Lambert (ben@benjaminlambert.com)

(declaim (optimize (debug 3)))
(in-package :maxent)

(defun approximate-z (sentences model)
  "Computes the denominator of P(S) ... but this is fixed for each iteration.
   This is should be the expectation of the non-normalized sentence numerator.
   That is: E_p0 [ exp(Sum_i lambda_i * f_i(s)). ]."
  ;;(declare (optimize (speed 3)))
  (let* ((sum 0.0d0)
	 (samples sentences))
    (declare (double-float sum))
    (dolist (sample samples)
      (let* ((score (cl-lm::score model sample)) ;; This is the R(s) score, from Roni's paper.
	     (sample-numerator (exp score)))
	(declare (double-float sample-numerator))
	(incf sum sample-numerator)))
    (let ((sample-size (length sentences)))
      (setf (normalization-constant model) (/ sum sample-size))
      (format t "Normalization constant: ~f~%" (normalization-constant model)) 
      (normalization-constant model))))
