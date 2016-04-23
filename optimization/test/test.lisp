;;;; Author: Benjamin Lambert (ben@benjaminlambert.com)


(declaim (optimize (debug 3)))
(in-package :optimization-test)
(cl-user::file-summary "Simple numerical optimization")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Test functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl-user::section "Test function(s)")

(defun test-function-a (x)
  "A function that always returns the number 0.  This flat line should be confusing to min/max algorithms,
   since all points along it are both minima and maxima."
  (declare (ignore x))
  0)

(defun test-function-b1 (x)
  "((x - 10) ^ 2).  The minimum should be at +10."
  (expt (- x 10) 2))

(defun test-function-b (x)
  "((x - 10) ^ 2).  The minimum should be at +10."
  (expt (- (elt x  0) 10) 2))

(defun test-function-c (x)
  "((x - 10) ^ 2) + ((y - 10) ^ 2).  The minimum should be at (+10, +10)."
  (+ (expt (- (elt x 0) 10) 2) (expt (- (elt x 1) 10) 2)))

(defun test-function-d (x)
  "((x - 10) ^ 2) + ((y - 10) ^ 2) + ((z - 10) ^ 2).  The minimum should be at (+10, +10, +10)."
  (+ (expt (- (elt x 0) 10) 2) (expt (- (elt x 1) 10) 2) (expt (- (elt x 2) 10) 2)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Running the tests ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl-user::section "Running the tests")

(defparameter *1d-tests* '((test-function-b1 10)))

(defparameter *multidim-tests* '((test-function-b (10))
				 (test-function-c (10 10))
				 (test-function-d (10 10 10))))

;;(defparameter *linemin-methods* '(:brute :golden-section :brent :dbrent))
;;(defparameter *linemin-methods* '(:brute :golden-section :brent))
(defparameter *linemin-methods* '(:golden-section :brent))

(defparameter *multidim-min-functions* '(coordinate-descent steepest-descent conjugate-gradient-descent bfgs powell))

(defun vector-distance (x y)
  (setf x (coerce x 'list))
  (setf y (coerce y 'list))
  (sqrt (reduce '+ (mapcar 'square (mapcar '- x y)))))

(defun run-all-tests ()
  (let ((test-count 0)
	(pass-count 0))
    (format t "********** 1D OPTIMIZATIONS *************~%")
    (dolist (opt-method (remove :dbrent *linemin-methods*))
      (loop for (f reference) in *1d-tests*
    	 for value = (line-minimization-1d f :linemin opt-method) do
    	   (incf test-count)
    	   (if (< (abs (- reference value)) *default-linemin-tolerance*)
    	       (progn (format t "**PASSED** ") (incf pass-count))
    	       (format t "**FAILED** "))
    	   (format t "Method: ~30A Function: ~20A  " opt-method f)
    	   (format t " (Computed: ~f, Expected: ~f)~%" value reference)))
    (format t "~%********** MULTI-D OPTIMIZATIONS *************~%")
    (dolist (opt-function *multidim-min-functions*)
      (dolist (linemin (append *linemin-methods*));; (list :dbrent)))
	(loop for (f reference) in *multidim-tests*
	   for n = (length reference)
	   for value = (funcall opt-function f n :linemin linemin :verbose nil) do
	     (incf test-count)
	     (if (< (vector-distance reference value) *default-linemin-tolerance*)
		 (progn (format t "**PASSED** ") (incf pass-count))
		 (format t "**FAILED** "))
	     (format t "Method: ~30A, Linemin: ~20A Function: ~20A  " opt-function linemin f)
	     (format t " (Computed: (~{~f~^, ~}), Expected: (~{~f~^, ~}))~%" (coerce value 'list) (coerce reference 'list)))))
    (format t "~:D of ~:D tests passed ~,,2f %~%" pass-count test-count (/ pass-count test-count))))

       


