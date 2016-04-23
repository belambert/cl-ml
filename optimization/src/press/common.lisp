;;;; Author: Benjamin E. Lambert (ben@benjaminlambert.com)

(declaim (optimize (debug 3)))
(in-package :optimization)
(cl-user::file-summary "Function mostly copied from Press, et al. These may be officially copy-righted meaning we can't use them as-is (for commercial purposes?) (Only has POWELL)")

(defun get-unit-vector-matrix (n)
  "Get the unit identity matrix of size n x n."
  (let ((matrix (make-array (list n n) :element-type 'double-float :initial-element 0.0d0)))
    (dotimes (i n)
      (setf (aref matrix i i) 1.0d0))
    matrix))


