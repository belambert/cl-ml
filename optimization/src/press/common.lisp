;;;; Author: Ben Lambert
;;;; ben@benjaminlambert.com

(in-package :optimization)

(defun get-unit-vector-matrix (n)
  "Get the unit identity matrix of size n x n."
  (let ((matrix (make-array (list n n) :element-type 'double-float :initial-element 0.0d0)))
    (dotimes (i n)
      (setf (aref matrix i i) 1.0d0))
    matrix))


