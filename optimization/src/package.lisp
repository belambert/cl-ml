;;;; Author: Benjamin Lambert (ben@benjaminlambert.com)

(defpackage :optimization
  (:use :common-lisp :alexandria :split-sequence :blambert-util)
  (:export :line-minimization-1d
	   :*default-linemin-tolerance*
	   :coordinate-descent
	   :steepest-descent
	   :conjugate-gradient-descent
	   :bfgs
	   :powell
	   :square))




