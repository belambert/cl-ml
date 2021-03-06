;; Copyright 2010-2018 Ben Lambert

;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at

;;     http://www.apache.org/licenses/LICENSE-2.0

;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

(in-package :optimization)

(defun steepest-descent (f n &key x df (gradient-epsilon *default-gradient-epsilon*) (max-iter *default-max-iter*) (tolerance *default-tolerance*) (linemin-tolerance *default-linemin-tolerance*) (linemin *default-linemin*) (linemin-lower *default-linemin-lower*) (linemin-upper *default-linemin-upper*) feature-mask (gradient-type *default-gradient-type*) folder verbose)
  "The allegedly very 'dumb' optimization method.  The gradient function is optional, in which case
   it's computed with finite differnces."
  (with-optimization
    (setf f (wrap-function-with-counter f))
    (ensure-origin x n)
    (ensure-gradient df f gradient-epsilon :feature-mask feature-mask :gradient-type gradient-type)
    (let ((fx-prev nil)
	  (fx (funcall f x))
	  (prev-x nil)
	  (x-history nil))
      (loop for i from 1
	 while (and (not (converged-p fx fx-prev tolerance)) (< i max-iter))
	 for gradient = (funcall df x)
	 for normal-gradient = (normalize-vector gradient)
	 for neg-normal-gradient = (array-scalar* normal-gradient -1)
	 for gradient-norm = (vector-norm gradient) do
	   (when verbose
	     (format-section t "Steepest decent iteration #~:D~%" i)
	     (format t "Gradient norm: ~F~%" gradient-norm))
	   (setf fx-prev fx)
	   (setf prev-x (copy-seq x))
	   (push prev-x x-history)
	   (when verbose (print-vector-history x-history normal-gradient))
	   (setf x (line-minimization f x neg-normal-gradient :tolerance linemin-tolerance :linemin linemin
				      :lower linemin-lower :upper linemin-upper :iteration i :folder folder :verbose verbose))
	   (when verbose (format t "Direction# = ~5:D~%" i))
	   (setf fx (funcall f x))))
    x))
