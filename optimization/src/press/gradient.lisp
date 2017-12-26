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

(defun finite-difference-gradient (f x &key (epsilon *default-gradient-epsilon*))
  "This one is from Press, et al.  Don't use it.
   Epsilon wants to be a fractional value here... *not* what we want...?
   Compute the gradient of the function at point X, using finite differences.
   This requires n function evaluations."
  (let* ((n (length x))
	 (f-x (funcall f x))
	 (xh (copy-seq x))
	 (gradient (make-array n :element-type 'double-float :initial-element 0.0d0)))
    (loop for j from 0 below n
       for xj = (aref x j) ;; this is "temp"
       for run = (* epsilon (abs xj)) do ;; this is "h"
	 (when (= run 0.0d0)
	   (setf run epsilon))
	 (setf run (coerce run 'double-float))
	 (setf (aref xh j) (+ xj run))
	 (let ((f-h (funcall f xh)))
	   (setf (aref xh j) xj) ;; reset xh[j] to it's original value, for the next iteration
	   (setf (aref gradient j) (/ (- f-h f-x) run)))) ;; but save the rise over run into the gradient
    (report-gradient gradient epsilon)
    gradient))
