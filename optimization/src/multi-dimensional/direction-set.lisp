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

;;;; Direction set optimization helpers, for: coordinate descent and powell

(defun get-coordinate-directions (n &key (unit-magnitude 1.0d0) feature-mask)
  "Get a list of unit vectors along each of the coordinate directions."
  (let ((directions '()))
    (dotimes (i n)
      (when (or (not feature-mask) (elt feature-mask i))
	(let ((vector (make-array n :element-type 'double-float :initial-element 0.0d0)))
	  (setf (aref vector i) unit-magnitude)
	  (push vector directions))))
    (nreverse directions)))

(defun direction-set-minimization (f directions p &key (linemin-tolerance *default-linemin-tolerance*) (linemin *default-linemin*) (progress-interval 100) (iteration -1) (linemin-lower *default-linemin-lower*) (linemin-upper *default-linemin-upper*) folder verbose)
  "One iteration of minimizations through a set of directions."
  (loop for direction in directions 
     for i from 1 do
       (when (and verbose (= (mod i progress-interval) 0))
	 (format t "Iter# ~4:d  dir# ~4:d~%" iteration i))
       (setf direction (normalize-vector direction))
       (setf p (line-minimization f p direction :tolerance linemin-tolerance :linemin linemin :lower linemin-lower :upper linemin-upper :iteration i :folder folder :verbose verbose)))
  p)

(defun direction-set-minimization-iterative (f directions p &key (tolerance *default-tolerance*) (linemin-tolerance *default-linemin-tolerance*) (linemin *default-linemin*) (max-iter *default-max-iter*)
					     (linemin-lower *default-linemin-lower*) (linemin-upper *default-linemin-upper*) folder verbose)
  "Repeatedly iterate through a set of directions until convergence is reached."
  (let* ((min nil)
	 (prev-min nil))
    (loop for i from 1
       until (or (converged-p min prev-min tolerance) (> i max-iter)) do
	 (when verbose (format-now t "Direction set minimization iteration #~:D~%" i))
	 (setf p (direction-set-minimization f directions p :linemin-tolerance linemin-tolerance :linemin linemin :iteration i
					     :linemin-lower linemin-lower :linemin-upper linemin-upper :folder folder :verbose verbose))
	 (setf prev-min min)
	 (setf min (funcall f p)))
    p))
