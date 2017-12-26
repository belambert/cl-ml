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

(defun coordinate-descent (f n &key x (tolerance *default-tolerance*) 
				 (linemin-tolerance *default-linemin-tolerance*)
				 (linemin *default-linemin*)
				 (linemin-lower *default-linemin-lower*)
				 (linemin-upper *default-linemin-upper*)
				 (max-iter *default-max-iter*) feature-mask folder verbose)
  "Minimize the given function by performing optimizations along each of the n dimensions.
   x is the starting point-- which defaults to the origin."
  (with-optimization
    (ensure-origin x n)
    (setf f (wrap-function-with-counter f))
    (let* ((directions (get-coordinate-directions n :feature-mask feature-mask))
	   (min (direction-set-minimization-iterative f directions x :tolerance tolerance
						      :linemin-tolerance linemin-tolerance
						      :linemin linemin
						      :max-iter max-iter
						      :linemin-lower linemin-lower
						      :linemin-upper linemin-upper
						      :folder folder
						      :verbose verbose)))
      min)))
