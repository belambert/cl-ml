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
