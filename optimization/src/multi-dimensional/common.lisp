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

(defun random-parameters (f n &key x)
  "Return a vector of randomly chosen parameters.  For testing..."
  (declare (ignore f x))
  (let ((param (make-array n :element-type 'double-float :initial-element 0.0d0)))
    (dotimes (i n)
      (setf (elt param i) (- (random 4000.0d0) 2000.0d0)))
    param))

(defun print-vector-history (x-history search-direction)
  "Print a table the depicts the current search direction, and the corresponding 'best' points along those dimensions."
  (let* ((history-length (length x-history))
	 (history-headers (mapcar (lambda (x) (format nil "#~D" x)) (loop for i from history-length downto 1 collecting i))))
    (format t "~6A | ~16A  | p_i   ~{~10@A  ~}~%" "Dim #" "Search vec" history-headers)
    (format t "-----------------------------------------------------------~%")
    (loop for i from 0 below (length (first x-history))
       for history = (mapcar (lambda (x) (elt x i)) x-history) do
	 (when (or (/= (elt search-direction i) 0.0) (/= (first history) 0.0))
	   (format t "~6d | x_i = ~10,3F  | p_i = ~{~10,3F  ~}~%" i  (elt search-direction i) history)))
    (format t "*************************************************~%")))

(defun print-point-history (x-history)
  "Print the search space history (i.e. the history of the 'best' points) for the non-derivative searches."
  (format t "~4A. ~10A ~%" "f#" "x history")
  (loop for i from 0 below (length (first x-history)) do
       (when (/= (elt (first x-history) i) 0.0)
	 (format t "~4d. ~{~15,10f ~}~%" i (mapcar (lambda (x) (elt x i)) x-history)))))
