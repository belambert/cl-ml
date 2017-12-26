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

(defun golden-section-search (f ax bx cx &key (max-iterations 200) (tolerance *default-linemin-tolerance*))
  "Golden section search from Press, et al.
   F is a 1-d function, and ax, bx, and cx are bracketing points."
  (let ((x0 ax) (x3 cx)
	x1 x2 f1 f2
	(iterations 0)
	(points '()))
    ;; Make x0 to x1 the smaller segments, and fill in the new point to be tried
    (if (> (abs (- cx bx)) (abs (- bx ax)))
	(progn (setf x1 bx)
	       (setf x2 (+ bx (* +c+ (- cx bx)))))
	(progn (setf x2 bx)
	       (setf x1 (- bx (* +c+ (- bx ax))))))
    (setf f1 (funcall f x1))
    (setf f2 (funcall f x2))
    (push (list x1 f1) points)
    (push (list x2 f2) points)
    (loop while (> (abs (- x3 x0)) (* tolerance (+ (abs x1) (abs x2))))
       for i from 0 below max-iterations do
	 (incf iterations)
	 (if (< f2 f1)
	     (progn (shiftf x0 x1 x2 (+ (* +r+ x2) (* +c+ x3)))
		    (shiftf f1 f2 (funcall f x2))
		    (push (list x2 f2) points))
	     (progn (shiftf x3 x2 x1 (+ (* +r+ x1) (* +c+ x0)))
		    (shiftf f2 f1 (funcall f x1))
		    (push (list x1 f1) points))))
    (if (< f1 f2)
	(values x1 f1 iterations points)
	(values x2 f2 iterations points))))
