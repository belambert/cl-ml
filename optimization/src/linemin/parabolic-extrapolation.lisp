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

(defun parabolic-extrapolation-simple-test (a b c f)
  "Try to do a parabolic extrapolation of function f, at points a, b, and c."
  (let ((fa (funcall f a))
	(fb (funcall f b))
	(fc (funcall f c)))
    (parabolic-extrapolation-simple a b c fa fb fc)))

(defun parabolic-extrapolation-simple (ax bx cx fa fb fc)
  "Given points (ax,fx), (bx,fb), (cx,fc), that are assumed to be points on a
   parabola, compute the abcissa of the parabola's minimal/max.  (i.e.
   the abcissa of the 'focus' point?).
   If the points are co-linear, this will throw a div-by-zero error."
  (let* ((r (* (- bx ax) (- fb fc))) ;; the area of the "lower-left"(?) rectangle
	 (q (* (- bx cx) (- fb fa))) ;; the area of the "upper-right"(?) rectangle
	 (u (- bx (/ (- (* (- bx cx) q) ;; q times it's width
			(* (- bx ax) r))  ;; r times it's with
		     (* 2.0 (- q r))))))  ;; 2x the difference of the two rectangles areas.
    u))

(defun parabolic-extrapolation (ax bx cx fa fb fc)
  "Given points (ax,fx), (bx,fb), (cx,fc), that are assumed to be points on a
   parabola, compute the abcissa of the parabola's minimal/max.  (i.e.
   the abcissa of the 'focus' point?).
   This adds a small amount of noise to prevent a div-by-zero error when
   the points are co-linear."
  (let* ((r (* (- bx ax) (- fb fc)))
	 (q (* (- bx cx) (- fb fa)))
	 (u (- bx (/ (- (* (- bx cx) q)
			(* (- bx ax) r))
		     (* 2.0 
			(sign (max (abs (- q r)) +epsilon+) (- q r))
			)))))
    u))
