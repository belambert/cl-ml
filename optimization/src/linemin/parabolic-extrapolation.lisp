;;;; Author: Benjamin Lambert (ben@benjaminlambert.com)

(declaim (optimize (debug 3)))
(in-package :optimization)
(cl-user::file-summary "1-dimensional line minimization methods for optimization")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Parabolic extrapolation ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl-user::section "Parabolic extrapolation")

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
