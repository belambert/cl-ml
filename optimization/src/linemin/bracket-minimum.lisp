;;;; Author: Benjamin Lambert (ben@benjaminlambert.com)

;;;; Bracketing minima

(declaim (optimize (debug 3)))
(in-package :optimization)
(cl-user::file-summary "1-dimensional line minimization methods for optimization")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Bracketing minima ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl-user::section "Bracketing minima")

(defun bracket-minimum-core (f ax bx &optional c)
  "Return a triplet of points that bracket a minimum of 1-d function f.
   There are a few different variations on this that I could try...
   but I'm not sure if they wil work.
   In particular, changing which three are returned in the two 'return-from' forms,
   and change which point gets 'thrown away' at each of the 'shiftf' statements."
  (let ((fa (funcall f ax))
	(fb (funcall f bx)))
    (when (> fb fa)
      (rotatef ax bx)
      (rotatef fa fb)
      ;; We switched directions, so the guess for c is meaningless...
      (setf c nil))
    ;; After the above rotation, we know that:
    ;; fa > fb
    ;; So, a is the "left" point, b is the center point
    ;; Now, we just have to go far enough in the direction from a to b, to find a 'c'
    ;; such that fc > fb.
    ;; If we're lucky, this first guess will be all we need to do.
    (let* ((cx (if c ;; if we're given a guess for c also, use that.
		   c
		   (+ bx (* +phi+ (- bx ax))))) ;; first guess for c
	   (fc (funcall f cx)))
      ;;(format t "fa(~10f)= ~10f, fb(~10f)= ~10f, fc(~10f)= ~10f~%" ax fa bx fb cx fc)
      ;; If fb = fc, then we might want to look farther, but there's a reasonable chance that this is as good as we'll get.
      (loop while (> fb fc) do ;; keep going until we find an fc > fb
      ;;(loop while (>= fb fc) do ;; keep going until we find an fc > fb
	   ;; Compute u by parabolic extrapolation from a, b, c
	   (let* ((u (parabolic-extrapolation ax bx cx fa fb fc))
		  (ulim (+ bx (* +glimit+ (- cx bx))))  ;; 'u' could be very far away, this is the max we'll go.
		  (fu nil))
	     (cond ((> (* (- bx u) (- u cx)) 0.0) ;; The extrapolated value is in the range we're already looking at (b/tw b and c)
		    ;; Evaluate the function at u, and then check if any 3 of points a, b, c, and u form a bracketing triplet.
		    (setf fu (funcall f u))
		    (cond ((< fu fc) ;; got a minimum between b and c
			   ;; So, use b,u,c as the triplet.  (alt: we could use a,u,c ?? - no)
			   (return-from bracket-minimum-core (values bx u cx fb fu fc )))
			  ((> fu fb) ;; got a minimum between a and u
			   ;; This is, f(u) is greater than f(b), so *b* is the minimum, and f(a) and f(u) are greater
			   ;; So use a,b,u  as the triplet
			   (write-line "RETURN POINT 2")
			   (return-from bracket-minimum-core (values ax bx u fa fb fu))))
		    ;; Parabolic fit was no use, use magnification by adding "phi * (c - b)" to c to get a new point to try
		    (setf u (+ cx ( * +phi+ (- cx bx))))
		    (setf fu (funcall f u)))
		   ;; O/w u is *beyond* c.  If it's within our limit, then "try" it.
		   ;; If f(u) < f(c), then this 'u' didn't help...
		   ;; ... so, use 'c' as the new 'b'; 'u' as the new 'c'; and set 'u' to (c + phi*(c-b))
		   ((> (* (- cx u) (- u ulim)) 0.0)
		    (setf fu (funcall f u))
		    (when (< fu fc)
		      (shiftf bx cx u (+ cx (* +phi+ (- cx bx))))
		      (shiftf fb fc fu (funcall f u))))
		   ;; If 'u' was bigger than our max, scale it down to our max, evaluate it, and proceed.
		   ((>= (* (- u ulim) (- ulim cx)) 0.0)
		    (setf u ulim)
		    (setf fu (funcall f u)))
		   ;; Reject parabolic u, use default magnification
		   (t 
		    (setf u (+ cx (* +phi+ (- cx bx))))
		    (setf fu (funcall f u))))
	     ;; Eliminate oldest point and continue
	     ;; That is, get rid of 'a', and use b,c,u as the new triplet.
	     (shiftf ax bx cx u)
	     (shiftf fa fb fc fu)))
      ;; If we fell out of the main loop..., then 
      ;; fb <= fc
      (values ax bx cx fa fb fc))))

(defun bracket-minimum (f ax bx &optional cx)
  "Like above, but ensures that ax < bx < cx."
  (multiple-value-bind (ax bx cx fa fb fc)
      (bracket-minimum-core f ax bx cx)
    ;; Put them in the "correct" order...
    (when (> ax cx)
      (rotatef ax cx)
      (rotatef fa fc))
    ;; this is all of a sudden broken!?!?
    ;;(assert (< ax bx cx))
    (values ax bx cx fa fb fc)))

