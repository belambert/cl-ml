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

(defun brent-linemin (f ax bx cx &key (max-iterations 200) (tolerance *default-linemin-tolerance*))
  "F is a 1-d function, and ax, bx, and cx are bracketing points.
   This is broken?!!?"
  (let ((a 0.0d0) (b 0.0d0) (d 0.0d0) (e 0.0d0)
	(cgold 0.3819660)
	(iterations 0)
	etemp fu fv fw fx
	p q r tol1 tol2 u v w x xm
	points)
    (setf a (if (< ax cx) ax cx))
    (setf b (if (> ax cx) ax cx))
    (setf x bx w bx v bx)
    (setf fx (funcall f x) fv fx fw fx)
    (push (list x fx) points)
    (loop for i from 0 below max-iterations do
	 (incf iterations)
	 (setf xm (* 0.5 (+ a b)))
	 (setf tol1 (+ (* tolerance (abs x)) +epsilon+))
	 (setf tol2 (* 2.0 tol1))
	 ;; Check if we're done.
	 (when (<= (abs (- x xm)) (- tol2 (* 0.5 (- b a))))
	   (return-from brent-linemin (values x fx iterations points)))
	 (if (> (abs e) tol1)
	     (progn
	       (setf r (* (- x w) (- fx fv)))
	       (setf q (* (- x v) (- fx fw)))
	       (setf p (- (* (- x v) q)
			  (* (- x w) r)))
	       (setf q (* 2.0 (- q r)))
	       (when (> q 0.0)
		 (setf p (- p)))
	       (setf q (abs q))
	       (setf etemp e)
	       (setf e d)
	       ;; Check the acceptability of the parabolic fit
	       (if (or (>= (abs p) (abs (* 0.5 q etemp)))
		       (<= p (* q (- a x)))
		       (>= p (* q (- b x))))
		   (progn 
		     (setf e (if (>= x xm) (- a x) (- b x)))
		     (setf d (* cgold e)))
		   (progn
		     (setf d (/ p q))
		     (setf u (+ x d))
		     (when (or (< (- u a) tol2)
			       (< (- b u) tol2))
		       (setf d (sign tol1 (- xm x)))))))
	     (progn
	       (setf e (if (>= x xm) (- a x) (- b x)))
	       (setf d (* cgold e))))
	 ;; This is in the loop, but at the top-level
	 (setf u (if (>= (abs d) tol1)
		     (+ x d)
		     (+ x (sign tol1 d))))
	 (setf fu (funcall f u))
	 (push (list u fu) points)
	 ;; This is the one function evaluation per iteration
	 (if (<= fu fx)
	     (progn
	       (if (>= u v) (setf a x) (setf b x))
	       (shiftf v w x u)
	       (shiftf fv fw fx fu))
	     (progn
	       (if (< u x) (setf a u) (setf b u))
	       (cond ((or (<= fu fw) (= w x))
		      (setf v w w u fv fw fw fu))
		     ((or (<= fu fv) (= v x) (= v w))
		      (setf v u)
		      (setf fv fu))))));; end loop
    (error "Too many iterations in Brent")))
