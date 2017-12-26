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

(defun dbrent-linemin (f df ax bx cx &key (max-iterations 200) (tolerance *default-linemin-tolerance*))
  "A version of the Brent line minimization algorithm that uses a derivative function."
  (let ((a 0.0d0) (b 0.0d0) (d 0.0d0)
	(d1 0.0d0) (d2 0.0d0) (du 0.0d0) (dv 0.0d0) (dw 0.0d0) (dx 0.0d0) 
	(e 0.0d0) (iterations 0)
	fu fv fw fx olde tol1 tol2 u u1 u2 v w x xm
	ok1 ok2
	points)
    (setf a (if (< ax cx) ax cx))
    (setf b (if (> ax cx) ax cx))
    (setf x bx w bx v bx)
    (setf fx (funcall f x) fv fx fw fx)
    (push (list x fx) points)
    (setf dx (funcall df x) dv dx dw dx)
    (loop for i from 0 below max-iterations do
	 (incf iterations)
	 (setf xm (* 0.5 (+ a b)))
	 (setf tol1 (+ (* tolerance (abs x)) +epsilon+))
	 (setf tol2 (* 2.0 tol1))
	 ;; Check if we're done.
	 (when (<= (abs (- x xm)) (- tol2 (* 0.5 (- b a))))
	   (return-from dbrent-linemin (values x fx iterations points)))
	 (if (> (abs e) tol1)
	     (progn
	       (setf d1 (* 2.0 (- b a)))
	       (setf d2 d1)
	       (when (/= dw dx)
		 (setf d1 (/ (* (- w x) dx) (- dx dw))))
	       (when (/= dv dx)
		 (setf d2 (/ (* (- v x) dx) (- dx dv))))
	       (setf u1 (+ x d1))
	       (setf u2 (+ x d2))
	       (setf ok1 (and (> (* (- a u1) (- u1 b)) 0.0) (<= (* dx d1) 0.0)))
	       (setf ok2 (and (> (* (- a u2) (- u2 b)) 0.0) (<= (* dx d2) 0.0)))
	       (Setf olde e)
	       (setf e d)
	       (if (or ok1 ok2)
		   (progn
		     (cond ((and ok1 ok2)
			    (setf d (if (< (abs d1) (abs d2)) d1 d2)))
			   (ok1 (setf d d1))
			   (t (setf d d2)))
		     (if (<= (abs d) (abs (* 0.5 olde)))
			 (progn
			   (setf u (+ x d))
			   (when (or (< (- a u) tol2) (< (- b u) tol2))
			     (setf d (sign tol1 (- xm x)))))
			 (progn
			   (setf e (if (>= dx 0.0) (- a x) (- b x)))
			   (setf d (* 0.5 e)))))
		   (progn
		     (setf e (if (>= dx 0.0) (- a x) (- b x)))
		     (setf d (* 0.5 e)))))
	     (progn
	       (setf e (if (>= dx 0.0) (- a x) (- b x)))
	       (setf d (* 0.5 e))))
	 (if (>= (abs d) tol1)
	     (progn
	       (setf u (+ x d))
	       (setf fu (funcall f u))
	       (push (list u fu) points))
	     (progn
	       (setf u (+ x (sign tol1 d)))
	       (setf fu (funcall f u))
	       (push (list u fu) points)
	       (when (> fu fx)
		 (return-from dbrent-linemin (values x fx iterations points)))))
	 ;; Housekeeping..
	 (setf du (funcall df u))
	 (if (<= fu fx)
	     (progn
	       (if (>= u x) (setf a x) (setf b x))
	       (psetf v w fv fw dv dw)
	       (psetf w x fw fx dw dx)
	       (psetf x u fx fu dx du))
	     (progn 
	       (if (< u x) (setf a u) (setf b u))
	       (cond ((or (<= fu fw) (= w x))
		      (psetf v w fv fw dv dw)
		      (psetf w u fw fu dw du))
		     ((or (< fu fv) (= v x) (= v w))
		      (psetf v u fv fu dv du)))))
	 );; end loop
    (error "Too many iterations in Brent")))
