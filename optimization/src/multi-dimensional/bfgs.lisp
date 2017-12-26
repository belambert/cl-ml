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

(defun bfgs (f n &key x df (gradient-epsilon *default-gradient-epsilon*) (max-iter *default-max-iter*) (tolerance *default-tolerance*) (linemin-tolerance *default-linemin-tolerance*) (linemin *default-linemin*)(linemin-lower *default-linemin-lower*) (linemin-upper *default-linemin-upper*) feature-mask (gradient-type *default-gradient-type*) folder verbose)
  "The Broyden–Fletcher–Goldfarb–Shanno (BFGS) method.
   See: http://en.wikipedia.org/wiki/BFGS_method ."
  (setf f (wrap-function-with-counter f))
  (ensure-origin x n)
  (ensure-gradient df f gradient-epsilon :feature-mask feature-mask :gradient-type gradient-type)
  (with-optimization
    (let* (;;(g-tolerance tolerance)
	   (p x)
	   (hessian (make-array (list n n) :element-type 'double-float :initial-element 0.0d0))
	   (xi (make-array n :element-type 'double-float :initial-element 0.0d0))
	   (delta-gradient (make-array n :element-type 'double-float :initial-element 0.0d0))
	   (hdg (make-array n :element-type 'double-float :initial-element 0.0d0))
	   (fp-prev nil)
	   ;; Calculate the starting function value and gradient
	   (fp (funcall f p))
	   (g (funcall df p))
	   (x-history '()))
      ;; Initialize the inverse Hessian to the unit matrix
      (dotimes (i n)
	;; 1.0's on the diagonal
	(setf (aref hessian i i) 1.0d0)
	;; The initial search direction is the negative gradient
	(setf (aref xi i) (- (aref g i))))
      ;; Main iterations
      (loop for iter from 1 to max-iter
	 for xi-normalized = (normalize-vector xi) do
	   (push p x-history)
	   (when verbose (print-vector-history x-history xi))
	 ;; Do a line minimization along xi
	 ;; Minimize along the normalized vector, so the lambda is on the [-200, 200]-ish scale ??
	   (setf p (line-minimization f p xi-normalized :tolerance linemin-tolerance :linemin linemin
				      :lower linemin-lower :upper linemin-upper :iteration iter :folder folder :verbose verbose))
	   (setf fp-prev fp)
	   (setf fp (funcall f p))
	   (when verbose (format-now t "Direction# = ~5:D~%" iter))
	   (when (converged-p fp fp-prev tolerance)
      	     (return))
	 ;; Save the old gradient
      	   (setf delta-gradient (copy-seq g))
	 ;; Re-compute the gradient
      	   (setf g (funcall df p))
	 ;; Compute the difference of gradients
      	   (setf delta-gradient (array- g delta-gradient))
	 ;; We could check for convergence on the gradient here if we wanted to...
      	   ;; And the difference times the current (Hessian?) matrix
      	   (dotimes (i n)
      	     (setf (aref hdg i) 0.0d0)
      	     (dotimes (j n)
      	       (incf (aref hdg i) (* (aref hessian i j) (aref delta-gradient j)))))
      	   ;; Calculate the dot products for the denominators
      	   (let ((fac 0.0d0)
      		 (fae 0.0d0)
      		 (fad nil))
      	     (dotimes (i n)
      	       (incf fac (* (aref delta-gradient i) (aref xi i)))
      	       (incf fae (* (aref delta-gradient i) (aref hdg i))))
	     ;; The delta-gradient is ending up as all zeros at the end... and leading to NaN's ...
      	     ;; Make the denominators multiplicative
      	     (setf fac (/ 1.0d0 fac))
      	     (setf fad (/ 1.0d0 fae))
      	     (dotimes (i n)
      	       (setf (aref delta-gradient i) (- (* fac (aref xi i)) (* fad (aref hdg i)))))
      	     ;; The vector which makes BFGS different from DFP
      	     (dotimes (i n)
      	       (dotimes (j n)
      		 (incf (aref hessian i j )
      		       (+ (* fac (aref xi i) (aref xi j))
      			  (- (* fad (aref hdg i) (aref hdg j)))
      			  (* fae (aref delta-gradient i) (aref delta-gradient j))))))
      	     ;; Now calculate the next direction to go
      	     (dotimes (i n)
      	       (setf (aref xi i) 0.0d0)
      	       (dotimes (j n)
      		 (decf (aref xi i) (* (aref hessian i j) (aref g j)))))))
      p)))
