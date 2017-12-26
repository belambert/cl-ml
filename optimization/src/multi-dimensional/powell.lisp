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

(defun powell (f n &key x (tolerance *default-tolerance*) (linemin-tolerance *default-linemin-tolerance*) (linemin *default-linemin*) (max-iter *default-max-iter*)(linemin-lower *default-linemin-lower*) (linemin-upper *default-linemin-upper*) feature-mask folder verbose)
  "Powell's method.
   See: http://en.wikipedia.org/wiki/Powell's_method , or Press chapter 11.5 (?).
   This seems to be, relatively speaking, pretty damn slow...
   Using the Brent linemin speeds this up alot, but that linemin alg is pulling us way away from zero, often."
  (with-optimization
    (ensure-origin x n)
    (setf f (wrap-function-with-counter f))
    (let ((p0 (copy-seq x))
	  (direction-set (get-coordinate-directions n :feature-mask feature-mask))
	  (p x)
	  (pn nil)
	  fp fp-prev
	  (x-history '())
	  (line-search-count 0))
      (push p0 x-history)
      (loop for iteration from 1 to max-iter do
	   (when verbose (format-section t "Powell iteration #~:d~%" iteration))
	   (setf fp-prev fp)
	   ;; Iterate over the directions
	   (loop for direction in direction-set
	      for i from 1 to n do
		(when verbose (format t "Iter# ~4:d  dir# ~4:d~%" iteration i))
		(setf p (line-minimization f p direction :tolerance linemin-tolerance :linemin linemin :verbose verbose
					   :lower linemin-lower :upper linemin-upper :iteration line-search-count :folder folder))
		(incf line-search-count)
		(when verbose (format t "Direction# = ~5:D~%" i))
		(when (= i n)
		  (setf pn p)))
	 ;; Remove the first
	   (pop direction-set)
	 ;; Pn - P0 is the new direction, compute it at add to the direction set
	   (let ((new-direction (array- pn p0)))
	     (nconc direction-set (list new-direction))
	     ;; Do a linemin in the new direction and call the new point p0
	     (setf p (line-minimization f p new-direction :tolerance linemin-tolerance :linemin linemin :iteration line-search-count :verbose verbose))
	     (incf line-search-count)
	     (setf p0 p))
	   (push p0 x-history)
	   (when verbose (print-point-history x-history))
	   (setf fp (funcall f p))
	   (when (converged-p fp fp-prev tolerance)
	     (return))
	 ;; Reset the direction set every n iterations
	   (when (= (mod iteration n) 0)
	     (setf direction-set (get-coordinate-directions n))))
      p)))
