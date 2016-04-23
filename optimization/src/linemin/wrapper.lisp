;;;; Author: Ben Lambert
;;;; ben@benjaminlambert.com

(in-package :optimization)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Wrapper function to do bracketing and minimization of a 1d-function ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun line-minimization-1d (f &key (ax *default-linemin-lower*) (bx *default-linemin-upper*) (tolerance *default-linemin-tolerance*) (linemin *default-linemin*) verbose)
  "Perform bracketing and minimizeation on a 1d function f.  
   ax and bx are initial guesses for the bracketing."
  (multiple-value-bind (ax bx cx fa fb fc)
      (bracket-minimum f ax bx)
    (when verbose (format t "Using bracket points: ~F ~F ~F~%" ax bx cx))
    (when (= fa fb fc)
      (format t "WARNING: Bracket points are equivalent, returning zero.~%")
      (return-from line-minimization-1d 0.0d0))
    (case linemin
      (:brute (golden-section-search f ax bx cx :tolerance tolerance))
      (:golden-section (golden-section-search f ax bx cx :tolerance tolerance))
      (:brent (brent-linemin f ax bx cx :tolerance tolerance))
      (:dbrent (dbrent-linemin f (finite-difference-gradient-function f) ax bx cx :tolerance tolerance))
      (otherwise (error "Unknown line search method: ~A" linemin)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Line minimization in the context of a multidimensional optimization ;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun linear-objective-function (f origin vector)
  "Returns a function that can be used to do multi-dimensional line minimization using
   a 1d line minimization function.  f is the original function; p is the 'starting' point;
   and vector is a vector in the direction to be optimized."
  (lambda (x)
    (let* ((vector-from-origin (array-scalar* vector x)) ;; multiply the vector by x
	   (eval-point (array+ vector-from-origin origin))
	   (f-x (funcall f eval-point))) ;; Finally evaluate the function at the new-point
      f-x)))

(defun line-minimization (f p x &key (lower *default-linemin-lower*) (upper *default-linemin-upper*) (tolerance *default-linemin-tolerance*)
			  (linemin *default-linemin*) (plot-bracket-points t) iteration folder verbose)
  "Given point p and vector x, minimize function f along vector x, beginning from point p.
   ax and bx are initial guesses for the bracketing.  Returns the point at which the minimum
   value was found.  Returns the multiplier of x as a second value."
  (let* ((f-1dim (linear-objective-function f p x))
	 (f-eval-count-before *function-evaluations*)
	 (x-norm (vector-norm x))
	 (middle (+ lower (/ (- upper lower) 2)))
	 (f-lower (funcall f-1dim lower))
	 (f-middle (funcall f-1dim middle))
	 (f-upper (funcall f-1dim upper))
	 (graph-name (if iteration
			 (format nil "Iteration-~d" iteration)
			 (format nil "~A" (random most-positive-fixnum)))))
    (when folder
      (setf graph-name (concatenate 'string folder "/" graph-name)))			 
    (when verbose (format t "Trying to do linemin along vector...~%"))
    (if (some 'nanp x)
	(format t "WARNING: SEARCH DIRECTION VECTOR CONTAINS NANs!!!!!!!!!!!!!!!!!~%")
	(when verbose (print-vector x :verbose nil)))
    (when verbose
      (format t "... from point:~%")
      (print-vector p :verbose nil))
    ;; It's a number and it's not zero.
    (when (and (not (zerop x-norm)) (not (nanp x-norm)))
      (setf lower (* lower (/ 1 x-norm)))
      (setf middle (* middle (/ 1 x-norm)))
      (setf upper (* upper (/ 1 x-norm))))
    ;; Get bracket points
    (when verbose (format t "Bracketing hints: ~10f ~10f ~10f~%" lower middle upper))
    (multiple-value-bind (ax bx cx fa fb fc)
	(bracket-minimum f-1dim lower middle upper)
      ;; If this failed, return.
      (when (= fa fb fc)
	(when (and verbose (not (eq linemin :brute)))
	  (format t "WARNING: Bracketing failed, returning original point. DEBUG:~%")
	  (format t "     f(~10f)= ~10f, f(~10f)= ~10f, f(~10f)= ~10f~%" ax fa bx fb cx fc))
	(return-from line-minimization p))
      ;; O/w do a line search with those bracketing points
      (when verbose
	(format t "Performing line search with bracket points:~%")
	(format t "     f(~,4f)= ~10f,   f(~,4f)= ~10f,   f(~,4f)= ~10f~%" ax fa bx fb cx fc))
      ;; With the bracket points, do a minimization along vector x
      (multiple-value-bind (lambda fx iterations points)
	  (case linemin
	    (:brute (linemin-brute f-1dim -150d0 0.0d0 150d0 :tolerance tolerance))
	    (:golden-section (golden-section-search f-1dim ax bx cx :tolerance tolerance))
	    (:brent (brent-linemin f-1dim ax bx cx :tolerance tolerance))
	    (:dbrent (dbrent-linemin f-1dim (finite-difference-gradient-function f) ax bx cx :tolerance tolerance)) ;; need the df function
	    (otherwise (error "Unknown line search method: ~A" linemin)))
	(declare (ignore fx iterations))
	(if plot-bracket-points
	    (setf points (sort (append points (list (list ax fa) (list bx fb) (list cx fc) (list lower f-lower) (list middle f-middle) (list upper f-upper)   )) '< :key 'first))
	    (setf points (sort points '< :key 'first)))
	(when verbose
	  (handler-case
	      (progn
		(gnuplot:plot-graph points :2dgraph :type :dumb :width 79 :height 24 :legend nil :debug nil :filename graph-name :title "Line search")
		(gnuplot:plot-graph points :2dgraph :legend nil :debug t :filename graph-name :title "Line search"))
	    (error (e) (format t "Error plotting line search: ~A" e))))
	(let* ((f-eval-count-after *function-evaluations*)
	       (f-eval-count (- f-eval-count-after f-eval-count-before))
	       (new-point (array+ p (array-scalar* x lambda))))
	  (when verbose
	    (format t "Completed line minimization:~%")
	    (format t "  Along ~:d-dimensional vector.  |x| = ~f ~%" (length x) (vector-norm x))
	    (format t "  Lambda= ~15,10f     after ~5:D function evaluations.~%" lambda f-eval-count)
	    (format t "  |p_new| = ~f~%" (vector-norm new-point)))
	  ;; Construct the new minimum point that we found
	  (values new-point
		  lambda
		  f-eval-count))))))
