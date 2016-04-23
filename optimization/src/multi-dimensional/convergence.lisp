;;;; Author: Ben Lambert
;;;; ben@benjaminlambert.com

(in-package :optimization)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Convergence testing ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun converged-p (fp fp-prev tolerance &key verbose)
  "Check if the proportional change from fp-prev to fp is smaller than the given tolerance."
  (when verbose
    (format t "Checking for convergence~%")
    (format t "*****CHECKING FOR CONVERGENCE*****~%")
    (when fp-prev (format t "  Prev best f(x): ~20F~%" fp-prev))
    (when fp (format t "  Best f(x):      ~20F~%" fp))
    (force-output *standard-output*))
  (unless (and fp fp-prev)
    (when verbose (format t "Not yet converged.~%")  (force-output *standard-output*))
    (return-from converged-p nil))
  (let ((change-2x (* 2.0 (abs (- fp fp-prev))))
	(min-change-2x (* tolerance (+ (abs fp) (abs fp) +epsilon+))))
    (when verbose
      (format t "  Raw change:         ~F~%" (- fp fp-prev))
      (format t "  Abs change x 2:     ~F~%" change-2x)
      (format t "  Min delta x 2:      ~F~%" min-change-2x))
    (let ((converged-p (<= change-2x min-change-2x)))
      (when verbose
	(if converged-p
	    (format t "*****CONVERGENCE REACHED*****~%")
	    (format t "Not yet converged.~%"))
	(force-output *standard-output*))
      converged-p)))

(defun converged-gradient-p ()
  "Check if we've reached convergence based on the current and previous gradient...(???)"
  ;; (let ((test 0.0d0)
  ;; 	 (den (max fp 1.0d0)))
  ;;   (loop for j from 0 below n
  ;; 	for temp = (/ (* (abs (aref g j))
  ;; 			 (max (abs (aref p j)) 1.0d0))
  ;; 		      den) do
  ;; 	  (when (> temp test) (setf test temp)))
  ;;   (when (< test g-tolerance)
  ;;     (format t "Converged b/c of gradient..."
  ;;     (return-from bfgs p)))
  nil)

