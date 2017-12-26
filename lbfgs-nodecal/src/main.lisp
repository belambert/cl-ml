;; Orig Copyright 2006 by Robert Dodier
;; Released under the terms of the GNU General Public License.

;;; Some parameters

(defparameter *default-lbfgs-max-iterations* 100) ;; 15? 20? 30? 100? ;; usually gets close after about 20... but to be safe.
(defparameter *default-lbfgs-ncorrections* 10) ;;25? ... affects memory use more than time?

(defun train-with-lbfgs (function gradient epsilon n
			 &key (max-iterations *default-lbfgs-max-iterations*)
			 (ncorrections *default-lbfgs-ncorrections*)
			 x-initial
			 (verbosity 0)
			 (iteration-verbosity 0)
			 (model nil)
			 (data nil)
			 (return-on-line-search-failure t))
  "std. verbosity: -1/0/1"
  (unless x-initial
    (setf x-initial (make-array n :initial-element 0.0d0 :element-type 'double-float)))
  (setf epsilon (coerce epsilon 'double-float))

  (let* ((m ncorrections)
	 (nwork (+ (* n (+ (* 2 m) 1)) (* 2 m))))    
    (format t "Parameter count:              ~:d~%" n)
    (format t "Update history count:         ~:d~%" m)
    (format t "Working space needed:         ~:d~%" nwork)
    (format t "Working space needed (bytes): ~:d~%" (* 8 nwork))
    (format t "Max iterations:               ~:d~%" max-iterations)
    (force-output t)    
  (let* ((xtol double-float-epsilon)
	 (iflag 0)
	 (info nil)
	 (scache (make-array n :element-type 'double-float))
	 (w (make-array nwork :element-type 'double-float))
	 (diag (make-array n :element-type 'double-float))
	 (g (make-array n :element-type 'double-float))
	 (x x-initial)
	 (iprint (make-array 2 :element-type 'f2cl-lib:integer4))
	 (diagco nil)
	 (f 0.0d0)
	 (previous-f nil))
    (assert (or (= verbosity -1) (= verbosity 0) (= verbosity 1)))
    (assert (or (= iteration-verbosity 0) (= iteration-verbosity 1) (= iteration-verbosity 2) (= iteration-verbosity 3)))
    (setf (aref iprint 0) (coerce verbosity '(signed-byte 32)))
    (setf (aref iprint 1) (coerce iteration-verbosity '(signed-byte 32)))
    (setf diagco f2cl-lib:%false%)
    (common-lisp-user::/blockdata-lb2/)
    (dotimes (nfeval max-iterations)
      (format t "L-BFGS iteration # ~S~%" nfeval)(force-output t)
      (setf f (funcall function x model data))
      (setf g (funcall gradient x model data))
      (if previous-f
	(format t "F: ~10F  PREV-F: ~10F delta: ~10F~%" f previous-f (- f previous-f))
	(format t "F: ~10F~%" f))
      (setf previous-f f)
      (force-output t)
      (multiple-value-bind (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                                  var-8 var-9 var-10 var-11 var-12 var-13)
        (common-lisp-user::lbfgs n m x f g diagco diag iprint epsilon xtol w iflag scache)
        (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                         var-8 var-9 var-10 var-12))
	;; this is the status...
        (setf iflag var-11)
	(setf info var-13)
        (cond
          ((eq iflag 0)
	   (format t "L-BFGS optimization converged after ~A iterations.~%" nfeval)
	   (return-from train-with-lbfgs nfeval))
	  ((eq iflag 1)) ;; do nothing...
          ((eq iflag -1)
	   (format t "WARNING: The line search routine MCSRCH failed.~%")
	   (when info (print-info-code info))
	   (when return-on-line-search-failure
	     (format t "Line search failed, returning current model.~%")
	     (return-from train-with-lbfgs nfeval)))
          ((eq iflag -2)
	   (format t "WARNING: The i-th diagonal element of the diagonal inverse Hessian approximation, given in DIAG, is not positive.~%"))
          ((eq iflag -3)
	   (format t "Improper input parameters for LBFGS (N or M are not positive~%"))
	  (t 
	   (format t "Unknown status code '~A'.  Continuing...  ~%" iflag)))
	  ))))

  (format t "Completed ~A iterations. Stopping before convergence...~%" max-iterations)
  max-iterations)

(defun print-info-code (code-num)
  "Helper function to report what's goign wrong with L-BFGS."
  (cond ((= code-num 0)
	 (format t "IMPROPER INPUT PARAMETERS.~%"))
	((= code-num 2)
	 (format t "RELATIVE WIDTH OF THE INTERVAL OF UNCERTAINTY IS AT MOST XTOL.~%"))
	((= code-num 3)
	 (format t "MORE THAN 20 FUNCTION EVALUATIONS WERE REQUIRED AT THE PRESENT ITERATION.~%"))
	((= code-num 4)
	 (format t "THE STEP IS TOO SMALL~%"))
	((= code-num 5)
	 (format t "THE STEP IS TOO LARGE.~%"))
	((= code-num 6)
	 (format t "ROUNDING ERRORS PREVENT FURTHER PROGRESS.  THERE MAY NOT BE A STEP WHICH SATISFIES~%THE SUFFICIENT DECREASE AND CURVATURE CONDITIONS. TOLERANCES MAY BE TOO SMALL.~%"))
	(t (format t "Unknown info code '~A'~%" code-num))))
