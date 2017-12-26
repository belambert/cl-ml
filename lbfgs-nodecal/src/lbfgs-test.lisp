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

(let* ((n 5 ) ;;variable count, n
      (m 3) ;;corrections per update,  3<= M <=7 is recommended. Restriction: M>0
      (initial-solution (make-array n :element-type 'double-float :initial-element 1.0d0));; X DOUBLE PRECISION array of length N.
      (variable-value-of-f 0.0d0) ;;is a DOUBLE PRECISION variable. Before initial entry and on a re-entry with IFLAG=1, it must be set by the user to contain the value of the function F at the point X.
      (gradient (make-array n :element-type 'double-float :initial-element 0.0d0));; G is a DOUBLE PRECISION array of length N. Before initial entry and on a re-entry with IFLAG=1, it must be set by the user to contain the components of the gradient G at the point X.
      (diagco-p nil) ;; DIAGCO  is a LOGICAL variable that must be set to .TRUE. if the user  wishes to provide the diagonal matrix Hk0 at each iteration. Otherwise it should be set to .FALSE., in which case  LBFGS will use a default value described below. If DIAGCO is set to .TRUE. the routine will return at each iteration of the algorithm with IFLAG=2, and the diagonal matrix Hk0  must be provided in the array DIAG.
      (diag (make-array n :element-type 'double-float :initial-element 0.0d0)) ;;DIAG    is a DOUBLE PRECISION array of length N. If DIAGCO=.TRUE., then on initial entry or on re-entry with IFLAG=2, DIAG it must be set by the user to contain the values of the  diagonal matrix Hk0.  Restriction: all elements of DIAG must be positive.
      (iprint (make-array 2 :element-type '(signed-byte 32) :initial-contents '(1 3)))
      #|     IPRINT(1) specifies the frequency of the output:
                IPRINT(1) < 0 : no output is generated,
                IPRINT(1) = 0 : output only at first and last iteration,
                IPRINT(1) > 0 : output every IPRINT(1) iterations.
 
             IPRINT(2) specifies the type of output generated:
                IPRINT(2) = 0 : iteration count, number of function 
                                evaluations, function value, norm of the
                                gradient, and steplength,
                IPRINT(2) = 1 : same as IPRINT(2)=0, plus vector of
                                variables and  gradient vector at the
                                initial point,
                IPRINT(2) = 2 : same as IPRINT(2)=1, plus vector of
                                variables,
                IPRINT(2) = 3 : same as IPRINT(2)=2, plus gradient vector.
      |#
      (eps 0.0001d0) ;; EPS     is a positive DOUBLE PRECISION variable that must be set by the user, and determines the accuracy with which the solution is to be found. The subroutine terminates when ||G|| < EPS max(1,||X||), where ||.|| denotes the Euclidean norm.
      (xtol (coerce 1e-16 'double-float)) ;;XTOL    is a  positive DOUBLE PRECISION variable that must be set by the user to an estimate of the machine precision (e.g. 10**(-16) on a SUN station 3/60). The line search routine will terminate if the relative width of the interval of uncertainty is less than XTOL.
      (w (make-array (+ (* n (1+ (* 2 m))) (* 2 m)) :element-type 'double-float)) ;; is a DOUBLE PRECISION array of length N(2M+1)+2M used as workspace for LBFGS. This array must not be altered by the user.
      (status-flag 0)
      #|
IFLAG=-1  The line search routine MCSRCH failed. The
C                        parameter INFO provides more detailed information
C                        (see also the documentation of MCSRCH):
C                       INFO = 0  IMPROPER INPUT PARAMETERS.
C                       INFO = 2  RELATIVE WIDTH OF THE INTERVAL OF
C                                 UNCERTAINTY IS AT MOST XTOL.
C                       INFO = 3  MORE THAN 20 FUNCTION EVALUATIONS WERE
C                                 REQUIRED AT THE PRESENT ITERATION.
C                       INFO = 4  THE STEP IS TOO SMALL.
C                       INFO = 5  THE STEP IS TOO LARGE.
C                       INFO = 6  ROUNDING ERRORS PREVENT FURTHER PROGRESS. 
C                                 THERE MAY NOT BE A STEP WHICH SATISFIES
C                                 THE SUFFICIENT DECREASE AND CURVATURE
C                                 CONDITIONS. TOLERANCES MAY BE TOO SMALL.
C              IFLAG=-2  The i-th diagonal element of the diagonal inverse
C                        Hessian approximation, given in DIAG, is not
C                        positive.
C              IFLAG=-3  Improper input parameters for LBFGS (N or M are
C                        not positive).
      |#
       (scache (make-array n :element-type 'double-float :initial-element 0.0d0)))

      ;;  (lbfgs n m x f g diagco diag iprint eps xtol w iflag scache)
      (lbfgs n m initial-solution variable-value-of-f gradient diagco-p diag iprint eps xtol w status-flag scache)

      (format t "Status flag: ~A~%" status-flag)
      (format t "Final solution: ~A~%" initial-solution))
