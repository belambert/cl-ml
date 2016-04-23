;;; Copyright Benjamin E. Lambert, 2005-2011
;;; All rights reserved
;;; Please contact author regarding licensing and use:
;;; ben@benjaminlambert.com

(in-package :lm-training)
(declaim (optimize (debug 3)))

(cl-user::file-summary "")

(defun g-function (sentence lm &key language-weight)
  "This function simply gives us a 'score' for the sentence,
   taking the acoustic and LM scores into account.
   Some call it a 'discriminant function'."
  (let* ((alpha (/ language-weight))
	 (acoustic (sentence-acoustic-score sentence))
	 ;;(lm-score (log-prob-of-sentence lm sentence))
	 (lm-score (score lm sentence))
	 )
    (+ (* alpha acoustic) lm-score)))

(defun little-g-wrapper (sentence lm eta &key language-weight)
  "This would have been embedded in the big-g-function, but we pulled it out to here
   so we can do the summing much cleaner."
  (exp (* (g-function sentence lm :language-weight language-weight) eta)))

(defun big-g-function (nbest lm eta &key language-weight)
  "An 'anti-discriminant function' that represents the competitors."
  (let* ((incorrect-candidates (remove-if 'sentence-correct-p (nbest-candidates nbest)))
	 (sum (reduce '+ incorrect-candidates :key (lambda (x) (little-g-wrapper x lm eta :language-weight language-weight))))
	 (n (length incorrect-candidates))) ;; should N be the length of the incorrect candidates, or the number of candidates?
    ;; The Juang paper has N as n-1?
    (expt (log (/ sum n)) (/ eta))))

(defun d-function (nbest lm eta &key language-weight)
  "This mis-classification function is called 'd' in the literature."
  (let (;;(w0 (get-reference-sentence nbest))
	(w0 (get-lowest-wer-sentence nbest))
	)
    (+ (- (g-function w0 lm :language-weight language-weight))
       (big-g-function nbest lm eta :language-weight language-weight))))

;; Gamma and theta represent....  slope and shift

(defun loss-function (nbest lm eta &key (gamma 1) (theta 0) language-weight)
  "This is the discriminant function embedded in a sigmoid function."
  (/ (+ 1
	(exp (+ (- (* gamma (d-function nbest lm eta :language-weight language-weight)))
		theta)))))

;; There's a lot we could do to optimize this function...
(defun loss-function-gradient (nbest lm eta &key (gamma 1) (theta 0) language-weight)
  (let* ((dimensions (length (parameters lm)))
	 (gradient (make-array dimensions :element-type 'single-float :initial-element 0.0)))
    (dotimes (i dimensions)
      (setf (aref gradient i)
	    (coerce
	     (* (* (* gamma (loss-function nbest lm eta :gamma gamma :theta theta :language-weight language-weight))
		   (- 1 (loss-function nbest lm eta :gamma gamma :theta theta :language-weight language-weight)))
		(loss-function-partial-derivative nbest lm eta i :language-weight language-weight))
	     'single-float
	     )))
    gradient))

(defun c-r-function (sentence nbest lm eta &key language-weight)
  (let ((numerator (little-g-wrapper sentence lm eta :language-weight language-weight))
	(denominator (reduce '+ (nbest-candidates nbest) :key (lambda (x) (little-g-wrapper x lm eta :language-weight language-weight)))))
    (/ numerator denominator)))

(defun loss-function-partial-derivative (nbest lm eta i &key language-weight)
  (let* (;;(w0 (get-reference-sentence nbest))
	 (w0 (get-lowest-wer-sentence nbest))
	 (w0-feature-count (count i (sentence-active-features w0)))  ;; First term is negative the number of times feature i occurs in the reference
	 (term2-sum 0))
    ;; Second term is a sum for R from 1 to N of likelihood-style number times the # of times feature i occurs in sentence R
    (loop for sentence across (nbest-candidates nbest)
       for wr-feature-count = (count i (sentence-active-features sentence)) do
	 (incf term2-sum (* (c-r-function sentence nbest lm eta :language-weight language-weight)
			    wr-feature-count)))
    (+ (- w0-feature-count)
       term2-sum)))

(defun expected-loss (nbests lm eta &key (gamma 1) (theta 0) language-weight)
  (reduce '+ nbests :key (lambda (nbest) (loss-function nbest lm eta :gamma gamma :theta theta :language-weight language-weight))))

(defun expected-loss-gradient (nbests lm eta &key (gamma 1) (theta 0) language-weight)
  (reduce '+ nbests :key (lambda (nbest) (loss-function-gradient nbest lm eta :gamma gamma :theta theta :language-weight language-weight))))

(defun get-parameterized-expected-loss-function (nbests lm eta &key (gamma 1) (theta 0) language-weight)
  (lambda (x)
    (setf (parameters lm) x)
    (expected-loss nbests lm eta :gamma gamma :theta theta :language-weight language-weight)))

(defun get-parameterized-expected-loss-gradient-function (nbests lm eta &key (gamma 1) (theta 0) language-weight)
  (lambda (x)
    (setf (parameters lm) x)
    (expected-loss-gradient nbests lm eta :gamma gamma :theta theta :language-weight language-weight)))
  

(defun run-juang-training (nbests lm &key (eta 1) (gamma 1) (theta 0))
  (let* ((language-weight (nbest-language-weight (elt nbests 0)))
	 (function (get-parameterized-expected-loss-function nbests lm eta :gamma gamma :theta theta :language-weight language-weight))
	 (gradient (get-parameterized-expected-loss-gradient-function nbests lm eta :gamma gamma :theta theta :language-weight language-weight)))
    (cl-optimization:conjugate-gradient-descent function (length (parameters lm)) :df gradient)
    ;; Return the LM:
    lm))
						




