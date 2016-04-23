;;;; Author: Benjamin E. Lambert (ben@benjaminlambert.com)

(declaim (optimize (debug 3)))
(in-package :maxent)

(defun data-feature-expectations (sentences model &key importance-sampling)
  (let ((expectations (make-array (length (cl-lm:parameters model)) :initial-element 0.0d0 :element-type 'double-float))
	(denominator 0)) ;; The number we'll divide by to turn the "counts" into expectations
    (dolist (s sentences)
      ;; This is a wasteful way to simply do the feature extraction...
      (cl-lm:score model s)
      (let ((weight (if importance-sampling
			(/ ;;(cl-lm:score model s)
			 (cl-lm:log-prob-of-sentence model s)
			 (cl-lm:score (baseline-model model) s))
			1)))
	(incf denominator weight)
	(loop for feature across (kbasr-aux:sentence-features s) do
	     (incf (aref expectations feature) weight))))
    ;; Turn the counts into expectations
    ;;(alexandria:coercef denominator 'single-float)
    (alexandria:coercef denominator 'double-float)
    (if (/= denominator 0)
	(map-into expectations (lambda (x) (coerce (/ x denominator) 'double-float)) expectations)
	(fill expectations 0.0d0))
    expectations))

(defun data-log-likelihood (sentences model)
  (reduce '+ sentences :key (lambda (x) (pseudo-log-prob-of-sentence model x))))

(defun data-log-likelihood-optimizable-function (sentences model)
  (lambda (x)
    (setf (cl-lm:parameters model) x)
    (reduce '+ sentences :key (lambda (x) (pseudo-log-prob-of-sentence model x)))))

(defun gradient (data-expectations model sample)
  (let ((sample-expectations (sample-feature-expectations sample model)))
    (map '(vector double-float) #'- data-expectations sample-expectations)))

(defun gradient-function (data-expectations model sample)
  (lambda (x)
    (setf (cl-lm:parameters model) x)
    (gradient data-expectations model sample)))

(defun sample-feature-expectations (sample model)
  (data-feature-expectations sample model :importance-sampling t))

(defun train-whole-sentence-me-model (pattern-lm ngram-lm sentences sample)
  (let ((me-lm (create-whole-sentence-maxent-lm pattern-lm ngram-lm)))
    (approximate-z sample me-lm)
    (let* ((data-expectations (data-feature-expectations sentences me-lm))
	   ;;(sample-expectations (sample-feature-expectations sample me-lm))
	   (likelihood-function (data-log-likelihood-optimizable-function sentences me-lm))
	   (gradient-function (gradient-function data-expectations me-lm sample)))
      (format t "Data log likelihood: ~f~%" (data-log-likelihood sentences me-lm))

      ;;(cl-optimization:conjugate-gradient-descent likelihood-function (length (cl-lm:parameters me-lm)) :x (cl-lm:parameters me-lm) :df gradient-function)
      ;;(cl-optimization:coordinate-descent likelihood-function (length (cl-lm:parameters me-lm)) :x (cl-lm:parameters me-lm))
      (cl-optimization:steepest-descent likelihood-function (length (cl-lm:parameters me-lm)) :x (cl-lm:parameters me-lm) :df gradient-function)
      )))
  
