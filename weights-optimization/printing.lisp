;;; Copyright Benjamin E. Lambert, 2005-2011
;;; All rights reserved
;;; Please contact author regarding licensing and use:
;;; ben@benjaminlambert.com

(declaim (optimize (debug 3)))
(in-package :lm-training)
(cl-user::file-summary "Printing the progress and graphing the training process")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;; Graphing stuff... ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl-user::section "Graphing stuff...")

(defun graph-parameter-sweep (points filename &key (verbose nil) (debug t))
  "Plots a single graph of the objective function over the course of the entire optimization."
  (when (= (length (first points)) 3)
    (gnuplot:plot-graph points :heatmap :filename (concat filename "-map") :title "2d parameter sweep" :legend-labels '("Objective function") :x-label "parameter1" :y-label "parameter2" :debug debug :verbose verbose)
    (gnuplot:plot-graph points :surface :filename (concat filename "-surface") :title "2d parameter sweep" :legend-labels '("Objective function") :x-label "parameter1" :y-label "parameter2" :z-label "Objective function" :debug debug :verbose verbose)
    (let ((counter 0))
      (setf points (mapcar (lambda (x) (prog1 (list counter (third x)) (incf counter))) points))))
  (gnuplot:plot-graph points :2dgraph :filename filename :title "Parameter sweep: time vs. objective function" :x-label "Function evaluation # (approx)" :y-label "Objective function" :debug debug :verbose verbose))

(defun graph-feature-parameter-sweeps (array prefix &key model (min-feature 0) (max-feature most-positive-fixnum) (verbose nil) (debug nil))
  "Plot one graph per parameter of the parameter value vs. the objective function."
  (unless array
    (format t "Couldn't plot feature parameter sweeps.  NIL array.~%")
    (return-from graph-feature-parameter-sweeps))
  (loop for feature from (max 0 min-feature) below (min (length array) max-feature)
     for points = (elt array feature)
     for filename = (format nil "~Afeature-~d" prefix feature)
     for this-feature = (when model (elt (patterns model) feature))
     for title = (format nil "Param val vs. obj func for feat #~:D: ~{~A ~}" feature (append (list (pattern-type this-feature)) (pattern-fillers this-feature)))
     for axis-labels =  '("Parameter value" "Objective function") do
       ;; Print the scatter plot version
       (gnuplot:plot-graph points :scatter :filename (concat filename "-scatter") :title title :x-label (first axis-labels) :y-label (second axis-labels) :debug debug :verbose verbose)
       ;; Print thumbnail versions...
       (setf filename (concat filename "-thumb"))
       (gnuplot:plot-graph points :scatter :filename (concat filename "-scatter") :title title :x-label (first axis-labels) :y-label (second axis-labels) :debug debug :verbose verbose :thumbnail t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;; Training ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl-user::section "Training")

(defun print-non-zero-features (array model)
  "Given an array of feature weights and a model, print out all the non-zero features in a readable format."
  (format t "~:D non-zero weight features:~%" (count-if-not 'zerop (parameters model)))
  (loop for i from 0 below (length array)
       for value = (elt array i)
       for feature = (elt (patterns model) i) do
       (when (/= value 0)
	 (format t "    Feature ~8:D  Weight=~10,2f, Name: ~A~%" i value (print-pattern feature nil))))
  (force-output t))

(defun print-before-and-after-reranking (model nbests &key (prefix ""))
  "Given a model and some nbest lists, evaluate and print performance before and after reranking."
  (let* ((before (evaluate-nbest-lists nbests))
	 (after (evaluate-nbest-lists (rerank-with-model nbests model)))
	 (best (evaluate-nbest-lists (rerank-by-wer nbests)))
	 ;;(delta (struct-subtract-recursive after before))
	 (delta (struct-subtract after before)))
    (format t "********** ~A best possible **********~%" prefix)
    (print-evaluation-statistics best)
    (format t "********** ~A baseline **********~%" prefix)
    (print-evaluation-statistics before)
    (format t "********** ~A with model **********~%" prefix)
    (print-evaluation-statistics after)
    (format t "********** ~A delta **********~%" prefix)
    (print-evaluation-statistics delta)))

(defun print-before-and-after-pairwise (model1 model2 pairs &key (prefix ""))
  "There really isn't a *before* with the pairwise eval... that's already factored in."
  (let* ((before (pairwise-evaluation model1 pairs))
	 (after (pairwise-evaluation model2 pairs)))
    (declare (ignore before))
    ;;(delta (struct-subtract-recursive after before)))
    (format t "********** ~A with model **********~%" prefix)
    (print-summary-results after)))

