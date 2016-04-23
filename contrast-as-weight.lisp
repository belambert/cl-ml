;;;; Author: Ben Lambert
;;;; ben@benjaminlambert.com

(declaim (optimize (debug 3)))
(in-package :lm-training)

(defun contrast-function-as-weight (new-model-file pattern-file &key 
				    train test id (nbest-evaluation nil) 
				    contains-correct-only ensure-contains-correct score-type multiplier
				    (abstraction-functions '(identity)) save-model cache discard-non-features)
  "Perform an evaluation using the contrast function as the weights.  THIS DOES NOT DO ACTUAL TRAINING..."
  (assert (kb-asr-ready))
  (format-now t "Creating model from pattern file...~%")
  (let* ((model (make-instance 'pattern-lm :pattern-file pattern-file :contrast-as-weight t :contrast-multiplier multiplier :abstraction-functions abstraction-functions)))
    (when save-model
      (format-now t "Saving model to file...~%")
      (save-model model new-model-file))
    (format-now t "Beginning evaluation...~%")
    (ensure-directories-exist (format nil "~A/file.txt" (id-to-result-folder id)))
    (format-now t "Performing N-Best evaluation...~%")(finish-output)
    (evaluate-nbest-complete model (read-nbest-lists train :contains-correct-only contains-correct-only :ensure-contains-correct ensure-contains-correct
								   :parse-on-load t :parse-on-load-model model :discard-non-features discard-non-features)
				    :folder (id-to-result-folder id) :prefix "train" :score-type score-type)
    (when test
      (evaluate-nbest-complete model (read-nbest-lists test :contains-correct-only contains-correct-only :ensure-contains-correct ensure-contains-correct 
								     :parse-on-load t :parse-on-load-model model :discard-non-features discard-non-features)
				      :folder (id-to-result-folder id) :prefix "test" :score-type score-type))
    (format t "ID: ~A~%" id)
    (format t "Finished.~%")
    model))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;; Wrapper functions for models to evaluate accuracy for diff parameters. ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sweep-multiplier (model nbests &key (begin -10) (end 10) multiplier-list)
  "Do a parameter sweep and evaluation of model multipliers."
  (setf model (copy-model model))
  (map-into nbests 'copy-nbest nbests)
  (let ((points '())
	(min most-positive-fixnum)
	(min-multiplier nil)
	(initial-weights (parameters model)))
    (unless multiplier-list
      (setf multiplier-list (bl:intervals 1 begin end)))

    (dolist (multiplier multiplier-list)
	 (setf (parameters model) (map '(vector double-float) (lambda (x) (* x multiplier)) initial-weights))
	 (let ((eval (evaluate-model-on-nbests model nbests)))
	   (push (list multiplier (net-evaluation-wer eval)) points)
	   (format t "~:D     ~F ~%" multiplier (net-evaluation-wer eval))
	   (when (< (net-evaluation-wer eval) min)
	     (setf min (net-evaluation-wer eval))
	     (setf min-multiplier multiplier))
	   (finish-output t)))
    ;; Set the parameters to the optimal value
    (setf (parameters model) (map '(vector double-float) (lambda (x) (* x min-multiplier)) initial-weights))
    (ignore-errors (gnuplot:plot-graph points :2dgraph :type :dumb :width 79 :height 24 :legend nil :debug nil :title "Multiplier sweep"))
    (format t "Min: ~F     ~d~%" min min-multiplier)
    model))

(defun sweep-multiplier-from-file (pattern-file nbests &key (begin -10) (end 10) multiplier-list)
  "Do a parameter sweep and evaluation of model multipliers."
  (let ((model (make-instance 'pattern-lm-with-contrast-weights :pattern-file pattern-file :abstraction-functions '(identity))))
    (sweep-multiplier model nbests :begin begin :end end :multiplier-list multiplier-list)))


