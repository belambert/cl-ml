;;;; Author: Ben Lambert
;;;; ben@benjaminlambert.com

(in-package :lm-training)

(defun lm-compatible-p (lm1 lm2)
  "Check if two language models are 'compatible'..."
  (and (eq (type-of lm1) (type-of lm2))
       ;; The equal checks
       (equal (lm-log-base lm1) (lm-log-base lm2))
       (equal (vocab-size lm1) (vocab-size lm2))
       (equal (start-sentence-token lm1) (start-sentence-token lm2))
       (equal (end-sentence-token lm1) (end-sentence-token lm2))
       (equal (open-vocabulary lm1) (open-vocabulary lm2))
       (equal (oov-token lm1) (oov-token lm2))
       (equal (feature-count lm1) (feature-count lm2))
       (equal (parameter-count lm1) (parameter-count lm2))
       ;; The equalp checks
       (equalp (feature-types lm1) (feature-types lm2))
       (equalp (patterns lm1) (patterns lm2))
       (equalp (vocab lm1) (vocab lm2))))

(defun id-to-result-folder (id)
  "Returns the name of a folder to put the results in for the given ID."
  (format nil "./results/results-for-id-~A/" id))

(defun combine-models (new-model-file &key model-files train test id (nbest-evaluation nil) contains-correct-only ensure-contains-correct score-type)
  "Combine a bunch of model files into one.  We're using this so that we can optimize parameters in parallel, then save a full model,
   that only has those parameters optimized.  *Then* we can combine all of the model files into one.
   We *should* be doing another optimization step after combining the files."
  (assert (kb-asr-ready))
  (format t "Attempting to combine ~:D models.~%" (length model-files))
  (let* ((new-model (load-model (first model-files))))
    (dolist (file (rest model-files))
      (handler-case
	  (let ((model (load-model file)))
	    (assert (lm-compatible-p model new-model))
	    (setf (parameters new-model) (array+ (parameters model) (parameters new-model))))
	(error (e) (format t "Couldn't load model ~A, Error: ~A" file e))))
    (format t "*** Saving combined model to file: ~A~%" new-model-file)
    (save-model new-model new-model-file)
    (print-non-zero-features (parameters new-model) new-model)
    (when train
      (format t "*** Evaluating pairwise accuracy...~%")
      (pairwise-evaluation-full new-model (read-pairs train :contains-correct-only contains-correct-only :ensure-contains-correct ensure-contains-correct :parse-on-load t :parse-on-load-model new-model :top-and-best-only t)
				:folder (id-to-result-folder id) :suffix "train" :score-type score-type))
    (when test
      (format t "*** Evaluating pairwise accuracy...~%")
      (pairwise-evaluation-full new-model (read-pairs test :contains-correct-only contains-correct-only :ensure-contains-correct ensure-contains-correct :parse-on-load t :parse-on-load-model new-model :top-and-best-only t)
				:folder (id-to-result-folder id) :suffix "test" :score-type score-type))
    (when nbest-evaluation
      (format t "Performing N-Best evaluation...~%")
      (evaluate-nbest-complete new-model (read-nbest-lists train) :folder (id-to-result-folder id) :prefix "train" :score-type score-type)
      (when test (evaluate-nbest-complete new-model (read-nbest-lists test) :folder (id-to-result-folder id) :prefix "test" :score-type score-type)))
    (format t "ID: ~A~%" id)
    (print-time-since-start)
    (format t "Finished.~%")
    new-model))


