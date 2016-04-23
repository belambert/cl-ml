;;; Author: Ben Lambert
;;; ben@benjaminlambert.com

(in-package :lm-training)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;; Training  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
(defun train-model-pairwise-count-correct (train pattern-file model-file &rest rest &key contains-correct-only ensure-contains-correct score-type &allow-other-keys)
  "Main function for optimization maximizing pairwise accuracy."
  (assert (kb-asr-ready))
  (let* ((model (apply 'train-simple-model train pattern-file (lambda (x &rest rest) (apply 'read-pairs x :contains-correct-only contains-correct-only :ensure-contains-correct ensure-contains-correct rest))  'count-correct-as-function-of-parameters-function t :score-type score-type :model-file model-file rest)))
    model))

(defun train-model-pairwise-wer (train pattern-file model-file &rest rest &key contains-correct-only ensure-contains-correct score-type &allow-other-keys)
  "Main function for optimizing pairwise WER -- THIS IS EQUIVALENT TO THE PREVIOUS ONE... DUH."
  (assert (kb-asr-ready))
  (let* ((model (apply 'train-simple-model train pattern-file (lambda (x &rest rest) (apply 'read-pairs x :contains-correct-only contains-correct-only :ensure-contains-correct ensure-contains-correct rest)) 'wer-as-function-of-parameters-function nil :score-type score-type :model-file model-file rest)))
    model))

(defun train-model-nbest-wer (train pattern-file model-file &rest rest &key contains-correct-only ensure-contains-correct score-type &allow-other-keys)
  "Main function for optimizing WER after n-best re-ranking."
  (assert (kb-asr-ready))
  (let* ((model (apply 'train-simple-model train pattern-file (lambda (x &rest rest) (apply 'read-nbest-lists x :contains-correct-only contains-correct-only :ensure-contains-correct ensure-contains-correct rest)) 'nbest-wer-as-function-of-parameters-function nil :score-type score-type :model-file model-file rest)))
    model))

(defun train-model-nbest-ndcg (train pattern-file model-file &rest rest &key contains-correct-only ensure-contains-correct score-type &allow-other-keys)
  "Main function for optimizing NDCG after n-best re-ranking."
  (assert (kb-asr-ready))
  (let* ((model (apply 'train-simple-model train pattern-file (lambda (x &rest rest) (apply 'read-nbest-lists x :contains-correct-only contains-correct-only :ensure-contains-correct ensure-contains-correct rest)) 'nbest-ndcg-as-function-of-parameters-function t :score-type score-type :model-file model-file rest)))
    model))

(defun negate-function (f)
  "Return a function that is the negated version of the given function."
  (lambda (x)
    (- (funcall f x))))


(defun optimize-model (model-file new-model &key train test id pattern-file pattern-count
		       contains-correct-only ensure-contains-correct 
		       (optimization :coordinate) (tolerance 1d-8) (linemin-tolerance 1d-8) (gradient-epsilon 20d0) reset-parameters (linemin cl-optimization::*default-linemin*) (mode :nbest)
		       (linemin-lower cl-optimization::*default-linemin-lower*) (linemin-upper cl-optimization::*default-linemin-upper*)
		       (gradient-type cl-optimization::*default-gradient-type*)
		       (score-type :sum)
		       (max-iter 10))
  "Mode can be :nbest or :pairs

   We could have this take a 'rest' parameter, or maybe a list of nbest reading option...?
   maybe also bundle together the options for the optimization function...?

   We're not using the 'id' here, and perhaps we do want to be using it?!!?"
  (assert (kb-asr-ready))
  (let* ((model (if model-file 
		    (load-model model-file)
		    (make-instance 'pattern-lm :pattern-file pattern-file :pattern-count pattern-count)))
	 (initial-model nil)
	 (train-set (case mode
		      (:nbest (read-nbest-lists train :parse-on-load t :parse-on-load-model model :evaluate-on-load t :discard-non-features t))
		      (:pairs (read-pairs train :parse-on-load t :parse-on-load-model model :evaluate-on-load t :discard-non-features t 
					  :contains-correct-only contains-correct-only :ensure-contains-correct ensure-contains-correct))
		      (otherwise (error "Unrecognized 'mode' ~A.  Known modes are: :nbest and :pairs~%" mode))))
	 (nbests-for-training (if (eq mode :nbest)
				  (mapcar 'copy-nbest train-set)
				  (mapcar 'copy-seq train-set)))
	 ;; function to minimize
	 (f (case mode
	      (:nbest (nbest-wer-as-function-of-parameters-function model nbests-for-training :score-type score-type))
	      (:pairs (negate-function (accuracy-as-function-of-parameters-function model nbests-for-training :score-type score-type)))
	      (otherwise (error "Unrecognized 'mode' ~A.  Known modes are: :nbest and :pairs~%" mode))))
	 (parameters (parameters model))
	 (n (length parameters))
	 (folder (format nil "./results/results-for-id-~A/" id))
	 (features (get-data-features train-set))
	 (feature-mask (feature-mask-from-features features n)))
    (format t "Beginning optimization of ~:D of ~:D parameters.~%" (length features) n)

    (when reset-parameters 
      (setf parameters (make-array n :element-type 'double-float :initial-element 0.0d0))
      (setf (parameters model) parameters))
    (setf initial-model (copy-model model))
    ;; Print an initial evaluation before doing any optimization
    ;; These two are only going to std out... should we also send them to a file in 'folder'???
    (case mode
      (:nbest (print-evaluation-statistics (evaluate-nbest-lists train-set)))
      (:pairs (print-summary-results (pairwise-evaluation model train-set :score-type score-type))))
    ;; Do the optimization
    (let ((new-parameters (case optimization
			    (:random (cl-optimization::random-parameters f n))
			    (:coordinate (cl-optimization::coordinate-descent f n :x parameters :tolerance tolerance :linemin-tolerance linemin-tolerance :linemin linemin
									 :max-iter max-iter :feature-mask feature-mask :linemin-lower linemin-lower :linemin-upper linemin-upper :folder folder))
			    (:steepest (cl-optimization::steepest-descent f n :x parameters  :tolerance tolerance  :linemin-tolerance linemin-tolerance :gradient-epsilon gradient-epsilon :linemin linemin 
								     :max-iter max-iter :feature-mask feature-mask :linemin-lower linemin-lower :linemin-upper linemin-upper :gradient-type gradient-type :folder folder))
			    (:powell (cl-optimization::powell f n :x parameters :tolerance tolerance :linemin-tolerance linemin-tolerance :linemin linemin 
							 :max-iter max-iter :feature-mask feature-mask :linemin-lower linemin-lower :linemin-upper linemin-upper :folder folder))
			    (:conjugate-gradient (cl-optimization::conjugate-gradient-descent f n :x parameters :tolerance tolerance :linemin-tolerance linemin-tolerance :gradient-epsilon gradient-epsilon :linemin linemin
											 :max-iter max-iter :feature-mask feature-mask :linemin-lower linemin-lower :linemin-upper linemin-upper :gradient-type gradient-type :folder folder))
			    (:bfgs (cl-optimization::bfgs f n :x parameters :tolerance tolerance :linemin-tolerance linemin-tolerance :gradient-epsilon gradient-epsilon :linemin linemin 
						     :max-iter max-iter :feature-mask feature-mask :linemin-lower linemin-lower :linemin-upper linemin-upper :gradient-type gradient-type :folder folder))
			    (otherwise (error "Unknown optimization type:~A" optimization)))))
      (setf (parameters model) new-parameters))
    (print-non-zero-features (parameters model) model)
    ;; Print an evaluation on the train data
    (case mode
      (:nbest (evaluate-nbest-complete model train-set :folder folder :prefix "train" :score-type score-type))
      (:pairs (pairwise-evaluation-full model train-set :folder folder :suffix "train" :score-type score-type)))
    (format t "Saving new model to file: ~A~%" new-model)
    (save-model model new-model)
    ;; It should be safe to clear out the training data at this point
    (setf train-set nil)
    (setf nbests-for-training nil)
    ;; Print results on test data
    (when test
      (let ((test-set (case mode
			(:nbest (read-nbest-lists test :parse-on-load t :parse-on-load-model model :evaluate-on-load t))
			(:pairs (read-pairs test :parse-on-load t :parse-on-load-model model :evaluate-on-load t)))))
	(case mode
	  (:nbest (evaluate-nbest-complete model test-set :folder folder :prefix "test" :score-type score-type))
	  (:pairs (pairwise-evaluation-full model test-set :folder folder :suffix "test" :score-type score-type)))))
    (format t "ID: ~A~%" id)
    (print-time-since-start)
    (format t "Finished.~%")
    model))

(defun optimize-model-new (pattern-file model-file &key nbest-folder train-ctl pattern-count
			   (optimization :coordinate) (tolerance 1d-8) (linemin-tolerance 1d-8) (gradient-epsilon 20d0) reset-parameters (linemin cl-optimization::*default-linemin*)
			   (linemin-lower cl-optimization::*default-linemin-lower*) (linemin-upper cl-optimization::*default-linemin-upper*)
			   (gradient-type cl-optimization::*default-gradient-type*)
			   (score-type :sum)
			   (max-iter 10))
  "Mode can be :nbest or :pairs

   We could have this take a 'rest' parameter, or maybe a list of nbest reading option...?
   maybe also bundle together the options for the optimization function...?

   We're not using the 'id' here, and perhaps we do want to be using it?!!?"
  (assert (kb-asr-ready))
  (let* ((model (make-instance 'pattern-lm :pattern-file pattern-file :pattern-count pattern-count))
	 (initial-model nil)
	 (training-nbests (read-nbest-lists nbest-folder :parse-on-load t :parse-on-load-model model :evaluate-on-load t :discard-non-features t))
	 (nbests-for-training (mapcar 'copy-nbest train-set))
	 ;; function to minimize
	 (f (nbest-wer-as-function-of-parameters-function model nbests-for-training :score-type score-type))
	 (parameters (parameters model))
	 (n (length parameters))
	 (folder (format nil "./results/results-for-id-~A/" id))
	 (features (get-data-features train-set))
	 (feature-mask (feature-mask-from-features features n)))
    (format t "Beginning optimization of ~:D of ~:D parameters.~%" (length features) n)
    (setf initial-model (copy-model model))
    ;; Print an initial evaluation before doing any optimization
    ;; These two are only going to std out... should we also send them to a file in 'folder'???
    (print-evaluation-statistics (evaluate-nbest-lists train-set))
    ;; Do the optimization
    (let ((new-parameters (case optimization
			    (:random (cl-optimization::random-parameters f n))
			    (:coordinate (cl-optimization::coordinate-descent f n :x parameters :tolerance tolerance :linemin-tolerance linemin-tolerance :linemin linemin
									 :max-iter max-iter :feature-mask feature-mask :linemin-lower linemin-lower :linemin-upper linemin-upper :folder folder))
			    (:steepest (cl-optimization::steepest-descent f n :x parameters  :tolerance tolerance  :linemin-tolerance linemin-tolerance :gradient-epsilon gradient-epsilon :linemin linemin 
								     :max-iter max-iter :feature-mask feature-mask :linemin-lower linemin-lower :linemin-upper linemin-upper :gradient-type gradient-type :folder folder))
			    (:powell (cl-optimization::powell f n :x parameters :tolerance tolerance :linemin-tolerance linemin-tolerance :linemin linemin 
							 :max-iter max-iter :feature-mask feature-mask :linemin-lower linemin-lower :linemin-upper linemin-upper :folder folder))
			    (:conjugate-gradient (cl-optimization::conjugate-gradient-descent f n :x parameters :tolerance tolerance :linemin-tolerance linemin-tolerance :gradient-epsilon gradient-epsilon :linemin linemin
											 :max-iter max-iter :feature-mask feature-mask :linemin-lower linemin-lower :linemin-upper linemin-upper :gradient-type gradient-type :folder folder))
			    (:bfgs (cl-optimization::bfgs f n :x parameters :tolerance tolerance :linemin-tolerance linemin-tolerance :gradient-epsilon gradient-epsilon :linemin linemin 
						     :max-iter max-iter :feature-mask feature-mask :linemin-lower linemin-lower :linemin-upper linemin-upper :gradient-type gradient-type :folder folder))
			    (otherwise (error "Unknown optimization type:~A" optimization)))))
      (setf (parameters model) new-parameters))
    (print-non-zero-features (parameters model) model)
    ;; Print an evaluation on the train data
    (evaluate-nbest-complete model train-set :folder folder :prefix "train" :score-type score-type)
    (format t "Saving new model to file: ~A~%" model-file)
    (save-model model model-file)
    ;; It should be safe to clear out the training data at this point
    model))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;; Training from n-best re-ranking ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun train-simple-model (folder pattern-file read-data-function objective-function-function maximize
				 &key
				 (optimization-function 'parameter-sweep-optimization)
				 model-file (use-sphinx-score t)
				 (min -150) (max 150) (step 10)
				 (abstraction-functions *default-abstraction-functions*)
				 (pattern-count most-positive-fixnum)
				 graph-sweep (graph-feature-sweeps nil)
				 (min-feature 0) (max-feature most-positive-fixnum)
				 (max-cycles 10)
				 (parse-on-load t)
				 (score-type :sum)
				 &allow-other-keys)
  "Wrapper-ish function for optimization..."
  (assert (kb-asr-ready))
  (let* ((model (make-instance 'pattern-lm :pattern-file pattern-file :abstraction-functions abstraction-functions :pattern-count pattern-count))
	 (data (funcall read-data-function folder :parse-on-load parse-on-load :parse-on-load-model model :discard-non-features t))
	 (function (funcall objective-function-function model data :use-sphinx-score use-sphinx-score :score-type score-type))
	 (init-param (parameters model)))
    (format t "Beginning optimization...~%")(force-output t)
    (multiple-value-bind (opt all-fx feature-fx)
	(funcall optimization-function function init-param :min min :max max :step step :maximize maximize :min-feature min-feature :max-feature max-feature :max-cycles max-cycles)
      (format t "Finished optimization...~%")(force-output t)
      (setf (parameters model) opt)
      (format t "Saving model to ~a...~%" model-file)(force-output t)
      (save-model model model-file)
      (write-line "Plotting graphs...")(force-output t)
      (when graph-sweep (graph-parameter-sweep all-fx graph-sweep))
      (when graph-feature-sweeps (graph-feature-parameter-sweeps feature-fx graph-feature-sweeps :model model :min-feature min-feature :max-feature max-feature))
      (print-non-zero-features opt model)
      (print-time-since-start)
      (write-line "Finished.")(force-output t)
      model)))
