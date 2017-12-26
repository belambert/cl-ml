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

(in-package :lm-training)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;; ACCURACY ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun model-pairwise-count-correct (model pairs &key (use-sphinx-score t) max-feature-num (score-type :sum))
  "Given an model and some pairs of candidates, evaluate the model: i.e. count the number of correct decisions."
  (assert (kb-asr-ready))
  (let* ((results (mapcar (lambda (x) (single-pair-choice model x :use-sphinx-score use-sphinx-score :max-feature-num max-feature-num :score-type score-type)) pairs))
	 (net (combine-pair-results results)))
    (net-correct net)))

(defun count-correct-as-function-of-parameters-function (model data &key (use-sphinx-score t) max-feature-num (score-type :sum))
  "Function that returns a wrapper function of the above function."
  (assert (kb-asr-ready))
  (lambda (parameters)
    (setf (parameters model) parameters)
    (model-pairwise-count-correct model data :use-sphinx-score use-sphinx-score :max-feature-num max-feature-num :score-type score-type)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;; PAIRWISE WER ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun model-pairwise-wer (model pairs &key (use-sphinx-score t) max-feature-num (score-type :sum))
  "Given an model and some pairs of candidates, evaluate the model: i.e. count the number of correct decisions."
  (assert (kb-asr-ready))
  (let* ((results (mapcar (lambda (x) (single-pair-choice model x :use-sphinx-score use-sphinx-score :max-feature-num max-feature-num :score-type score-type)) pairs))
	 (model-wer (/ (reduce '+ (mapcar 'wer ;;'candidate-wer
						     (mapcar (lambda (x) (if (eq (pair-choice x) :second)
									     (pair-c2 x) (pair-c1 x)))
							     results)))
			  (length results))))
    model-wer))

(defun wer-as-function-of-parameters-function (model data &key (use-sphinx-score t) max-feature-num (score-type :sum))
  "Return a function that wraps the previous function."
  (assert (kb-asr-ready))
  (lambda (parameters)
    (setf (parameters model) parameters)
    (model-pairwise-wer model data :use-sphinx-score use-sphinx-score :max-feature-num max-feature-num :score-type score-type)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;; NBEST WER  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun model-nbest-wer (model nbests &key (use-sphinx-score t) max-feature-num (score-type :sum))
  "An objective function to minimize the WER after reranking n-best lists."
  (declare (ignore use-sphinx-score)) ;;the nbest re-ranking automatically uses the sphinx score
  (assert (kb-asr-ready))
  (setf nbests (mapcar (lambda (x) (nscore-and-rank-nbest-with-model x model :max-feature-num max-feature-num :score-type score-type)) nbests))
  (let* ((evals (mapcar (lambda (x) (evaluate-nbest-list x :evaluate-ranking-p nil)) nbests))
	 (net (aggregate-evaluations evals)))
    (net-evaluation-wer net)))

(defun nbest-wer-as-function-of-parameters-function (model nbests &key (use-sphinx-score t) max-feature-num (score-type :sum))
  "Returns a function that's a wrapper for the previous function."
  (declare (ignore use-sphinx-score)) ;;the nbest re-ranking automatically uses the sphinx score
  (assert (kb-asr-ready))
  (lambda (parameters)
    (setf (parameters model) parameters)
    (model-nbest-wer model nbests :max-feature-num max-feature-num :score-type score-type)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;; NBest DCG training ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun model-nbest-ndcg (model nbests &key max-feature-num (score-type :sum))
  "Objective function to maximize the NDCG after n-best re-ranking."
  (assert (kb-asr-ready))
  ;;(score-and-rank-with-model model nbests) ;;22%
  (mapcar (lambda (x) (nscore-and-rank-nbest-with-model x model :max-feature-num max-feature-num :score-type score-type)) nbests)
  (let* ((evals (mapcar (lambda (x) (evaluate-nbest-list x :evaluate-ranking-p t)) nbests))
	 (net (aggregate-evaluations evals :evaluate-ranks-p t))) ;;the aggregation doesn't take much time at all
    (assert (numberp (net-evaluation-mean-ndcg net)))
    (net-evaluation-mean-ndcg net)))

(defun nbest-ndcg-as-function-of-parameters-function (model nbests &key max-feature-num (score-type :sum) &allow-other-keys)
  "Returns a function that's a wrapper for the previous function."
  (assert (kb-asr-ready))
  (lambda (parameters)
    (setf (parameters model) parameters)
    (model-nbest-ndcg model nbests :max-feature-num max-feature-num :score-type score-type)))
