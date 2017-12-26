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

(in-package :maxent)

(defclass* whole-sentence-maxent-lm (cl-lm::pattern-lm)
  ((baseline-model nil iar "The baseline model")
   (normalization-constant nil iar "The Z constant."))
  (:documentation "A whole sentence maxent LM that handles sentence 'patterns'"))

(defmethod cl-lm:log-prob-of-sentence ((lm whole-sentence-maxent-lm) (sentence kbasr-aux:sentence))
  (let ((score (cl-lm::score lm sentence))
	(baseline-log-prob (cl-lm::log-prob-of-sentence (baseline-model lm) sentence)))
    (+ (- (log (normalization-constant lm))) baseline-log-prob score)))

(defmethod pseudo-log-prob-of-sentence ((lm whole-sentence-maxent-lm) sentence &key max-feature-num score-type force-feature-extraction)
  "This is the non-normalized log prob of the given sentence."
  (declare (ignore max-feature-num score-type force-feature-extraction))
  (let ((baseline-score (cl-lm::score (baseline-model lm) sentence))
	(model-adjustment-score (cl-lm::score lm sentence)))
    (+ baseline-score model-adjustment-score)))

(defun create-whole-sentence-maxent-lm (pattern-lm ngram-lm)
  "Creates a whole-sentence MaxEnt LM from a pattern LM and an n-gram LM.  This simply copies all the
   information in the pattern LM into a new MaxEnt LM and sets the baseline model to the ngram-lm."
  (let ((me-lm (make-instance 'whole-sentence-maxent-lm
			      :pattern-modes (cl-lm:pattern-modes pattern-lm)
			      :pattern-file (cl-lm:pattern-file pattern-lm)
			      :patterns (cl-lm:patterns pattern-lm)
			      :pattern-table (cl-lm:pattern-table pattern-lm)
			      :non-lexical-patterns (cl-lm:non-lexical-patterns pattern-lm)
			      :parameters (cl-lm:parameters pattern-lm)
			      :score-combination-type (cl-lm:score-combination-type pattern-lm)
			      :contrast-as-weight-p (cl-lm:contrast-as-weight-p pattern-lm)
			      :contrast-multiplier (cl-lm:contrast-multiplier pattern-lm)
			      :abstraction-functions (cl-lm:abstraction-functions pattern-lm)
			      :grammar-file (cl-lm:grammar-file  pattern-lm)
			      :filler-file (cl-lm:filler-file pattern-lm)
			      :baseline-model ngram-lm)))
    me-lm))
