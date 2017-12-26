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

(defpackage :optimization
  (:use :common-lisp :alexandria :split-sequence :blambert-util)
  (:export :line-minimization-1d
	   :*default-linemin-tolerance*
	   :coordinate-descent
	   :steepest-descent
	   :conjugate-gradient-descent
	   :bfgs
	   :powell
	   :square))
