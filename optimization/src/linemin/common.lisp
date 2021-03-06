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

(in-package :optimization)

(defconstant +glimit+ 100.0
  "When bracketing, the maximum magnification allowed for a parabolic-fit step.")

(defconstant +r+ 0.61803399d0
  "A golden ratio.")

(defconstant +c+ (- 1.0 +r+)
  "A golden ratio.")

(defun sign (a b)
  "Return a version of a which is the same sign as b."
  (if (> b 0.0)
      (abs a)
      (- (abs a))))
