;;;; Author: Ben Lambert (ben@benjaminlambert.com)

(cl-user::file-summary "Defines package optimization")

(defpackage :optimization-test
  (:use :common-lisp :optimization :array-operations :alexandria :split-sequence :blambert-util)
  (:export :run-all-tests))




