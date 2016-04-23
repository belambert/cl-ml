;;-*- Mode: Lisp -*- 

;;;; Author: Benjamin E. Lambert (ben@benjaminlambert.com)

(asdf:defsystem "maxent"
  :description "Maximum entropy language modeling"
  :version "0.1.0"
  :author "Benjamin Lambert"
  :licence "All rights reserved"
  :serial t
  :components
  ((:module src
	    :serial t
	    :components ((:file "package")
			 (:file "clos")
			 (:file "z-approximation")
			 (:file "train")
			 )))
  :depends-on (:lispdoc :blambert-util :cl-optimization :language-model :metatilities))

