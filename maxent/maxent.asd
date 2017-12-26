;;-*- Mode: Lisp -*- 

(asdf:defsystem "maxent"
  :description "Maximum entropy language modeling"
  :version "0.1.0"
  :author "Ben Lambert"
  :licence "Apache-2.0"
  :serial t
  :components
  ((:module src
	    :serial t
	    :components ((:file "package")
			 (:file "clos")
			 (:file "z-approximation")
			 (:file "train"))))
  :depends-on (:lispdoc :blambert-util :cl-optimization :language-model :metatilities))
