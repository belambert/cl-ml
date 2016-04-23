;;-*- Mode: Lisp -*- 

(asdf:defsystem "optimization"
  :description "Numerical optimization"
  :version "0.1.0"
  :author "Ben Lambert"
  :serial t
  :components
  ((:module src
	    :serial t
	    :components ((:file "package")
			 (:file "common")
			 (:module linemin
				  :serial t
				  :components ((:file "common")
					       (:file "bracket-minimum")
					       (:file "golden-section")
					       (:file "brute")
					       (:file "brent")
					       (:file "dbrent")
					       (:file "parabolic-extrapolation")
					       (:file "wrapper")))
			 (:file "gradient")
			 (:module multi-dimensional
				  :serial t
				  :components ((:file "common")
					       (:file "convergence")
					       (:file "direction-set")
					       (:file "coordinate")
					       (:file "steepest")
					       (:file "powell")
					       (:file "conjugate-gradient")
					       (:file "bfgs")
					       (:file "lbfgs"))))))
  :depends-on (:lispdoc :blambert-util :split-sequence :alexandria :array-operations :gnuplot))


(defsystem :optimization-test
  :depends-on (:optimization)
  :components ((:module "test"
                        :serial t
                        :components ((:file "package")
                                     (:file "test")))))

(defmethod perform ((o test-op) (c (eql (find-system :optimization))))
  (operate 'load-op :optimization-test)
  (funcall (intern (symbol-name :run-all-tests) (find-package :optimization-test))))

