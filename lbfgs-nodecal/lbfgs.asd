
(defsystem "lbfgs"
  :description "Limited-memory BFGS optimization"
  :version "0.1"
  :author "Various"
  :licence "GPL?"
  :components
  ((:module src
	    :components
	    ((:file "lb1")
	     (:file "lb2")
	     (:file "lbfgs-ddot")
	     (:file "mcstep")
	     (:file "mcsrch")
	     (:file "lbfgs-daxpy")
	     (:file "lbfgs")
	     (:file "main"))))
  :depends-on (:f2cl))
