(defsystem :cl-random-tests
  :description "Unit tests for CL-RANDOM."
  :version "alpha"
  :author "Tamas K Papp <tkpapp@gmail.com"
  :license "Same as CL-RANDOM--this is part of the CL-RANDOM library."
  :serial t
  :components
  ((:module 
    "package-init"
    :pathname #P"tests/"
    :components
    ((:file "package")))
   (:module
    "utilities-and-setup"
    :pathname #P"tests/"
    :components
    ((:file "utilities")
     (:file "setup")))
   (:module 
    "tests"
    :pathname #P"tests/"
    :components
    ((:file "log-infinity")
     (:file "special-functions")
     (:file "univariate")
     (:file "continuous-time")
     (:file "multivariate")
     (:file "regressions"))))
  :depends-on
  (cl-utilities iterate metabang-bind anaphora lift cl-num-utils random-sample
                cl-random lla))
