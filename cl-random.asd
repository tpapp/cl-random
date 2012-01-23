(asdf:defsystem #:cl-random
  :description "Random numbers and distributions."
  :author "Tamas K Papp"
  :license "MIT"
  :version "0.0.1"
  :serial t
  :components
  ((:module
    "package-init"
    :pathname #P"src/"
    :components
    ((:file "package")))
   (:module
    "basics"
    :pathname #P"src/"
    :serial t
    :components
    ((:file "utilities")
     (:file "log-infinity")
     (:file "special-functions")
     (:file "random")
     (:file "discrete")
     (:file "univariate")
     (:file "continuous-time")
     (:file "statistics")
     (:file "multivariate")
     ;; (:file "design-matrix")
     (:file "regressions")
     ;; (:file "optimization")
)))
  :depends-on
  (#:alexandria #:cl-num-utils #:iterate #:let-plus #:anaphora #:lla
                #:cl-rmath))

(asdf:defsystem #:cl-random-tests
  :description "Unit tests for CL-RANDOM."
  :author "Tamas K Papp <tkpapp@gmail.com"
  :license "Same as CL-RANDOM, this is part of the CL-RANDOM library."
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
     (:file "discrete")
     (:file "univariate")
     (:file "continuous-time")
     (:file "multivariate")
     (:file "regressions")
     (:file "statistics"))))
  :depends-on
  (#:cl-utilities #:iterate #:metabang-bind #:anaphora #:lift #:cl-num-utils
                  #:cl-random #:lla #:cl-num-utils-tests))
