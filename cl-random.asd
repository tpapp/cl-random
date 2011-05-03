(defpackage #:cl-random-asd
    (:use :cl :asdf))

(in-package #:cl-random-asd)

(defsystem #:cl-random
    :description "Random numbers and distributions."
  :author "Tamas K Papp"
  :license "MIT"
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
     (:file "univariate")
     (:file "continuous-time")
     ;; (:file "statistics")
     ;; (:file "multivariate")
     ;; (:file "design-matrix")
     ;; (:file "regressions")
     ;; (:file "optimization")
)))
  :depends-on 
  (alexandria cl-num-utils iterate metabang-bind anaphora lla))

