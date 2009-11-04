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
     (:file "random")
     (:file "univariate")
     (:file "multivariate")))
   (:module
    "statistics"
    :pathname #P"src/"
    :serial t
    :components
    ((:file "statistics"))))
  :depends-on (:cl-utilities :iterate :metabang-bind :xarray :lla :lift))

