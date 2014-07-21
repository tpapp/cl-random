(asdf:defsystem #:cl-random
  :description "Random numbers and distributions."
  :author "Tamas K Papp"
  :license "MIT"
  :version "0.0.1"
  :depends-on (#:alexandria
               #:anaphora
               #:array-operations
               #:cl-num-utils
               #:cl-rmath
               #:cl-slice
	       #:gsll
               #:let-plus
               #:lla)
  :pathname #P"src/"
  :serial t
  :components
  ((:file "internals")
   (:file "package")
   (:file "random-number-generator")
   (:file "random")
   (:file "discrete")
   (:file "univariate")
   (:file "continuous-time")
   ;; (:file "statistics")
   ;; (:file "multivariate")
   ;; ;; (:file "design-matrix")
   ;; (:file "regressions")
   ;; (:file "optimization")
   )
  )

(asdf:defsystem #:cl-random-tests
  :description "Unit tests for CL-RANDOM."
  :author "Tamas K Papp <tkpapp@gmail.com"
  :license "Same as CL-RANDOM, this is part of the CL-RANDOM library."
  :depends-on (#:cl-random
               #:clunit)
  :pathname #P"tests/"
  :serial t
  :components
  ((:file "setup")
   (:file "discrete")
   (:file "univariate")
   (:file "continuous-time")
   ;; (:file "multivariate")
   ;; (:file "regressions")
   ;; (:file "statistics")
))
