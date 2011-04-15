(defpackage cl-random-tests
    (:use cl alexandria iterate metabang-bind anaphora lift cl-random cl-num-utils)
  (:shadowing-import-from cl-random mean variance)
  (:export run-cl-random-tests))
