(defpackage cl-random-tests
  (:use cl alexandria iterate let-plus anaphora lift cl-random
        cl-num-utils lla)
  (:shadowing-import-from cl-random mean variance)
  (:export random-y-x run-cl-random-tests))
