(defpackage cl-random-tests
  (:use #:cl #:alexandria #:iterate #:let-plus #:anaphora #:lift #:cl-random
        #:cl-num-utils #:lla #:transit)
  (:shadowing-import-from #:cl-random #:mean #:variance #:median)
  (:export #:random-y-x #:run)
  (:import-from #:cl-num-utils-tests #:vector* #:array*))
