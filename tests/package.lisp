(defpackage cl-random-tests
  (:use #:cl #:alexandria #:iterate #:let-plus #:anaphora #:lift #:cl-random
        #:cl-num-utils #:lla)
  (:shadowing-import-from #:cl-random #:mean #:variance #:median ; also in ALEXANDRIA
                          #:displace-array ; no longer in ALEXANDRIA, TODO remove in 2012 June
                          #:sum         ; also in ITERATE
                          )
  (:export #:random-y-x #:run)
  (:import-from #:cl-num-utils-tests #:vector* #:array*))
