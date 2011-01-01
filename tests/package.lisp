(defpackage #:cl-random-tests
    (:use #:cl #:cl-utilities #:iterate #:metabang-bind #:anaphora #:lift #:lla
          #:cl-random #:cl-num-utils #:tpapp-utils)
  (:shadowing-import-from :iterate :collecting :collect)
  (:shadowing-import-from :cl-random #:mean)
;  (:import-from cl-random)
  (:export run-cl-random-tests))
