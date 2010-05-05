(defpackage #:cl-random-unit-tests
    (:use #:cl #:cl-utilities #:iterate #:metabang-bind #:cffi #:xarray
          #:anaphora #:named-readtables #:lift #:lla #:cl-random
          #:cl-num-utils)
  (:shadowing-import-from :iterate :collecting :collect)
  (:shadowing-import-from :cl-random #:type)
;  (:import-from cl-random)
  (:export run-cl-random-tests))
