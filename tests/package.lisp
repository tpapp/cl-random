(defpackage cl-random-tests
  (:use #:cl
        #:alexandria
        #:anaphora
        #:cl-num-utils.elementwise
        #:cl-num-utils.matrix-shorthand
        #:cl-random
        #:iterate
        #:let-plus
        #:lift
        #:lla
        )
  (:shadowing-import-from #:cl-num-utils #:mean #:variance #:median ; also in ALEXANDRIA
                          #:sum         ; also in ITERATE
                          )
  (:export #:random-y-x #:run))
