(defpackage #:cl-random
  (:nicknames #:rv)
  (:use #:common-lisp #:alexandria #:iterate #:let-plus #:anaphora
        #:cl-num-utils #:lla)
  (:shadowing-import-from #:cl-num-utils #:mean #:variance #:median ; also in alexandria
                          #:displace-array ; no londer in ALEXANDRIA TODO remove in 2012 June
                          #:sum         ; also in ITERATE
                          )
  (:export
   ;; utilities
   #:missing
   #:not-implemented
   #:undefined
   #:positive-double-float
   #:as-double-float
   #:double-float-vector
   #:as-double-float-vector
   #:truncation-boundary
   #:as-double-float-probabilities
   ;; log-infinity
   #:plus-infinity
   #:~+
   #:~-
   #:~log
   #:~exp
   ;; special-functions
   #:log-gamma
   #:gamma
   ;; random
   #:draw
   #:generator
   #:replicating
   #:cdf
   #:log-pdf
   #:pdf
   #:quantile
   ;; discrete
   #:draw-bernoulli
   #:draw-bernoulli-rational
   #:distinct-random-integers
   ;; univariate
   #:r-uniform
   #:left
   #:right
   #:r-exponential
   #:rate
   #:draw-standard-exponential
   #:r-normal
   #:mean
   #:sd
   #:draw-standard-normal
   #:r-log-normal
   #:log-mean
   #:log-sd
   #:r-t
   #:scale
   #:t-scale-to-variance-coefficient
   #:r-gamma
   #:alpha
   #:beta
   #:r-inverse-gamma
   #:r-chi-square
   #:r-chi-square-nu
   #:r-inverse-chi-square
   #:r-inverse-chi-square-nu
   #:r-inverse-chi-square-scale
   #:r-inverse-chi-square-s^2
   #:r-beta
   #:r-discrete
   #:probabilities
   #:r-truncated-normal
   ;; continuous-time
   #:r-uniformized-markov-jump
   ;; multivariate
   #:r-multivariate-normal
   #:variance-left-sqrt
   #:r-multivariate-t
   #:scaling-factor
   #:multivariate-normal
   #:nu
   #:r-wishart
   #:scale-left-sqrt
   #:r-inverse-wishart
   #:inverse-scale-right-sqrt
   ;; statistics
   #:matrix-mean
   #:demean-matrix
   #:matrix-sse
   #:matrix-variance
   #:matrix-mean-and-variance
   #:add-regression-dummies
   #:linear-regression-dummies
   #:linear-regression
   #:posterior
   #:r^2
   #:s^2
   #:as-regression-covariates
   #:transform-y-x
   ))
