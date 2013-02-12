(defpackage #:cl-random
  (:nicknames #:rv)
  (:use #:common-lisp
        #:alexandria
        #:anaphora
        #:cl-num-utils.num=
        #:cl-random.internals
        #:let-plus
        #:lla)
  (:shadow #:mean #:variance)           ; also in ALEXANDRIA
  ;;  ;; continuous-time
  ;;  #:r-uniformized-markov-jump
  ;;  ;; multivariate
  ;;  #:r-multivariate-normal
  ;;  #:variance-left-sqrt
  ;;  #:r-multivariate-t
  ;;  #:scaling-factor
  ;;  #:multivariate-normal
  ;;  #:nu
  ;;  #:r-wishart
  ;;  #:scale-left-sqrt
  ;;  #:r-inverse-wishart
  ;;  #:inverse-scale-right-sqrt
  ;;  ;; statistics
  ;;  #:matrix-mean
  ;;  #:demean-matrix
  ;;  #:matrix-sse
  ;;  #:matrix-variance
  ;;  #:matrix-mean-and-variance
  ;;  #:add-regression-dummies
  ;;  #:linear-regression-dummies
  ;;  #:linear-regression
  ;;  #:posterior
  ;;  #:r^2
  ;;  #:s^2
  ;;  #:as-regression-covariates
  ;;  #:transform-y-x
  ;;  #:check-probability
  ;;  #:r-dirichlet)
  (:export                              ; general interface
   #:draw
   #:generator
   #:mean
   #:variance
   #:cdf
   #:quantile
   #:log-pdf
   #:pdf)
  (:export                              ; discrete
   #:draw-bernoulli
   #:distinct-random-integers
   #:distinct-random-integers-dense)
  (:export                              ; univariate
   #:r-uniform
   #:left
   #:right
   #:r-exponential
   #:rate
   #:draw-standard-exponential
   #:draw-standard-normal
   #:to-standard-normal
   #:from-standard-normal
   #:r-normal
   #:r-truncated-normal
   #:r-log-normal
   #:log-mean
   #:log-sd
   #:t-scale-to-variance-coefficient
   #:draw-standard-t
   #:r-t
   #:scale
   #:nu
   #:r-gamma
   #:alpha
   #:beta
   #:r-inverse-gamma
   #:r-chi-square
   #:r-inverse-chi-square
   #:r-beta
   #:r-discrete
   #:probabilities))
