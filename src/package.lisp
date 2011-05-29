(defpackage cl-random
  (:nicknames rv)
  (:use common-lisp alexandria iterate let-plus anaphora cl-num-utils lla)
  (:shadowing-import-from cl-num-utils xor mean variance) ; also in alexandria
  (:export

   ;; utilities

   missing not-implemented undefined positive-double-float as-double-float 
   double-float-vector as-double-float-vector truncation-boundary
   as-double-float-probabilities

   ;; log-infinity

   plus-infinity ~+ ~- ~log ~exp

   ;; special-functions

   log-gamma gamma

   ;; random

   draw generator replicating cdf log-pdf pdf quantile

   ;; univariate

   r-uniform left right
   r-exponential beta draw-standard-exponential
   r-normal mean sd draw-standard-normal
   r-log-normal log-mean log-sd
   r-t scale t-scale-to-variance-coefficient
   r-gamma alpha beta r-inverse-gamma 
   r-chi-square r-chi-square-nu 
   r-inverse-chi-square r-inverse-chi-square-nu r-inverse-chi-square-scale
   r-inverse-chi-square-s^2
   r-beta
   r-discrete probabilities

   ;; continuous-time
   
   r-uniformized-markov-jump

   ;; multivariate

   r-multivariate-normal variance-left-sqrt
   r-multivariate-t nu
   r-wishart scale-left-sqrt r-inverse-wishart inverse-scale-right-sqrt

   ;; statistics

   matrix-mean demean-matrix matrix-sse matrix-variance
   matrix-mean-and-variance
   
   ;; add-constant-column column-sums column-means demean-columns column-variances
   ;; column-mean-variances rescale-by-sd variance->correlation

   ;; regressions

   add-regression-dummies linear-regression-dummies linear-regression
   posterior r^2 s^2 as-regression-covariates transform-y-x

   ;; dummy-observations linear-regression-kv xx-inverse-right-sqrt
   ;; linear-regression s^2 r^2

   ;; design-matrix
   
   ;; design-matrix

   ;; optimization

   ;; bfgs-objective simple-double-vector richardson-derivative3 num-gradient
   ;; num-hessian bfgs-parameters linesearch-max-iter numdiff-epsilon relative rho
   ;; sigma alpha-max tau1 tau2 tau3 kappa step-reduction max-bisections
   ;; max-expansions *default-bfgs-parameters* bfgs-minimize

   ))
