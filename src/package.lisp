(in-package #:cl-random-asd)

(defpackage #:cl-random
    (:nicknames :rv)
  (:use :common-lisp
        :alexandria
        :iterate
        :bind
        :anaphora
        :cl-num-utils)
  (:shadowing-import-from cl-num-utils xor mean variance) ; also in alexandria
  (:export

   ;; utilities

   missing not-implemented undefined positive-double-float as-double-float 
   double-float-vector as-double-float-vector truncation-boundary
   as-double-float-probabilities

   ;; log-infinity

   plus-infinity ~+ ~- ~log ~exp

   ;; special-functions

   log-gamma-function gamma-function

   ;; random

   rv generator mean variance cdf log-pdf pdf quantile

   ;; univariate

   uniform left right
   exponential beta
   normal mean sd
   log-normal log-mean log-sd
   gamma alpha beta inverse-gamma chi-square nu inverse-chi-square scale
   discrete probabilities

   ;; multivariate

   ;; mv-normal variance-right-sqrt log-pdf-constant mv-t scaling-factor nu wishart
   ;; scale inverse-wishart

   ;; statistics
   
   ;; add-constant-column column-sums column-means demean-columns column-variances
   ;; column-mean-variances rescale-by-sd variance->correlation

   ;; regressions

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
