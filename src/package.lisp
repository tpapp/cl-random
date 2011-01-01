(in-package #:cl-random-asd)

(defpackage #:cl-random
    (:nicknames :rv)
  (:use :common-lisp
        ;;	:cl-utilities
        :alexandria
        :iterate
        :bind
        :lla
        :anaphora
        :cl-num-utils)
  (:shadow :mean :variance)                   ; in alexandria
  (:shadowing-import-from :iterate :collecting :collect)
  (:shadowing-import-from :cl-num-utils :xor) ; also in alexandria
  (:export

   ;; utilities

   missing not-implemented undefined positive-double-float vector-double-float
   vector-positive-double-float truncation-boundary

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

   ;; multivariate

   ;; mv-normal variance-right-sqrt log-pdf-constant mv-t scaling-factor nu wishart
   ;; scale inverse-wishart

   ;; statistics
   
   ;; add-constant-column column-sums column-means demean-columns column-variances
   ;; column-mean-variances rescale-by-sd empirical-quantiles variance->correlation

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
