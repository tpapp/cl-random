(in-package #:cl-random-asd)

(defpackage #:cl-random
    (:nicknames :rv)
  (:shadow #:type)
  (:use :common-lisp
	:cl-utilities
        :iterate
        :bind
        :lla
        :xarray
        :anaphora
        :cl-num-utils)
  (:import-from :cl-num-utils #:mean #:variance #:sse)
  (:shadowing-import-from :iterate :collecting :collect)
  (:export

   ;; utilities

   missing not-implemented undefined positive-double-float vector-double-float
   vector-positive-double-float truncation-boundary

   ;; log-infinity

   plus-infinity ~+ ~- ~log ~exp

   ;; random

   rv generator generator* draw draw* pdf pdf* log-pdf log-pdf* 
   mean* variance* cdf cdf* dimensions univariate-continuous-generator
   univariate-discrete-generator
   univariate multivariate quantile quantile* type dimensions

   ;; univariate

   uniform left right draw-standard-exponential exponential beta
   normal mu sigma pdf-standard-normal cdf-standard-normal
   draw-standard-normal to-standard-normal from-standard-normal
   truncated-normal left right gamma alpha beta
   generator-standard-gamma inverse-gamma chi-square inverse-chi-square
   beta discrete probabilities

   ;; multivariate

   mv-normal mv-t linear-regression wishart nu scale inverse-wishart

   ;; statistics
   
   add-constant-column column-sums column-means demean-columns
   column-mean-variances empirical-quantiles

   ;; special-functions

   log-gamma gamma

   ))
