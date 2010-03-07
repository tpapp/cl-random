(in-package #:cl-random-asd)

(defpackage #:cl-random
    (:nicknames :rv)
  (:shadow #:type)
  (:use :common-lisp
	:cl-utilities
        :iterate
        :bind
        :lla
        :xarray)
  (:shadowing-import-from :iterate :collecting :collect)
  (:export

   ;; utilities

   missing not-implemented undefined positive-double-float vector-double-float
   vector-positive-double-float truncation-boundary

   ;; random

   rv generator draw draw* pdf pdf* mean mean* variance variance* cdf cdf*
   dimensions univariate-continuous-generator univariate-discrete-generator
   univariate multivariate quantile quantile* type dimensions

   ;; univariate

   uniform left right draw-standard-exponential exponential beta
   normal mu sigma pdf-standard-normal cdf-standard-normal
   draw-standard-normal to-standard-normal from-standard-normal
   truncated-normal left right gamma alpha beta
   generator-standard-gamma inverse-gamma chi-square inverse-chi-square
   beta discrete probabilities

   ;; multivariate

   mv-normal linear-regression tau wishart nu scale inverse-wishart
   inverse-scale

   ;; statistics
   
   demean-matrix matrix-mean-variance empirical-quantiles

   ))
