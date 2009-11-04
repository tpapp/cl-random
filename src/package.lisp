(in-package #:cl-random-asd)

(defpackage #:cl-random
    (:nicknames :rv)
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

   rv draw draw* pdf pdf* mean mean* variance variance* cdf cdf*
   univariate-continuous-generator univariate-discrete-generator
   multivariate-continuous-generator multivariate-discrete-generator
   univariate multivariate quantile quantile* draw-type draw-dimensions
   draw-many draw-many*

   ;; univariate

   uniform draw-standard-exponential exponential normal
   pdf-standard-normal cdf-standard-normal draw-standard-normal
   to-standard-normal from-standard-normal truncated-normal gamma
   generator-standard-gamma beta discrete

   ;; multivariate

   mv-normal

   ))
