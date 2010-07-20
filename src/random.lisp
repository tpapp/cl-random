(in-package :cl-random)

;;;; Abstract base class rv
;;;;
;;;; All random variables have a generator, which is a function with
;;;; no arguments, returning a single draw.  You can use the draw
;;;; method, or use the generator directly (if you are concerned about
;;;; speed).  Some common generator types are defined below.  Finally,
;;;; (draw* rv ...) is equivalent to (draw (make-instance 'rv ...)).

(define-abstract-class rv ()
  ((generator :reader generator :type (function () t)
   :documentation "A function without parameters that returns a random
   variable."))
  (:documentation "Base class for all random variables.
Implementation note: subclasses are allowed to calculate intermediate
values (eg to speed up computation) any time, eg right after the
initialization of the instance, or on demand.  The consequences or
changing the slots of RV classes are UNDEFINED, but probably quite
nasty.  DON'T DO IT."))

(define-abstract-class univariate (rv)
  ()
  (:documentation "Base class for univariate random variables.
  Univariate RV's have densities, distribution functions and
  quantiles."))

(define-abstract-class multivariate (rv)
  ;; NOTE: distribution functions could be defined for multivariate
  ;; RV's of course, but they are rarely used in practice and are a
  ;; PITA to provide.  Contact the package maintainers if you really
  ;; need them, but be prepared to (1) provide a really good argument
  ;; and (2) possibly contribute the relevant code and unit tests.
  ()
  (:documentation "Base class for multivariate random variables.
  Univariate RV's have densities, but NO distribution functions and
  quantiles."))

(def* generator () "Generator for a random variate of the given type and parameters.")

(defgeneric rv-type (rv)
  (:documentation "Type of objects returned by draw.")
  (:method (rv)
    t))

(defgeneric dimensions (rv)
  (:documentation "Dimensions of objects returned by draw if array or
  similar, otherwise nil (also for scalars, etc).")
  (:method ((rv univariate))
    nil))

(defgeneric draw (rv)
  (:documentation "Draw a random variate from rv.")
  (:method ((rv rv))
    (funcall (generator rv))))

(def* draw () "Draw a random variate of the given type and parameters.")

;;; Density functions
;;; 
;;; Since the there is a bijection between the pdf and the log pdf, only one of
;;; them needs to be provided.  Most applications work with logs because of
;;; overflows, so that is the primary one which needs to be defined, and the pdf
;;; is derived from that by default.

(defun scale-log-pdf (rv unscaled? unscaled)
  "Scale UNSCALED part of log PDF, depending on UNSCALED?."
  (if unscaled?
      unscaled
      (+ (log-pdf-constant rv) unscaled)))

(defclass log-pdf-constant ()
  ((log-pdf-constant :reader log-pdf-constant
                     :documentation "Log of the constant part of the PDF."))
  (:documentation "Mixin class, saving the log PDF constant for reuse."))

(defgeneric log-pdf (rv x &optional unscaled?)
  (:documentation "Log probability density function of rv evaluated at x.  When
  UNSCALED?, the implementation is allowed to drop the constant, but has to do
  it consistently for calls using the same RV.  When the PDF is zero, LOG-PDF
  returns NIL."))

(def* log-pdf (x) "Log PDF for a random variate of the given type and parameters.")

(defgeneric pdf (rv x &optional unscaled?)
  (:documentation "Probability density function of rv evaluated at x.  When
  UNSCALED?, the implementation is allowed to drop the constant.")
  (:method ((rv rv) x &optional unscaled?)
    (~exp (log-pdf rv x unscaled?))))

(def* pdf (x) "PDF for a random variate of the given type and parameters.")

(defgeneric cdf (rv x)
  (:documentation "Cumulative distribution function of rv evaluated at x.")
  (:method ((rv univariate) x)
    (error 'missing))
  (:method ((rv univariate) x)
    (error 'not-implemented)))

(def* cdf (x) "CDF (ie Pr(X <= x)) for a random variate of the given
type and parameters.")

(defgeneric quantile (rv q)
  (:documentation "Quantile function of rv evaluated at x.")
  (:method ((rv univariate) q)
    (error 'missing))
  (:method ((rv multivariate) q)
    (error 'undefined)))

(def* quantile (q) "Quantile for a random variate of the given type and parameters.")

(defmethod mean ((rv rv))
  (error 'missing))

(def* mean () "Mean for a random variate of the given type and parameters.")

(defmethod variance ((rv rv))
  (error 'missing))

(def* variance () "Variance for a random variate of the given type and parameters.")

;;;; Some commonly used generator types.
;;;;
;;;; The convention is returning double-floats and fixnums for
;;;; discrete and continuous arguments, respectively.  You can use
;;;; type declarations to speed things up if you are using the
;;;; generator directly.

(deftype univariate-continuous-generator ()
  '(function () double-float))

(deftype univariate-discrete-generator ()
  '(function () fixnum))
