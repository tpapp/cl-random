(in-package :cl-random)

;;;;
;;;;  MULTIVARIATE-NORMAL distribution
;;;;
;;;;  The generator of this distribution allows to draws to be
;;;;  multiplied by a scale factor, which is useful for sampling from
;;;;  posteriors, etc.

(defclass mv-normal (multivariate)
  ((mu :initarg :mu :reader mu :type numeric-vector-double
       :documentation "vector of means")
   (sigma :initarg :sigma :reader sigma :type lower-triangular-matrix
            :documentation "cholesky decomposition of variance, ie
            sigmasq sigmasq^T = variance.  Can be calculated from
            variance matrix using a lla:cholesky.  Not checked for
            PSD, only for type dimensions.")))

(defmethod initialize-instance :after ((rv mv-normal) &key &allow-other-keys)
  (with-slots (mu sigma) rv
    (setf mu (take 'numeric-vector-double mu)
          sigma (take 'lower-triangular-matrix sigma))
    (bind ((n (xdim mu 0))
           ((n1 n2) (xdims sigma)))
      (unless (and (plusp n) (= n n1 n2))
        (error "incompatible dimensions for mu and/or sigma")))
    rv))

(define-printer (mv-normal)
    (format stream "~&MEAN: ~A~%VARIANCE:~%~A~%" (mean rv) (variance rv)))

(defmethod mean ((rv mv-normal))
  (mu rv))

(defmethod variance ((rv mv-normal))
  (update-syhe (sigma rv) 'symmetric-matrix nil))

;;;; !!!! define at least pdf

(cached-slot (rv mv-normal generator)
  (bind (((:slots-read-only mu sigma) rv)
         (n (xdim mu 0)))
    (lambda (&optional (scale 1d0))
      (let* ((x (make-nv n :double))
             (x-data (nv-data x)))
        (dotimes (i n)
          (setf (aref x-data i) (draw-standard-normal)))
        (xmap 'numeric-vector-double #'+
              mu (mm sigma x :alpha scale))))))

;;;;
;;;;  LINEAR-REGRESSION
;;;;
;;;;  This distribution is for drawing from the posterior (beta,sigma)
;;;;  of a linear regression y = X beta + epsilon, where epsilon ~
;;;;  N(0,sigma^2).  Internally, the implementation uses the precision
;;;;  tau = sigma^(-2).  See Lancaster (2004, Chapter 3) for the
;;;;  formulas.
;;;;
;;;;  LINEAR-REGRESSION behaves as if the random variable was BETA for
;;;;  calculating the mean, variance, drawing random values, etc.  For
;;;;  the latter, the generator returns a value of SIGMA as a second
;;;;  value.

(defclass linear-regression (multivariate)
  ((beta :type mv-normal :reader beta :initarg :beta
         :documentation "\"raw\" conditional posterior for beta, needs to
                        be scaled up by (sqrt tau)")
   (tau :type gamma :reader tau :initarg :tau
        :documentation "posterior for precision tau"))
  (:documentation "The random variates returned are samples from a
  posterior distribution of a Bayesian least squares model with the
  usual reference prior.  The sampled standard deviation (sigma) is
  returned as the second value by the generator or draw."))

(defun linear-regression (y x)
  (bind (((:values b qr ss nu) (least-squares x y))
         (sigma (take 'lower-triangular-matrix (least-squares-raw-variance qr)))
         (beta (make-instance 'mv-normal :mu b :sigma sigma))
         (tau (make-instance 'gamma :alpha (/ nu 2d0) :beta (/ ss 2))))
    (make-instance 'linear-regression :beta beta :tau tau)))

(defmethod mean ((rv linear-regression))
  (mean (beta rv)))

(defmethod variance ((rv linear-regression))
  (x* (variance (beta rv)) (/ (mean (tau rv)))))

(cached-slot (rv linear-regression generator)
  (bind (((:slots-read-only beta tau) rv)
         (beta-generator (generator beta))
         (tau-generator (generator tau)))
    (lambda ()
      (let ((sigma (/ (sqrt (funcall tau-generator)))))
        (values (funcall beta-generator sigma)
                sigma)))))
   
;;;; ??? implement pdf -- Tamas

