;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-random)

;;; Dummies from priors
;;; 
;;; Convert conjugate priors to dummy observations.

(defgeneric dummy-observations (prior &key &allow-other-keys)
  (:documentation "Return dummy observations (VALUES Y X) corresponding to a
  regression, or a random variable representing draws from a regression's
  posterior."))

(defun add-prior-dummies (y x prior &optional prior-type)
  "When PRIOR is non-nil, add dummies to Y and X, otherwise make no changes.
Return (VALUES Y X).  When PRIOR-TYPE is given, check that PRIOR is of that
type."
  (when (and prior prior-type)
    (assert (typep prior prior-type)))
  (if prior
      (bind (((:values y-dummy x-dummy) (dummy-observations prior)))
        (values (concat y y-dummy)
                (stack :matrix :vertically x x-dummy)))
      (values y x)))

;;; LINEAR-REGRESSION-KV
;;; 
;;; Linear regression with a known variance matrix.  Not used directly in
;;; practice, but useful for Gibbs sampling.

(defgeneric linear-regression-kv (y x &key variance-right-sqrt prior)
  (:documentation "Linear regression of Y on X with known variance matrix,
returned as an MV-NORMAL distribution (or NORMAL, if X is a vector).
VARIANCE-RIGHT-SQRT is the rigth square root of the variance matrix (or the
standard deviation if X is a vector), assumed to be the identity if not given.
Numbers are interpreted as the common variance.  PRIOR is used to generate dummy
observations with the eponymous function."))

(defmethod linear-regression-kv ((y vector) (x vector) &key
                                 variance-right-sqrt prior)
  (sub (linear-regression-kv y (as-column x) 
                             :variance-right-sqrt variance-right-sqrt
                             :prior prior)
       0))

(defmethod linear-regression-kv ((y vector) (x dense-matrix-like) &key
                                 variance-right-sqrt prior)
  (check-type y vector)
  (check-type x dense-matrix-like)
  ;; apply variance matrix if given
  (typecase variance-right-sqrt
    (null)                              ; do nothing
    (number (setf x (e/ x variance-right-sqrt)
                  y (e/ y variance-right-sqrt)))
    (t ;; ?? maybe we could stack and solve once, for efficiency
       (setf x (solve variance-right-sqrt x)
             y (solve variance-right-sqrt y))))
  ;; attach prior to transformed data; dummy observations don't need to be
  ;; rescaled as they come directly from the prior
  (bind (((:values y x) (add-prior-dummies y x prior '(or normal mv-normal)))
         ((:values beta nil nil other-values) (least-squares y x :method :qr)))
    (make-instance 'mv-normal :mean beta :variance-right-sqrt
                   (qr-xx-inverse-sqrt (getf other-values :qr)))))

(defmethod dummy-observations ((rv normal) &key)
  (bind (((:slots-r/o mu sigma) rv))
    (values (clo :double (/ mu sigma))
            (clo :double (/ sigma)))))

(defmethod dummy-observations ((rv mv-normal) &key)
  (bind (((:slots-r/o mean variance-right-sqrt) rv)
         (r-t (transpose variance-right-sqrt)))
    (values (solve r-t mean)
            (invert r-t))))

;;;  LINEAR-REGRESSION
;;;
;;;  This is a helper function to run obtain the posterior
;;;  distribution of linear regressions.

(defun xx-inverse-right-sqrt (x &optional (tolerance 0))
  "Calculate the right square root of (X'X)^-1, using SVD.  Tolerance is used
when inverting the singular values."
  (bind (((:values s nil vt) (svd x :right :all)))
    (mm (invert s :tolerance tolerance) vt)))

(defclass linear-regression (mv-t)
  ((s^2 :accessor s^2 :initarg :s^2 :documentation "sum of squared errors")
   (r^2 :accessor r^2 :initarg :r^2 :documentation "R^2"))
  (:documentation "Class representing linear regressions.  Basically a
  multivariate T distribution, but the generator returns the randomly drawn
  sigma^2 as the second value (scaled correctly by s^2)."))

(defmethod mean ((rv linear-regression))
  (bind (((:slots-r/o scaling-factor s^2) rv))
    (values (call-next-method rv)
            (* s^2 (mean scaling-factor)))))

(defmethod variance ((rv linear-regression))
    (bind (((:slots-r/o scaling-factor s^2) rv))
    (values (call-next-method rv)
            (* (expt s^2 2) (variance scaling-factor)))))

(define-cached-slot (rv linear-regression generator)
  (bind (((:slots-read-only scaling-factor mv-normal s^2) rv)
         (scaling-factor-generator (generator scaling-factor))
         (mv-normal-generator (generator mv-normal)))
    (lambda ()
      (let ((scaling-factor (funcall scaling-factor-generator)))
        (values 
          (funcall mv-normal-generator (sqrt scaling-factor))
          (* scaling-factor s^2))))))

(defmethod dummy-observations ((rv linear-regression) &key)
  (bind (((:accessors-r/o mv-normal s^2 nu) rv)
         (nu (as-integer nu))
         (s (sqrt s^2))
         ((:slots-r/o mean variance-right-sqrt) mv-normal)
         (k (length mean))
         (r-t (transpose (e/ variance-right-sqrt s))))
    (values (concat (solve r-t mean) (lla-vector nu :double s))
            (stack :matrix :vertically (invert r-t) 
                   (make-matrix nu k :double
                                :initial-element 0d0)))))

(defun linear-regression (y x &key r^2? (method :qr) prior)
  "Return the following values: 1. an MV-T distribution for drawing
  means from the distribution.  2. The mean of the variance posterior.
  Multiplied by the second value returned when drawing from the MV-T
  distribution, this yields the variance corresponding to that draw.
  3. When R^2?, return the R^2 value."
  (bind (((:values y x) (add-prior-dummies y x prior 'linear-regression))
         ((:values b ss nu other-values) (least-squares y x :method method))
         (s^2 (/ ss nu)))
    (aprog1
        (make-instance 'linear-regression
                       :mean b
                       :sigma-right-sqrt 
                       (e* (sqrt s^2)
                           (if (eq method :qr)
                               (qr-xx-inverse-sqrt (getf other-values :qr))
                               (xx-inverse-right-sqrt x)))
                       :nu nu
                       :s^2 s^2)
      (when r^2?
        (setf (r^2 it) (- 1 (/ ss (sse y))))))))

