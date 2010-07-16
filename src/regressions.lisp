;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-random)

(defgeneric dummy-observations (object &key &allow-other-keys)
  (:documentation "Return dummy observations corresponding to a regression, or a
  random variable representing draws from a regression's posterior.  Return
  (values Y X)."))

(defmethod dummy-observations ((rv mv-normal) &key)
  (bind (((:slots-r/o mean variance-right-sqrt) rv)
         (r-t (transpose variance-right-sqrt)))
    (values (solve r-t mean)
            (invert r-t))))

;;; LINEAR-REGRESSION-KV
;;; 
;;; Linear regression with a known variance matrix.  Not used directly in
;;; practice, but useful for Gibbs sampling.


(defun linear-regression-kv (y x &key variance-right-sqrt prior)
  "Linear regression of Y on X with known variance matrix.  VARIANCE-RIGHT-SQRT
is the rigth square root of the variance matrix, assumed to be the identity if
not given.  PRIOR is used to generate dummy observations with the eponymous
function."
  (check-type y vector)
  (check-type x dense-matrix-like)
  ;; apply variance matrix if given
  (when variance-right-sqrt
    ;; ?? maybe we could stack and solve once, for efficiency
    (setf x (solve variance-right-sqrt x))
    (setf y (solve variance-right-sqrt y)))
  ;; attach prior to transformed data; dummy observations don't need to be
  ;; rescaled as they come directly from the prior
  (when prior
    (bind (((:values y-dummy x-dummy) (dummy-observations prior)))
      (setf x (stack-vertically x x-dummy)
            y (concat y y-dummy))))
  (bind (((:values beta nil nil other-values) (least-squares y x :method :qr)))
    (make-instance 'mv-normal :mean beta :variance-right-sqrt
                   (qr-xx-inverse-sqrt (getf other-values :qr)))))


;;;  LINEAR-REGRESSION
;;;
;;;  This is a helper function to run obtain the posterior
;;;  distribution of linear regressions.

(defun xx-inverse-right-sqrt (x &optional (tolerance 0))
  "Calculate the right square root of (X'X)^-1, using SVD.  Tolerance is used
when inverting the singular values."
  (bind (((:values s nil vt) (svd x :right :all)))
    (mm (invert s :tolerance tolerance) vt)))

(defun linear-regression (y x &key r^2? (method :qr))
  "Return the following values: 1. an MV-T distribution for drawing
  means from the distribution.  2. The mean of the variance posterior.
  Multiplied by the second value returned when drawing from the MV-T
  distribution, this yields the variance corresponding to that draw.
  3. When R^2?, return the R^2 value."
  (bind (((:values b ss nu) (least-squares y x :method method))
         (s^2 (/ ss nu)))
    (values
      (make-instance 'mv-t :mean b
                     :sigma-right-sqrt (e* (sqrt s^2) 
                                           (xx-inverse-right-sqrt x))
                     :nu nu)
      s^2
      (when r^2?
        (- 1 (/ ss (sse y)))))))

