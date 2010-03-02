(in-package :cl-random)

;;;;
;;;;  MULTIVARIATE-NORMAL distribution
;;;;
;;;;  The generator of this distribution allows to draws to be
;;;;  multiplied by a scale factor, which is useful for sampling from
;;;;  posteriors, etc.

(defclass mv-normal (multivariate)
  ((mu :initarg :mu :reader mu :type numeric-vector
       :documentation "vector of means")
   (sigma :initarg :sigma :reader sigma
          :type hermitian-matrix
          :documentation "variance matrix")
   (sigma-sqrt :initarg :sigma-sqrt :reader sigma-sqrt :type dense-matrix
               :documentation "(right) square root of sigma, ie M such that M^T M=sigma")))

;; (define-modify-macro multf (factor) *)

;; (defun sigma-sqrt (sigma)
;;   "Calculate matrix square root of sigma."
;;   (bind (((:values val vec) (eigen sigma :vectors-p t))
;;          ((:slots-read-only (n nrow) (vec-elements elements)) vec)
;;          (val-elements (elements val)))
;;     ;; we can modify destructively, since vec is freshly created
;;     (dotimes (i n)
;;       (let ((sqrt-diag (sqrt (aref val-elements i))))
;;         (iter
;;           (for j :from (cm-index2 n 0 i) :below (cm-index2 n n i))
;;           (multf (aref vec-elements j) sqrt-diag))))
;;     vec))

(defmethod initialize-instance :after ((rv mv-normal) &key &allow-other-keys)
  (let ((sigma-or-sigma-sqrt 
         (cond 
           ((slot-boundp rv 'sigma) (sigma rv))
           ((slot-boundp rv 'sigma-sqrt) (sigma-sqrt rv))
           (t (error "At least one of SIGMA or SIGMA-SQRT has to be provided.")))))
    (assert (square-matrix-p sigma-or-sigma-sqrt) ()  "SIGMA/SIGMA-SQRT has to be a square matrix.")
    (unless (slot-boundp rv 'mu)
      (setf (slot-value rv 'mu) (make-nv (lla-type sigma-or-sigma-sqrt) (nrow sigma-or-sigma-sqrt))))
    rv))

(cached-slot (rv mv-normal sigma)
  (mm t (sigma-sqrt rv)))

(cached-slot (rv mv-normal sigma-sqrt)
  (factor (cholesky (sigma rv) :U)))

(define-printer (mv-normal)
    (format stream "~&MEAN: ~A~%VARIANCE:~%~A~%" (mean rv) (variance rv)))

(defmethod dimensions ((rv mv-normal))
  (xdims (sigma-sqrt rv)))

(defmethod type ((rv mv-normal))
  'numeric-vector)

(defmethod mean ((rv mv-normal))
  (mu rv))

(defmethod variance ((rv mv-normal))
  (sigma rv))

;;;; !!!! define at least pdf

(cached-slot (rv mv-normal generator)
  (bind (((:slots-read-only mu sigma-sqrt) rv)
         (n (xdim mu 0)))
    (lambda (&optional (scale 1d0))
      (let* ((x (make-nv :double n))
             (x-elements (elements x)))
        (dotimes (i n)
          (setf (aref x-elements i) (draw-standard-normal)))
        (x+ mu (mm x sigma-sqrt scale))))))

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
         (sigma (least-squares-xx-inverse qr :reconstruct-p nil)) ; Cholesky decomposition
         (beta (make-instance 'mv-normal :mu b :sigma-sqrt (component sigma :U)))
         (tau (make-instance 'gamma :alpha (/ nu 2d0) :beta (/ ss 2))))
    (make-instance 'linear-regression :beta beta :tau tau)))

(defmethod dimensions ((rv linear-regression))
  (values (dimensions (beta rv)) nil))

(defmethod type ((rv linear-regression))
  (values 'numeric-vector 'double-float))

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

;;;  WISHART
;;;
;;;  The k-dimensional Wishart distribution with NU degrees of freedom
;;;  and scale parameter SCALE is the multivariate generalization of
;;;  the gamma (or chi-square) distribution.

(defclass wishart (multivariate)
  ((nu :initarg :nu :reader nu :type fixnum :documentation "degrees of freedom")
   (scale :initarg :scale :reader scale
          :type hermitian-matrix
          :documentation "scale matrix")
   (mv-normal :reader mv-normal :type mv-normal
              :documentation "multivariate normal for drawing")))

(defmethod initialize-instance :after ((rv wishart) &key &allow-other-keys)
  (with-slots (scale mv-normal) rv 
    (check-type scale hermitian-matrix)
    (setf mv-normal (make-instance 'mv-normal :sigma scale)))
  rv)

(defmethod dimensions ((rv wishart))
  (xdims (scale rv)))

(defmethod type ((rv wishart))
  'hermitian-matrix)

(defmethod mean ((rv wishart))
  (x* (nu rv) (scale rv)))

(defun draw-wishart (nu mv-normal)
  (bind (((k nil) (dimensions mv-normal))
         ((:lla-matrix w) (make-matrix :double k k :kind :hermitian)))
    (assert (>= nu k) () "Generator not defined for NU < K.")
    (dotimes (i nu)
      (bind (((:lla-vector a) (draw mv-normal)))
        (dotimes (col k)
          (iter
            (for row :from 0 :to col)
            (incf (w row col) (* (a row) (a col)))))))
    w))

(cached-slot (rv wishart generator)
  (bind (((:slots-read-only nu mv-normal) rv))
    (lambda ()
      (draw-wishart nu mv-normal))))
