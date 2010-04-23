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
        :documentation "posterior for precision tau")
   (R^2 :accessor R^2 :initarg :R^2 :documentation
        "coefficient of determination"))
  (:documentation "The random variates returned are samples from a
  posterior distribution of a Bayesian least squares model with the
  usual reference prior.  The sampled standard deviation (sigma) is
  returned as the second value by the generator or draw."))

(defun linear-regression (y x &key save-r^2?)
  (declare (optimize (debug 3)))
  (bind (((:values b qr ss nu) (least-squares y x))
         (sigma (least-squares-xx-inverse qr))
         (beta (make-instance 'mv-normal :mu b :sigma sigma))
         (tau (make-instance 'gamma :alpha (/ nu 2d0) :beta (/ ss 2))))
    (aprog1 (make-instance 'linear-regression :beta beta :tau tau)
      (when save-r^2?
        (let ((ss-total (vector-sum-of-squares (demean-vector y))))
          (setf (r^2 it) (- 1 (/ ss ss-total))))))))

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
   (scale-left-root :accessor scale-left-root)))

(defmethod initialize-instance :after ((rv wishart) &key &allow-other-keys)
  (with-slots (scale scale-left-root) rv 
    (check-type scale hermitian-matrix)
    (setf scale-left-root (component (cholesky scale :L) :L)))
  rv)

(defmethod dimensions ((rv wishart))
  (xdims (scale rv)))

(defmethod type ((rv wishart))
  'hermitian-matrix)

(defmethod mean ((rv wishart))
  (x* (nu rv) (scale rv)))

(defun draw-standard-wishart-left-root (nu k)
  "Draw a matrix L such that (mm L t) has Wishart(I,nu)
distribution (dimension k x k)."
  (check-type nu integer)
  (bind ((nu (coerce nu 'double-float))
         ((:lla-matrix l) (make-matrix :double k k :kind :lower-triangular)))
    (dotimes (i k)
      (setf (l (l-index i i)) (sqrt (draw* 'chi-square :nu (- nu i))))
      (iter
        (for l-index :from (l-index (1+ i) i) :below (l-index k i))
        (setf (l l-index) (draw-standard-normal))))
    l))

(cached-slot (rv wishart generator)
  (bind (((:slots-read-only nu scale-left-root) rv)
         (k (nrow (scale rv))))
    (lambda ()
      (mm (mm scale-left-root (draw-standard-wishart-left-root nu k)) t))))


;;;  INVERSE-WISHART
;;;
;;;  If A ~ Inverse-Wishart[nu,inverse-scale], then 
;;;  (invert A) ~ Wishart(nu,inverse-scale).

(defclass inverse-wishart (multivariate)
  ((nu :initarg :nu :reader nu :type fixnum :documentation "degrees of freedom")
   (inverse-scale :initarg :inverse-scale :reader inverse-scale
                  :type hermitian-matrix
                  :documentation "Inverse scale matrix, to which the
                  mean is proportional.")
   (inverse-scale-right-root
    :accessor inverse-scale-right-root
    :documentation "C, where (mm C t) is scale.")  )
  (:documentation "Inverse Wishart distribution.  The PDF p(X) is
proportional to |X|^-(dimension+nu+1)/2 exp(-trace(inverse-scale X^-1))"))

(defmethod initialize-instance :after ((rv inverse-wishart) &key &allow-other-keys)
  (with-slots (inverse-scale inverse-scale-right-root) rv 
    (check-type inverse-scale hermitian-matrix)
    (setf inverse-scale-right-root (component (cholesky inverse-scale :U) :U)))
  rv)

(defmethod dimensions ((rv inverse-wishart))
  (xdims (scale rv)))

(defmethod type ((rv inverse-wishart))
  'hermitian-matrix)

(defmethod mean ((rv inverse-wishart))
  (with-slots (nu inverse-scale) rv 
    (x/ inverse-scale (- nu (nrow inverse-scale) 1))))

(cached-slot (rv inverse-wishart generator)
  (bind (((:slots-read-only nu inverse-scale-right-root) rv)
         (k (nrow (inverse-scale rv))))
    (lambda ()
      (mm t (solve (draw-standard-wishart-left-root nu k) inverse-scale-right-root)))))
