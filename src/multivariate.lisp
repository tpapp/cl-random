(in-package :cl-random)

(defun check-mean-variance-compatibility (mean variance)
  "Assert that the mean is a vector, and its dimensions are compatible with
variance."
  (assert (and (vectorp mean) 
               (= (length mean) (nrow variance) (ncol variance)))))

(defun normal-quadratic-form (x mean variance-left-sqrt)
  "Calculate (x-mean)^T variance^-1 (x-mean), given X, MEAN, and the left square root
of variance."
  (dot (solve variance-left-sqrt (e- x mean)) t))

(define-rv r-multivariate-normal (mean variance)
  (:documentation "Multivariate normal MVN(mean,variance) distribution."
   :instance instance)
  ((n :type fixnum :documentation "Number of dimensions.")
   (mean :type vector :reader t)
   (variance-left-sqrt :documentation "Left square root of the variance matrix."
                       :reader t))
  (let ((variance-left-sqrt 
         (etypecase variance
           (hermitian-matrix (values (left-square-root (cholesky variance))))
           (square-root (left-square-root variance)))))
    (check-mean-variance-compatibility mean variance-left-sqrt)
    (make :mean mean :variance-left-sqrt variance-left-sqrt :n (length mean)))
  (variance () (mm variance-left-sqrt t))
  (log-pdf (x &optional ignore-constant?)
           (maybe-ignore-constant
            ignore-constant?
            (* -0.5d0 (normal-quadratic-form x mean
                                             variance-left-sqrt))
            (- (/ (* (log (* 2 pi)) n) -2)
               (logdet variance-left-sqrt))))
  (draw (&key (scale 1d0))
        (let* ((x (make-array n :element-type 'double-float)))
          (dotimes (i n)
            (setf (aref x i) (draw-standard-normal)))
          (e+ mean (mm variance-left-sqrt x scale))))
  (sub (&rest index-specifications)
       (bind (((index-specification) index-specifications)
              (mean (sub mean index-specification))
              (variance (sub (variance instance) index-specification)))
         (if (vectorp mean)
             (r-multivariate-normal mean variance)
             (r-normal mean (sqrt variance))))))

;;;  MULTIVARIATE T distribution
;;;
;;;  When drawing numbers, the scaling factor (with distribution
;;;  inverse-chi-square, nu degrees of freedom) is returned as the second value.

(define-rv r-multivariate-t (mean sigma nu &key multivariate-normal scaling-factor)
  (:documentation "Multivariate T distribution with given MEAN, scale SIGMA and NU
  degrees of freedom.")
  ((multivariate-normal :type r-multivariate-normal :documentation
              "distribution for obtaining normal draws")
   (scaling-factor :type r-inverse-gamma :documentation
                   "distribution that scales the variance of draws.")
   (nu :type double-float :documentation "degrees of freedom"))
  (bind (((:values nu scaling-factor)
          (aif scaling-factor
               (progn
                 (check-type it r-inverse-gamma)
                 (assert (not nu) ()
                         "Can't initialize with both NU and SCALING-FACTOR.")
                 (values (* 2 (alpha it)) it))
             (with-doubles (nu)
               (values nu (r-inverse-chi-square nu))))))
    (make :multivariate-normal 
          (aif multivariate-normal
               (prog1 it
                 (check-type it r-multivariate-normal)
                 (assert (not (or mean sigma)) () "Can't initialize with both
                       MEAN & SIGMA and MULTIVARIATE-NORMAL."))
               (r-multivariate-normal mean sigma))
          :scaling-factor scaling-factor :nu nu))
  (mean () 
        (assert (< 1 nu))
        (mean multivariate-normal))
  (variance ()
            (assert (< 2 nu))
            (e* (variance multivariate-normal)
                (/ nu (- nu 2d0))))
  (log-pdf (x &optional ignore-constant?)
           (bind (((:accessors-r/o mean variance-left-sqrt) multivariate-normal)
                  (d (length mean)))
             (maybe-ignore-constant
              ignore-constant?
              (* (log (1+ (/ (normal-quadratic-form x mean variance-left-sqrt) nu)))
                 (/ (+ nu d) -2d0))
              (- (log-gamma (/ (+ nu d) 2d0))
                 (log-gamma (/ nu 2d0))
                 (* (+ (log nu) (log pi)) (/ d 2d0))
                 (logdet variance-left-sqrt)))))
  (draw (&key)
        (let ((scaling-factor (draw scaling-factor)))
          (values (draw multivariate-normal :scale (sqrt scaling-factor))
                  scaling-factor)))
  (sub (&rest index-specifications)
       (bind (((index-specification) index-specifications)
              (normal (sub multivariate-normal index-specification)))
         (etypecase normal
           (r-normal (r-t (mean normal) (sd normal) nu))
           (r-multivariate-normal 
              (r-multivariate-t nil nil nil
                                :multivariate-normal normal
                                :scaling-factor scaling-factor))))))

 ;; ;;;  WISHART
;; ;;;
;; ;;;  The k-dimensional Wishart distribution with NU degrees of freedom
;; ;;;  and scale parameter SCALE is the multivariate generalization of
;; ;;;  the gamma (or chi-square) distribution.

;; (defclass wishart (multivariate)
;;   ((nu :initarg :nu :reader nu :type fixnum :documentation "degrees of freedom")
;;    (scale :initarg :scale :reader scale
;;           :type hermitian-matrix
;;           :documentation "scale matrix")
;;    (scale-left-root :accessor scale-left-root)))

;; (defmethod initialize-instance :after ((rv wishart) &key &allow-other-keys)
;;   (with-slots (scale scale-left-root) rv 
;;     (check-type scale hermitian-matrix)
;;     (setf scale-left-root (component (cholesky scale :L) :L)))
;;   rv)

;; (defmethod dimensions ((rv wishart))
;;   (bind ((n (nrow (scale rv))))
;;     (list n n)))

;; (defmethod rv-type ((rv wishart))
;;   'hermitian-matrix)

;; (defmethod mean ((rv wishart))
;;   (e* (nu rv) (scale rv)))

;; (defun draw-standard-wishart-left-root (nu k)
;;   "Draw a matrix L such that (mm L t) has Wishart(I,nu)
;; distribution (dimension k x k)."
;;   (check-type nu integer)
;;   (bind ((nu (coerce nu 'double-float))
;;          ((:lla-matrix l) (make-matrix k k :double :kind :lower-triangular)))
;;     (dotimes (i k)
;;       (setf (l (l-index i i)) (sqrt (draw* 'chi-square :nu (- nu i))))
;;       (iter
;;         (for l-index :from (l-index (1+ i) i) :below (l-index k i))
;;         (setf (l l-index) (draw-standard-normal))))
;;     l))

;; (define-cached-slot (rv wishart generator)
;;   (bind (((:slots-read-only nu scale-left-root) rv)
;;          (k (nrow (scale rv))))
;;     (lambda ()
;;       (mm (mm scale-left-root (draw-standard-wishart-left-root nu k)) t))))


;; ;;;  INVERSE-WISHART
;; ;;;
;; ;;;  If A ~ Inverse-Wishart[nu,inverse-scale], then 
;; ;;;  (invert A) ~ Wishart(nu,inverse-scale).

;; (defclass inverse-wishart (multivariate)
;;   ((nu :initarg :nu :reader nu :type fixnum :documentation "degrees of freedom")
;;    (inverse-scale :initarg :inverse-scale :reader inverse-scale
;;                   :type hermitian-matrix
;;                   :documentation "Inverse scale matrix, to which the
;;                   mean is proportional.")
;;    (inverse-scale-right-root
;;     :accessor inverse-scale-right-root
;;     :documentation "C, where (mm C t) is scale.")  )
;;   (:documentation "Inverse Wishart distribution.  The PDF p(X) is
;; proportional to |X|^-(dimension+nu+1)/2 exp(-trace(inverse-scale X^-1))"))

;; (defmethod initialize-instance :after ((rv inverse-wishart)
;;                                        &key &allow-other-keys)
;;   (with-slots (inverse-scale inverse-scale-right-root) rv 
;;     (check-type inverse-scale hermitian-matrix)
;;     (setf inverse-scale-right-root (component (cholesky inverse-scale :U) :U)))
;;   rv)

;; (defmethod dimensions ((rv inverse-wishart))
;;   (let ((n (nrow (scale rv))))
;;     (list n n)))

;; (defmethod rv-type ((rv inverse-wishart))
;;   'hermitian-matrix)

;; (defmethod mean ((rv inverse-wishart))
;;   (with-slots (nu inverse-scale) rv 
;;     (e/ inverse-scale (- nu (nrow inverse-scale) 1))))

;; (define-cached-slot (rv inverse-wishart generator)
;;   (bind (((:slots-read-only nu inverse-scale-right-root) rv)
;;          (k (nrow (inverse-scale rv))))
;;     (lambda ()
;;       (mm t (solve (draw-standard-wishart-left-root nu k)
;;                    inverse-scale-right-root)))))
