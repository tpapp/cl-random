;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; -*-

(in-package #:cl-random)

(defun check-mean-variance-compatibility (mean variance)
  "Assert that the mean is a vector, and its dimensions are compatible with
variance."
  (assert (and (vectorp mean) 
               (= (length mean) (nrow variance) (ncol variance)))))

(defun normal-quadratic-form (x mean variance-left-sqrt)
  "Calculate (x-mean)^T variance^-1 (x-mean), given X, MEAN, and the left
square root of variance."
  (dot (solve variance-left-sqrt (e- x mean)) t))

(define-rv r-multivariate-normal (mean variance)
  (:documentation "Multivariate normal MVN(mean,variance) distribution."
   :instance instance)
  ((n :type fixnum :documentation "Number of dimensions.")
   (mean :type vector :reader t)
   (variance-left-sqrt :reader t :documentation 
                       "Left square root of the variance matrix."))
  (let ((variance-left-sqrt (left-square-root variance)))
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
       (let+ (((index-specification) index-specifications)
              (mean (sub mean index-specification))
              (variance (sub (variance instance) index-specification)))
         (if (vectorp mean)
             (r-multivariate-normal mean variance)
             (r-normal mean variance)))))

;;;  MULTIVARIATE T distribution
;;;
;;;  When drawing numbers, the scaling factor (with distribution
;;;  inverse-chi-square, nu degrees of freedom) is returned as the second
;;;  value.

(define-rv r-multivariate-t (mean sigma nu 
                                  &key multivariate-normal scaling-factor 
                                  (s^2 1d0 s^2?))
  (:documentation "Multivariate T distribution with given MEAN, scale
  SIGMA*S^2 and NU degrees of freedom.")
  ((multivariate-normal :type r-multivariate-normal :documentation
                        "distribution for obtaining normal draws" :reader t)
   (scaling-factor :type r-inverse-gamma :documentation
                   "distribution that scales the variance of draws."
                   :reader t))
  (make :multivariate-normal 
        (aif multivariate-normal
             (prog1 it
               (check-type it r-multivariate-normal)
               (assert (not (or mean sigma)) () "Can't initialize with both
                       MEAN & SIGMA and MULTIVARIATE-NORMAL."))
             (r-multivariate-normal mean sigma))
        :scaling-factor
        (aif scaling-factor
             (progn
               (check-type it r-inverse-gamma)
               (assert (not nu) ()
                       "Can't initialize both NU and SCALING-FACTOR.")
               (assert (not s^2?) ()
                       "Can't initialize both S^2 and SCALING-FACTOR.")
               it)
             (r-inverse-chi-square nu s^2)))
  (mean () 
        (assert (< 1 (nu scaling-factor)))
        (mean multivariate-normal))
  (variance ()
            (let+ (((&accessors-r/o nu s^2) scaling-factor))
              (e* (variance multivariate-normal)
                  (* (t-scale-to-variance-coefficient nu)
                     s^2))))
  (log-pdf (x &optional ignore-constant?)
           (let+ (((&accessors-r/o mean variance-left-sqrt)
                   multivariate-normal)
                  ((&accessors-r/o nu s^2) scaling-factor)
                  (variance-left-sqrt (e* (sqrt s^2) variance-left-sqrt))
                  (d (length mean)))
             (maybe-ignore-constant
              ignore-constant?
              (* (log (1+ (/ (normal-quadratic-form x mean variance-left-sqrt)
                             nu)))
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
       (let+ (((index-specification) index-specifications)
              (normal (sub multivariate-normal index-specification))
              ((&accessors-r/o nu s^2) scaling-factor))
         (etypecase normal
           (r-normal 
            (r-t (mean normal) (* s^2 (variance normal)) nu))
           (r-multivariate-normal 
            (r-multivariate-t nil nil nil
                              :multivariate-normal normal
                              :scaling-factor scaling-factor)))))
  (nu ()
      (nu scaling-factor))
  (s^2 ()
       (s^2 scaling-factor)))

;;;  WISHART
;;;
;;;  The k-dimensional Wishart distribution with NU degrees of freedom
;;;  and scale parameter SCALE is the multivariate generalization of
;;;  the gamma (or chi-square) distribution.

(defun draw-standard-wishart-left-sqrt (nu k)
  "Draw a lower triangular matrix L such that (mm L t) has Wishart(I,nu)
distribution (dimension k x k)."
  (let+ ((nu (coerce nu 'double-float))
         (l (make-array (list k k) :element-type 'double-float)))
    (dotimes (i k)
      (setf (aref l i i) (sqrt (draw (r-chi-square (- nu i)))))
      (iter
        (for row-major-index :from (array-row-major-index l i 0)
             :below (array-row-major-index l i i))
        (setf (row-major-aref l row-major-index) (draw-standard-normal))))
    (make-lower-triangular-matrix l)))

(define-rv r-wishart (nu scale)
  (:documentation "Wishart distribution with NU degrees of freedom and given
  SCALE matrix (which, as usual, can be a decomposition which yields a left
  square root.  Draws are returned as decompositions.")
  ((nu :type fixnum :reader t :documentation "degrees of freedom")
   (scale-left-sqrt :reader t
                    :documentation "left square root of the scale matrix")
   (k :type fixnum :documentation "dimension"))
  (let ((scale-left-sqrt (left-square-root scale)))
    (check-type nu (and fixnum (satisfies plusp)))
    (make :nu nu :scale-left-sqrt scale-left-sqrt :k (nrow scale-left-sqrt)))
  (mean () (e* nu (mm scale-left-sqrt t)))
  (draw (&key) 
        (mm (mm scale-left-sqrt (draw-standard-wishart-left-sqrt nu k)) t)))

;;;  INVERSE-WISHART
;;;
;;;  If A ~ Inverse-Wishart[nu,inverse-scale], then 
;;;  (invert A) ~ Wishart(nu,inverse-scale).

(define-rv r-inverse-wishart (nu inverse-scale)
  (:documentation "Inverse Wishart distribution.  The PDF p(X) is proportional
to |X|^-(dimension+nu+1)/2 exp(-trace(inverse-scale X^-1)).  Draws are
returned as decompositions.")
  ((nu :type fixnum :reader t :documentation "degrees of freedom")
   (inverse-scale-right-sqrt :reader t :documentation
                             "right square root of the inverse scale matrix")
   (k :type fixnum :reader t :documentation "number of dimensions"))
  (let ((inverse-scale-right-sqrt (right-square-root inverse-scale)))
    (check-type nu (and fixnum (satisfies plusp)))
    (make :nu nu :inverse-scale-right-sqrt inverse-scale-right-sqrt
          :k (nrow inverse-scale-right-sqrt)))
  (mean () (e/ (mm t inverse-scale-right-sqrt) (- nu k 1)))
  (draw (&key)
        (mm (transpose (solve (draw-standard-wishart-left-sqrt nu k)
                              inverse-scale-right-sqrt))
            t)))
