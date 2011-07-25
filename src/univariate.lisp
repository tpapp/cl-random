(in-package :cl-random)


;;; ****************************************************************
;;; Uniform distribution.
;;; ****************************************************************

(define-rv r-uniform (left right)
  (:documentation "Uniform(left,right) distribution.")
  ((left :type double-float)
   (right :type double-float)
   (width :type double-float))
  (with-doubles (left right)
    (assert (< left right))
    (let ((width (- right left)))
      (make :left left :right right :width width)))
  (mean () (/ (+ left right) 2))
  (variance () (/ (expt width 2) 12d0))
  (log-pdf (x &optional ignore-constant?)
           (declare (ignore ignore-constant?))
           (with-doubles (x)
             (if (<= left x right)
                 (- (log width))
                 nil)))
  (cdf (x)
       (with-doubles (x)
         (cond
           ((< x left) 0d0)
           ((< right x) 1d0)
           (t (/ (- x left) width)))))
  (quantile (p)
            (with-doubles (p)
              (check-probability p)
              (+ left (* p (- right left)))))
  (draw (&key)
        (+ left (random width))))

;;; ****************************************************************
;;; Exponential distribution.
;;;
;;; Also provides the primitive draw-standard-exponential, which is
;;; useful for constructing other distributions.
;;; ****************************************************************

(defun draw-standard-exponential ()
  "Return a random variable from the Exponential(1) distribution,
which has density exp(-x)."
  ;; need 1-random, because there is a remote chance of getting a 0.
  (- (log (- 1d0 (random 1d0)))))

(declaim (inline draw-standard-exponential))

(define-rv r-exponential (beta)
  (:documentation "Exponential(beta) distribution, with density
  beta*exp(-beta*x) on x >= 0.")
  ((beta :type double-float :reader t))
  (with-doubles (beta)
    (assert (plusp beta))
    (make :beta beta))
  (mean () (/ beta))
  (variance () (expt beta -2))
  (log-pdf (x &optional ignore-constant?)
           (declare (ignore ignore-constant?))
           (with-doubles (x)
             (- (log beta) (* beta x))))
  (cdf (x)
       (with-doubles (x)
         (- 1 (exp (- (* beta x))))))
  (quantile (p)
            (with-doubles (p)
              (check-probability p :right)
              (/ (log (- 1 p)) (- beta))))
  (draw (&key) 
    (/ (draw-standard-exponential) beta)))

;;; ****************************************************************
;;; Normal distribution (univariate).
;;;
;;; Also provides some primitives (mostly for standardized normal)
;;; that are useful for constructing/drawing from other
;;; distributions.
;;; ****************************************************************


(declaim (ftype (function () double-float) draw-standard-normal))

(defun draw-standard-normal ()
  "Draw a random number from N(0,1)."
  ;; Method from Leva (1992).  This is considered much better/faster
  ;; than the Box-Muller method.
  (declare (optimize (speed 3) (safety 0)))
  (tagbody
   top
     (let* ((u (random 1d0))
            (v (* 1.7156d0 (- (random 1d0) 0.5d0)))
            (x (- u 0.449871d0))
            (y (+ (abs v) 0.386595d0))
            (q (+ (expt x 2) (* y (- (* 0.19600d0 y) (* 0.25472d0 x))))))
       (if (and (> q 0.27597d0)
                (or (> q 0.27846d0) 
                    (plusp (+ (expt v 2) (* 4 (expt u 2) (log u))))))
           (go top)
           (return-from draw-standard-normal (/ v u))))))

(declaim (inline to-standard-normal from-standard-normal))

(defun to-standard-normal (x mu sigma)
  "Scale x to standard normal."
  (/ (- x mu) sigma))

(defun from-standard-normal (x mu sigma)
  "Scale x from standard normal." 
  (+ (* x sigma) mu))

(define-rv r-normal (mean sd)
  (:documentation "Normal(mean,sd) distribution.")
  ((mean :type double-float :reader t)
   (sd :type double-float :reader t))
  (with-doubles (mean sd)
    (assert (plusp sd))
    (make :mean mean :sd sd))
  (variance () (expt sd 2))
  (log-pdf (x &optional ignore-constant?)
           (maybe-ignore-constant ignore-constant?
                                  (with-doubles (x)
                                    (/ (expt (- x mean) 2) (expt sd 2) -2d0))
                                  (- +normal-log-pdf-constant+ (log sd))))
  (cdf (x)
       (with-doubles (x)
         (rmath:pnorm5 x mean sd 1 0)))
  (quantile (q)
            (with-doubles (q)
              (check-probability q :both)
              (rmath:qnorm5 q mean sd 1 0)))
  (draw (&key)
    (from-standard-normal (draw-standard-normal) mean sd)))

;;; !! It is claimed in Marsaglia & Tsang (2000) that the ziggurat
;;; method is about 5-6 times faster than the above, mainly because of
;;; precomputed tables.  Need to write and test this, and if it is
;;; true, use that method instead.


;; !! do quantile, based on the links in Marsaglia's articles, ie
;; !! rootfinding using the CDF from a good guess

;;; ****************************************************************
;;; Truncated normal distribution (univariate).
;;; ****************************************************************

;; (define-rv truncated-normal (mu sigma left right)
;;   "Truncated normal distribution with given mu and sigma (corresponds to the
;; mean and standard deviation in the untruncated case, respectively), on the
;; interval [left, right].  If either of them is nil, that means no truncation from
;; that direction. If both are nil, reverts to the normal distribution."
  
;;   )


;; (defclass truncated-normal (univariate)
;;   ((mu :initarg :mu :initform 0d0 :reader mu :type double-float)
;;    (sigma :initarg :sigma :initform 1d0 :reader sigma :type positive-double-float)
;;    (left :initarg :left :initform nil :reader left :type truncation-boundary)
;;    (right :initarg :right :initform nil :reader right :type truncation-boundary)
;;    (mass :type (double-float 0d0 1d0) :documentation "total mass of the raw PDF")
;;    (mean :reader mean :type double-float :documentation "mean")
;;    (variance :reader variance :type positive-double-float :documentation "variance")
;;    (cdf-left :type double-float :documentation "CDF at left"))
;;   (:documentation ))

;; (define-printer-with-slots truncated-normal mu sigma left right)

;; (defmethod initialize-instance :after ((rv truncated-normal) &key
;;                                        &allow-other-keys)
;;   ;; !!! calculations of mass, mean, variance are very bad if the
;;   ;; support is far out in the tail.  that should be approximated
;;   ;; differently (maybe we should give a warning?).
;;   (bind (((:slots left right mu sigma mass mean variance) rv))
;;     (flet ((conditional-calc (x left-p)
;;              ;; Return (values pdf xpdf cdf), also for missing
;;              ;; boundary (left-p gives which).  x is the normalized
;;              ;; variable; xpdf is x * pdf, 0 for infinite boundaries.
;;              (if x
;;                  (let* ((x (to-standard-normal x mu sigma))
;;                         (pdf (pdf-standard-normal x))
;;                         (xpdf (* x pdf))
;;                         (cdf (cdf-standard-normal x)))
;;                    (values pdf xpdf cdf))
;;                  (values 0d0 0d0 (if left-p 0d0 1d0)))))
;;       (check-type left truncation-boundary)
;;       (check-type right truncation-boundary)
;;       (bind (((:values pdf-left xpdf-left cdf-left)
;;               (conditional-calc left t))
;;              ((:values pdf-right xpdf-right cdf-right)
;;               (conditional-calc right nil)))
;;         ;; (format t "left  pdf=~a  xpdf=~a  cdf=~a~%right pdf=~a  xpdf=~a  cdf=~a~%"
;;         ;;         pdf-left xpdf-left cdf-left pdf-right xpdf-right cdf-right)
;;         (setf mass (- cdf-right cdf-left))
;;         (unless (plusp mass)
;;           (error "invalid left and/or right boundaries"))
;;         (let ((ratio (/ (- pdf-left pdf-right) mass)))
;;           (setf mean (+ mu (* ratio sigma))
;;                 variance (* (expt sigma 2)
;;                             (- (1+ (/ (- xpdf-left xpdf-right) mass))
;;                                (expt ratio 2)))
;;                 (slot-value rv 'cdf-left) cdf-left)))))
;;   rv)

;; (defmethod pdf ((rv truncated-normal) x &optional unscaled)
;;   (declare (ignore unscaled))
;;   (check-type x double-float)
;;   (bind (((:slots-read-only mu sigma mass left right) rv))
;;     (if (or (<* x left) (>* x right))
;;         0d0
;;         (/ (pdf-standard-normal (to-standard-normal x mu sigma))
;;            sigma
;;            mass))))

;; (defmethod cdf ((rv truncated-normal) x)
;;   (check-type x double-float)
;;   (bind (((:slots-read-only mu sigma mass left right cdf-left) rv))
;;     (cond
;;       ((<* x left) 0d0)
;;       ((>* x right) 1d0)
;;       (t (/ (- (cdf-standard-normal (to-standard-normal x mu sigma)) cdf-left)
;;             mass)))))

;; (declaim (inline truncated-normal-optimal-alpha truncated-normal-left-p))

;; (defun truncated-normal-optimal-alpha (left)
;;   "Calculate optimal exponential parameter for left-truncated normals."
;;   (/ (+ left (sqrt (+ (expt left 2) 4d0)))
;;      2d0))

;; (defun truncated-normal-left-p (optimal-alpha left right)
;;   "Calculate if it is optimal to use the left-truncated draw and
;; reject than the two-sided accept-reject algorithm."
;;   (> (* optimal-alpha (exp (* optimal-alpha left 0.5d0)) (- right left))
;;      (* (exp 0.5d0) (exp (/ (expt left 2))))))

;; (declaim (inline draw-left-truncated-standard-normal
;;                  draw-left-right-truncated-standard-normal))

;; (defun draw-left-truncated-standard-normal (left alpha)
;;   "Draw a left truncated standard normal, using an Exp(alpha,left)
;; distribution."
;;   (try ((z (+ (/ (draw-standard-exponential) alpha) left))
;;             (rho (exp (* (expt (- z alpha) 2) -0.5))))
;;        (<= (random 1d0) rho) z))

;; (defun draw-left-right-truncated-standard-normal (left width coefficient)
;;   "Accept-reject algorithm based on uniforms.  Coefficient is
;; multiplying the exponential, and has to be based on exp(left^2) or
;; exp(right^2) as appropriate.  width is right-left."
;;   (try ((z (+ left (random width)))
;;         (rho (* coefficient (exp (* (expt z 2) -0.5d0)))))
;;        (<= (random 1d0) rho) z))

;; (define-cached-slot (rv truncated-normal generator)
;;   (declare (optimize (speed 3)))
;;   (bind (((:slots-read-only mu sigma left right) rv))
;;     (declare (double-float mu sigma)
;;              (truncation-boundary left right))
;;     (macrolet ((lambda* (form)
;;                  "Lambda with no arguments, transform using mu and sigma."
;;                  `(lambda ()
;;                     (from-standard-normal ,form mu sigma)))
;;                (lambda*- (form)
;;                  "Like lambda*, but also negating the argument."
;;                  `(lambda* (- ,form))))
;;       (cond
;;         ;; truncated on both sides
;;         ((and left right)
;;          (let* ((left (to-standard-normal left mu sigma))
;;                 (right (to-standard-normal right mu sigma))
;;                 (width (- right left))
;;                 (contains-zero-p (<= left 0d0 right)))
;;            (cond
;;              ;; too wide: best to sample from normal and discard
;;              ((and (< (sqrt (* 2 pi)) width) contains-zero-p)
;;               (lambda* (try ((x (draw-standard-normal)))
;;                             (<= left x right) x)))
;;              ;; narrow & contains zero: always use uniform-based reject/accept
;;              (contains-zero-p
;;               (lambda* (draw-left-right-truncated-standard-normal
;;                         left width 1d0)))
;;              ;; whole support above 0, need to test
;;              ((< 0d0 left)
;;               (let ((alpha (truncated-normal-optimal-alpha left)))
;;                 (if (truncated-normal-left-p alpha left right)
;;                     ;; optimal to try and reject if not good
;;                     (lambda* (try ((x (draw-left-truncated-standard-normal
;;                                        left alpha)))
;;                                   (<= x right) x))
;;                     ;; optimal to use the uniform-based reject/accept
;;                     (lambda* (draw-left-right-truncated-standard-normal 
;;                               left width (* (expt left 2) 0.5d0))))))
;;              ;; whole support below 0, will flip
;;              (t
;;               ;; swap, and then negate
;;               (let ((left (- right))
;;                     (right (- left)))
;;                 (let ((alpha (truncated-normal-optimal-alpha left)))
;;                   (if (truncated-normal-left-p alpha left right)
;;                       ;; optimal to try and reject if not good
;;                       (lambda*- (try ((x (draw-left-truncated-standard-normal
;;                                           left alpha)))
;;                                      (<= x right) x))
;;                       ;; optimal to use the uniform-based reject/accept
;;                       (lambda*- (draw-left-right-truncated-standard-normal 
;;                                  left width (* (expt left 2) 0.5d0))))))))))
;;         ;; truncated on the left
;;         (left
;;          (let ((left (to-standard-normal left mu sigma)))
;;                (if (<= left 0d0)
;;                    (lambda* (try ((x (draw-standard-normal)))
;;                                  (<= left x) x))
;;                    (lambda* (draw-left-truncated-standard-normal 
;;                              left
;;                              (truncated-normal-optimal-alpha left))))))
;;         ;; truncated on the right, flip
;;         (right
;;          (let ((left (- (to-standard-normal right mu sigma))))
;;            (if (<= left 0d0)
;;                (lambda*- (try ((x (draw-standard-normal)))
;;                               (<= left x) x))
;;                (lambda*- (draw-left-truncated-standard-normal 
;;                           left
;;                           (truncated-normal-optimal-alpha left))))))
;;         ;; this is a standard normal, no truncation
;;         (t (lambda* (draw-standard-normal)))))))
  
;;; ****************************************************************
;;; Lognormal distribution
;;; ****************************************************************

(define-rv r-log-normal (log-mean log-sd)
  (:documentation "Log-normal distribution with location log-mean and scale log-sd.")
  ((log-mean :type double-float)
   (log-sd :type double-float))
  (with-doubles (log-mean log-sd)
    (assert (plusp log-sd))
    (make :log-mean log-mean :log-sd log-sd))
  
  (mean () (exp (+ log-mean (/ (expt log-sd 2) 2))))
  (variance () (let ((sigma^2 (expt log-sd 2)))
              (* (1- (exp sigma^2))
                 (exp (+ (* 2 log-mean) sigma^2)))))
  (log-pdf (x &optional ignore-constant?)
           (maybe-ignore-constant ignore-constant? 
                                  (with-doubles (x)
                                    (let ((log-x (log x)))
                                      (- (/ (expt (- log-x log-mean) 2)
                                            (expt log-sd 2) -2)
                                         log-x)))
                                  (- +normal-log-pdf-constant+ (log log-sd))))
  (draw (&key)
    (exp (from-standard-normal (draw-standard-normal) log-mean log-sd))))

;;; ****************************************************************
;;; Student's T distribution
;;; ****************************************************************

(declaim (inline t-scale-to-variance-coefficient))
(defun t-scale-to-variance-coefficient (nu)
  "Return the coefficient that multiplies the Sigma matrix or the squared
scale to get the variance of a (multivariate) Student-T distribution.  Also
checks that nu > 2, ie the variance is defined."
  (assert (< 2d0 nu))
  (/ nu (- nu 2d0)))

(defun draw-standard-t (nu)
  "Draw a standard T random variate, with NU degrees of freedom."
  ;; !! algorithm from Bailey (1994), test Marsaglia (1984) to see if it is faster
  (declare (double-float nu)
           (optimize (speed 3)))
  (try ((v1 (1- (random 2d0)))
        (v2 (1- (random 2d0)))
        (r-square (+ (expt v1 2) (expt v2 2))))
       (<= r-square 1)
       (* v1 (sqrt (the (double-float 0d0)
                     (/ (* nu (1- (expt r-square (/ -2d0 nu)))) r-square))))))

(define-rv r-t (mean scale nu)
  (:documentation "T(mean,scale,nu) random variate.")
  ((mean :type double-float :reader t)
   (scale :type double-float :reader t)
   (nu :type double-float :reader t))
  (with-doubles (mean scale nu)
    (assert (plusp nu))
    (assert (plusp scale))
    (make :mean mean :scale scale :nu nu))
  (variance ()
            (* (expt scale 2)
               (t-scale-to-variance-coefficient nu)))
  (draw (&key)
        (from-standard-normal (draw-standard-t nu) mean scale)))

;;;; ****************************************************************
;;;; Gamma distribution.
;;;;
;;;; Also provides a generator-standard-gamma, which returns a
;;;; generator for a given alpha.
;;;; ****************************************************************


(declaim (inline standard-gamma1-d-c draw-standard-gamma1
                 generator-standard-gamma))

(defun standard-gamma1-d-c (alpha)
  "Return precalculated constants (values d c), useful for drawing
form a gamma distribution."
  (let* ((d (the (double-float 0d0) (- alpha (/ 3d0))))
         (c (/ (sqrt (* 9d0 d)))))
    (values d c)))

(defun draw-standard-gamma1 (alpha d c)
  "Return a standard gamma variate (beta=1) with shape parameter alpha
>= 1.  See Marsaglia and Tsang (2004).  You should precalculate d
and c using the utility function above. "
  ;; !! see how much the change in draw-standard-normal would speed this up
  (declare (optimize (speed 3)))
  (declare (double-float d c))
  (check-type alpha (double-float 1d0))
  (tagbody 
   top
     (let+ (((&values x v) (prog () ; loop was not optimized for some reason
                            top
                              (let* ((x (draw-standard-normal))
                                     (v (expt (1+ (* c x)) 3)))
                                (if (plusp v)
                                    (return (values x v))
                                    (go top)))))
            (u (random 1d0))
            (xsq (expt x 2)))
       (if (or (< (+ u (* 0.0331 (expt xsq 2))) 1d0)
               (< (log u) (+ (* 0.5 xsq) (* d (+ (- 1d0 v) (log v))))))
           (return-from draw-standard-gamma1 (* d v))
           (go top)))))

(define-rv r-gamma (alpha beta)
  (:documentation "Gamma(alpha,beta) distribution, with density proportional
  to x^(alpha-1) exp(-x*beta).  Alpha and beta are known as shape and inverse
  scale (or rate) parameters, respectively.")
  ((alpha :type double-float :reader t)
   (beta :type double-float :reader t))
  (with-doubles (alpha beta)
    (assert (plusp alpha))
    (assert (plusp beta))
    (make :alpha alpha :beta beta))
  (mean () (/ alpha beta))
  (variance () (* alpha (expt beta -2)))
  (log-pdf (x &optional ignore-constant?)
           (maybe-ignore-constant
            ignore-constant?
            (with-doubles (x)
              (- (+ (* alpha (log beta)) (* (1- alpha) (log x)))
                 (* beta x)))
            (- (log-gamma alpha))))
  ;; note that R uses scale=1/beta
  (cdf (x)
       (with-doubles (x)
         (with-fp-traps-masked
           (rmath:pgamma x alpha (/ beta) 1 0))))
  (quantile (q)
            (with-doubles (q)
              (check-probability q :right)
              (with-fp-traps-masked
                (rmath:qgamma q alpha (/ beta) 1 0))))
  (draw (&key)
        ;; !! could optimize this by saving slots
        (if (< alpha 1d0)
            (let+ ((1+alpha (1+ alpha))
                   (1/alpha (/ alpha))
                   ((&values d c) (standard-gamma1-d-c 1+alpha)))
              ;; use well known-transformation, see p 371 of Marsaglia and
              ;; Tsang (2000)
              (/ (* (expt (random 1d0) 1/alpha)
                    (draw-standard-gamma1 1+alpha d c))
                 beta))
            (let+ (((&values d c) (standard-gamma1-d-c alpha)))
              (/ (draw-standard-gamma1 alpha d c) beta)))))

;;;; ****************************************************************
;;;; Inverse gamma distribution.
;;;; ****************************************************************

(define-rv r-inverse-gamma (alpha beta)
  (:documentation "Inverse-Gamma(alpha,beta) distribution, with density p(x)
 proportional to x^(-alpha+1) exp(-beta/x)")
  ((alpha :type double-float :reader t)
   (beta :type double-float :reader t))
  (with-doubles (alpha beta)
    (assert (plusp alpha))
    (assert (plusp beta))
    (make :alpha alpha :beta beta))
  (mean () (if (< 1 alpha)
               (/ beta (1- alpha))
               (error "Mean is defined only for ALPHA > 1")))
  (variance () (if (< 2 alpha)
                   (/ (expt beta 2) (expt (1- alpha) 2) (- alpha 2))
                   (error "Variance is defined only for ALPHA > 2")))
  (log-pdf (x &optional ignore-constant?)
           (maybe-ignore-constant
            ignore-constant?
            (- (* (- (1+ alpha)) (log x)) (/ beta x))
            (- (* alpha (log beta)) (log-gamma alpha))))
  (draw (&key)
        (if (< alpha 1d0)
            (let+ ((1+alpha (1+ alpha))
                   (1/alpha (/ alpha))
                   ((&values d c) (standard-gamma1-d-c 1+alpha)))
              ;; use well known-transformation, see p 371 of Marsaglia and
              ;; Tsang (2000)
              (/ beta
                 (* (expt (random 1d0) 1/alpha) (draw-standard-gamma1 1+alpha d c))))
            (let+ (((&values d c) (standard-gamma1-d-c alpha)))
              (/ beta (draw-standard-gamma1 alpha d c))))))

;;;; ****************************************************************
;;;; Chi-square and inverse-chi-square distribution (both scaled).
;;;;
;;;; We just reparametrize and rely on GAMMA and INVERSE-GAMMA.
;;;; ****************************************************************

(defgeneric nu (distribution)
  (:documentation "Return the degrees of freedom when applicable."))

(defgeneric s^2 (distribution)
  (:documentation "Return the scale when applicable."))

(defun r-chi-square (nu)
  "Chi-square distribution with NU degrees of freedom."
  (r-gamma (/ nu 2) 0.5d0))

(defmethod nu ((r-gamma r-gamma))
  (* 2 (r-gamma-alpha r-gamma)))

(defun r-inverse-chi-square (nu &optional (s^2 1d0))
  "Generalized inverse chi-square distribution.  Reparametrized to
INVERSE-GAMMA."
  (let ((nu/2 (/ nu 2)))
    (r-inverse-gamma nu/2 (* nu/2 s^2))))

(defmethod nu ((r-inverse-gamma r-inverse-gamma))
  (* 2 (r-inverse-gamma-alpha r-inverse-gamma)))

(defmethod s^2 ((r-inverse-gamma r-inverse-gamma))
  (let+ (((&structure r-inverse-gamma- alpha beta) r-inverse-gamma))
    (/ beta alpha)))

;;;; ****************************************************************
;;;; Beta distribution.
;;;; ****************************************************************

(define-rv r-beta (alpha beta)
  (:documentation "Beta(alpha,beta) distribution, with density proportional to
x^(alpha-1)*(1-x)^(beta-1).")
  ((alpha :type double-float :reader t)
   (beta :type double-float :reader t))
  (with-doubles (alpha beta)
    (assert (plusp alpha))
    (assert (plusp beta))
    (make :alpha beta :beta beta))
  (mean () (/ alpha (+ alpha beta)))
  (variance () (let ((sum (+ alpha beta)))
                 (/ (* alpha beta) (* (expt sum 2) (1+ sum)))))
  (draw (&key)
        (let ((alpha (draw (r-gamma alpha 1)))
              (beta (draw (r-gamma beta 1))))
          (/ alpha (+ alpha beta)))))

;;;; ****************************************************************
;;;; Discrete distribution.
;;;; ****************************************************************

;;; ?? The implementation may be improved speedwise with declarations
;;; and micro-optimizations.  Not a high priority.  However,
;;; converting arguments to double-float provided a great speedup,
;;; especially in cases when the normalization resulted in rationals
;;; -- comparisons for the latter are quite slow.

(define-rv r-discrete (probabilities)
  (:documentation "Discrete probabilities." :instance instance)
  ((probabilities :type double-float-vector :reader t)
   (prob :type double-float-vector)
   (alias :type (simple-array fixnum (*)))
   (n-double :type double-float))
  ;; algorithm from Vose (1991)
  (let* ((probabilities (as-double-float-probabilities probabilities))
         (p (copy-seq probabilities))   ; this is modified
         (n (length probabilities))
         (alias (make-array n :element-type 'fixnum))
         (prob (make-array n :element-type 'double-float))
         (n-double (as-double-float n))
         (threshold (/ n-double))
         small
         large)
    ;; separate using threshold
    (dotimes (i n)
      (if (> (aref p i) threshold)
          (push i large)
          (push i small)))
    ;; reshuffle
    (loop :while (and small large) :do
          (let* ((j (pop small))
                 (k (pop large)))
            (setf (aref prob j) (* n-double (aref p j))
                  (aref alias j) k)
            (if (< threshold (incf (aref p k)
                                   (- (aref p j) threshold)))
                (push k large)
                (push k small))))
    ;; the rest use 1
    (loop :for s :in small :do (setf (aref prob s) 1d0))
    (loop :for l :in large :do (setf (aref prob l) 1d0))
    ;; save what's needed
    (make :probabilities probabilities :prob prob :alias alias :n-double n-double))
  (mean ()
        (iter 
          (for p :in-vector probabilities :with-index i)
          (summing (* p i))))
  (variance ()
            (iter
              (with mean := (mean instance))
              (for p :in-vector probabilities :with-index i)
              (summing (* p (expt (- i mean) 2)))))
  (log-pdf (i &optional ignore-constant?)
           (declare (ignore ignore-constant?))
           (log (aref probabilities i)))
  (cdf (i)
       ;; NIL gives the whole CDF
       (if i
           (iter
             (for p :in-vector probabilities :to i)
             (summing p))
           (cumulative-sum probabilities 
                           :result-type 'double-float-vector)))
  (draw (&key)
        (multiple-value-bind (j p) (floor (random n-double))
          (if (<= p (aref prob j))
              j
              (aref alias j)))))

