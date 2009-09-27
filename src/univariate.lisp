(in-package :cl-random)

;;;; ****************************************************************
;;;; Exponential distribution.
;;;;
;;;; Also provides the primitive draw-standard-exponential, which is
;;;; useful for constructing other distributions.
;;;; ****************************************************************

(declaim (inline draw-standard-exponential))
(defun draw-standard-exponential ()
  "Return a random variable from the Exponential(1) distribution,
which has density exp(-x)."
  ;; need 1-random, because there is a remote chance of getting a 0.
  (- (log (- 1d0 (random 1d0)))))

(defclass exponential (rv)
  ((beta :initarg :beta :initform 1d0 :reader beta
         :type positive-double-float))
  (:documentation "Exponential(beta) distribution, which has density
beta*exp(-beta*x)."))

(define-printer-with-slots exponential beta)

(calculate-generator (rv exponential)
  (bind (((:slots-read-only beta) rv))
    (declare (double-float beta))
    (check-type beta positive-double-float)
    (lambda ()
      (/ (draw-standard-exponential) beta))))

;; !! do PDF, CDF, quantile

(defmethod mean ((rv exponential))
  (/ (beta rv)))

(defmethod variance ((rv exponential))
  (expt (beta rv) -2))


;;;; ****************************************************************
;;;; Normal distribution (univariate).
;;;;
;;;; Also provides some primitives (mostly for standardized normal)
;;;; that are useful for constructing/drawing from other
;;;; distributions.
;;;; ****************************************************************

(defclass normal (rv)
  ((mu :initarg :mu :initform 0d0 :reader mu :type double-float)
   (sigma :initarg :sigma :initform 1d0 :reader sigma :type positive-double-float))
  (:documentation "Normal distribution with mean mu and standard
deviation sigma."))

(define-printer-with-slots normal mu sigma)

(defmethod mean ((rv normal))
  (mu rv))

(defmethod variance ((rv normal))
  (expt (sigma rv) 2))


(declaim (inline pdf-standard-normal cdf-standard-normal draw-standard-normal))

(defun pdf-standard-normal (x)
  "PDF for N(0,1)."
  (* (exp (- (/ (expt x 2) 2d0)))
     #.(/ (sqrt (* 2d0 pi)))))

(defun cdf-standard-normal (x)
  "CDF for N(0,1)."
  ;; Uses method of Marsaglia (2004).  !! when i have tons of time on
  ;; my hands, I will try a Pade expansion for this. -- Tamas
  ;;
  ;; !!!!  very important: under/overflows for values far, far out,
  ;; need to deal with this, write second method in Marsaglia (2004)?
  (declare (optimize (speed 3) (safety 0)))
  (check-type x double-float)
  (let ((q (expt x 2))
        (s x)
        (b x)
        (s-p 0d0)
        (i 1))
    (declare (double-float q s b s-p)
             (fixnum i))
    (tagbody
     top
       (unless (= s-p s)
         (setf s-p s)
         (incf i 2)
         (setf b (* b (/ q i)))
         (incf s b)
         (go top)))
    (+ 0.5d0 (* s (exp (+ (* -0.5 q) #.(* -0.5d0 (log (* 2 pi)))))))))

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

;;; Two trivial functions below for more transparent code, should be
;;; inlined.

(declaim (inline to-standard-normal from-standard-normal))

(defun to-standard-normal (x mu sigma)
  "Scale x to standard normal."
  (/ (- x mu) sigma))

(defun from-standard-normal (x mu sigma)
  "Scale x from standard normal." 
  (+ (* x sigma) mu))

;;; !! It is claimed in Marsaglia & Tsang (2000) that the ziggurat
;;; method is about 5-6 times faster than the above, mainly because of
;;; precomputed tables.  Need to write and test this, and if it is
;;; true, use that method instead.

(defmethod pdf ((rv normal) x)
  (declare (optimize (speed 3))
           (double-float x))
  (bind (((:slots-read-only mu sigma) rv))
    (declare (double-float mu sigma))
    (/ (pdf-standard-normal (to-standard-normal x mu sigma))
       sigma)))

(defmethod cdf ((rv normal) x)
  (declare (optimize (speed 3))
           (double-float x))
  (bind (((:slots-read-only mu sigma) rv))
    (declare (double-float mu sigma))
    (cdf-standard-normal (to-standard-normal x mu sigma))))

;; !! do quantile, based on the links in Marsaglia's articles, ie
;; !! rootfinding using the CDF from a good guess

(calculate-generator (rv normal)
  (declare (optimize (speed 3)))
  (bind (((:slots-read-only mu sigma) rv))
    (declare (double-float mu sigma))
    (lambda ()
      (from-standard-normal (draw-standard-normal) mu sigma))))


;;;; ****************************************************************
;;;; Truncated normal distribution (univariate).
;;;; ****************************************************************

(defclass truncated-normal (rv)
  ((mu :initarg :mu :initform 0d0 :reader mu :type double-float)
   (sigma :initarg :sigma :initform 1d0 :reader sigma :type positive-double-float)
   (left :initarg :left :initform nil :reader left :type truncation-boundary)
   (right :initarg :right :initform nil :reader right :type truncation-boundary)
   (mass :type (double-float 0d0 1d0) :documentation "total mass of the raw PDF")
   (mean :reader mean :type double-float :documentation "mean")
   (variance :reader variance :type positive-double-float :documentation "variance")
   (cdf-left :type double-float :documentation "CDF at left"))
  (:documentation "Truncated normal distribution with given mu and
sigma (corresponds to the mean and standard deviation in the
untruncated case, respectively), on the interval [left, right].  If
either of them is nil, that means no truncation from that
direction. If both are nil, reverts to the normal distribution."))

(define-printer-with-slots truncated-normal mu sigma left right)

(defmethod initialize-instance :after ((rv truncated-normal) &key
                                       &allow-other-keys)
  ;; !!! calculations of mass, mean, variance are very bad if the
  ;; support is far out in the tail.  that should be approximated
  ;; differently (maybe we should give a warning?).
  (bind (((:slots left right mu sigma mass mean variance) rv))
    (flet ((conditional-calc (x left-p)
             ;; Return (values pdf xpdf cdf), also for missing
             ;; boundary (left-p gives which).  x is the normalized
             ;; variable; xpdf is x * pdf, 0 for infinite boundaries.
             (if x
                 (let* ((x (to-standard-normal x mu sigma))
                        (pdf (pdf-standard-normal x))
                        (xpdf (* x pdf))
                        (cdf (cdf-standard-normal x)))
                   (values pdf xpdf cdf))
                 (values 0d0 0d0 (if left-p 0d0 1d0)))))
      (check-type left truncation-boundary)
      (check-type right truncation-boundary)
      (bind (((:values pdf-left xpdf-left cdf-left)
              (conditional-calc left t))
             ((:values pdf-right xpdf-right cdf-right)
              (conditional-calc right nil)))
        ;; (format t "left  pdf=~a  xpdf=~a  cdf=~a~%right pdf=~a  xpdf=~a  cdf=~a~%"
        ;;         pdf-left xpdf-left cdf-left pdf-right xpdf-right cdf-right)
        (setf mass (- cdf-right cdf-left))
        (unless (plusp mass)
          (error "invalid left and/or right boundaries"))
        (let ((ratio (/ (- pdf-left pdf-right) mass)))
          (setf mean (+ mu (* ratio sigma))
                variance (* (expt sigma 2)
                            (- (1+ (/ (- xpdf-left xpdf-right) mass))
                               (expt ratio 2)))
                (slot-value rv 'cdf-left) cdf-left)))))
  rv)

(defmethod pdf ((rv truncated-normal) x)
  (check-type x double-float)
  (bind (((:slots-read-only mu sigma mass left right) rv))
    (if (or (<* x left) (>* x right))
        0d0
        (/ (pdf-standard-normal (to-standard-normal x mu sigma))
           sigma
           mass))))

(defmethod cdf ((rv truncated-normal) x)
  (check-type x double-float)
  (bind (((:slots-read-only mu sigma mass left right cdf-left) rv))
    (cond
      ((<* x left) 0d0)
      ((>* x right) 1d0)
      (t (/ (- (cdf-standard-normal (to-standard-normal x mu sigma)) cdf-left)
            mass)))))

(declaim (inline truncated-normal-optimal-alpha truncated-normal-left-p))

(defun truncated-normal-optimal-alpha (left)
  "Calculate optimal exponential parameter for left-truncated normals."
  (/ (+ left (sqrt (+ (expt left 2) 4d0)))
     2d0))

(defun truncated-normal-left-p (optimal-alpha left right)
  "Calculate if it is optimal to use the left-truncated draw and
reject than the two-sided accept-reject algorithm."
  (> (* optimal-alpha (exp (* optimal-alpha left 0.5d0)) (- right left))
     (* (exp 0.5d0) (exp (/ (expt left 2))))))

(declaim (inline draw-left-truncated-standard-normal
                 draw-left-right-truncated-standard-normal))

(defun draw-left-truncated-standard-normal (left alpha)
  "Draw a left truncated standard normal, using an Exp(alpha,left)
distribution."
  (try ((z (+ (/ (draw-standard-exponential) alpha) left))
            (rho (exp (* (expt (- z alpha) 2) -0.5))))
       (<= (random 1d0) rho) z))

(defun draw-left-right-truncated-standard-normal (left width coefficient)
  "Accept-reject algorithm based on uniforms.  Coefficient is
multiplying the exponential, and has to be based on exp(left^2) or
exp(right^2) as appropriate.  width is right-left."
  (try ((z (+ left (random width)))
        (rho (* coefficient (exp (* (expt z 2) -0.5d0)))))
       (<= (random 1d0) rho) z))

(calculate-generator (rv truncated-normal)
  (declare (optimize (speed 3)))
  (bind (((:slots-read-only mu sigma left right) rv))
    (declare (double-float mu sigma)
             (truncation-boundary left right))
    (macrolet ((lambda* (form)
                 "Lambda with no arguments, transform using mu and sigma."
                 `(lambda ()
                    (from-standard-normal ,form mu sigma)))
               (lambda*- (form)
                 "Like lambda*, but also negating the argument."
                 `(lambda* (- ,form))))
      (cond
        ;; truncated on both sides
        ((and left right)
         (let* ((left (to-standard-normal left mu sigma))
                (right (to-standard-normal right mu sigma))
                (width (- right left))
                (contains-zero-p (<= left 0d0 right)))
           (cond
             ;; too wide: best to sample from normal and discard
             ((and (< (sqrt (* 2 pi)) width) contains-zero-p)
              (lambda* (try ((x (draw-standard-normal)))
                            (<= left x right) x)))
             ;; narrow & contains zero: always use uniform-based reject/accept
             (contains-zero-p
              (lambda* (draw-left-right-truncated-standard-normal left width 1d0)))
             ;; whole support above 0, need to test
             ((< 0d0 left)
              (let ((alpha (truncated-normal-optimal-alpha left)))
                (if (truncated-normal-left-p alpha left right)
                    ;; optimal to try and reject if not good
                    (lambda* (try ((x (draw-left-truncated-standard-normal left alpha)))
                                  (<= x right) x))
                    ;; optimal to use the uniform-based reject/accept
                    (lambda* (draw-left-right-truncated-standard-normal 
                              left width (* (expt left 2) 0.5d0))))))
             ;; whole support below 0, will flip
             (t
              ;; swap, and then negate
              (let ((left (- right))
                    (right (- left)))
                (let ((alpha (truncated-normal-optimal-alpha left)))
                  (if (truncated-normal-left-p alpha left right)
                      ;; optimal to try and reject if not good
                      (lambda*- (try ((x (draw-left-truncated-standard-normal
                                          left alpha)))
                                     (<= x right) x))
                      ;; optimal to use the uniform-based reject/accept
                      (lambda*- (draw-left-right-truncated-standard-normal 
                                 left width (* (expt left 2) 0.5d0))))))))))
        ;; truncated on the left
        (left
         (let ((left (to-standard-normal left mu sigma)))
               (if (<= left 0d0)
                   (lambda* (try ((x (draw-standard-normal)))
                                 (<= left x) x))
                   (lambda* (draw-left-truncated-standard-normal 
                             left
                             (truncated-normal-optimal-alpha left))))))
        ;; truncated on the right, flip
        (right
         (let ((left (- (to-standard-normal right mu sigma))))
           (if (<= left 0d0)
               (lambda*- (try ((x (draw-standard-normal)))
                              (<= left x) x))
               (lambda*- (draw-left-truncated-standard-normal 
                          left
                          (truncated-normal-optimal-alpha left))))))
        ;; this is a standard normal, no truncation
        (t (lambda* (draw-standard-normal)))))))
  

;;;; ****************************************************************
;;;; Gamma distribution.
;;;;
;;;; Also provides a generator-standard-gamma, which returns a
;;;; generator for a given alpha.
;;;; ****************************************************************

(defclass gamma (rv)
  ((alpha :initarg :alpha :initform 1d0 
          :type positive-double-float :reader alpha
          :documentation "shape parameter")
   (beta :initarg :beta :initform 1d0
         :type positive-double-float :reader beta
         :documentation "scale parameter"))
  (:documentation "Gamma(alpha,beta) distribution, with density
  proportional to x^(alpha-1) exp(-x*beta) beta^k"))

(define-printer-with-slots gamma alpha beta)

(defmethod mean ((rv gamma))
  (/ (alpha rv) (beta rv)))

(defmethod variance ((rv gamma))
  (* (alpha rv) (expt (beta rv) -2)))

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
     (bind (((:values x v) (prog () ; loop was not optimized for some reason
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

(defun generator-standard-gamma (alpha)
  "Return a closure that will draw a double-float from the gamma
distribution with given alpha.  Useful for constructing other
distributions (eg Dirichlet, etc), too."
  (declare (optimize (speed 3))
           (double-float alpha))
  (if (< alpha 1d0)
      (bind ((1+alpha (1+ alpha))
             (1/alpha (/ alpha))
             ((:values d c) (standard-gamma1-d-c 1+alpha)))
        ;; use well known-transformation, see p 371 of Marsaglia and Tsang (2000)
        (lambda ()
          (* (expt (random 1d0) 1/alpha) (draw-standard-gamma1 1+alpha d c))))
      (bind (((:values d c) (standard-gamma1-d-c alpha)))
        (lambda ()
          (draw-standard-gamma1 alpha d c)))))

(calculate-generator (rv gamma)
  (declare (optimize (speed 3)))
  (bind (((:slots-read-only alpha beta) rv)
         (standard-generator (generator-standard-gamma alpha)))
    (declare (double-float alpha beta)
             (univariate-continuous-generator standard-generator))
    (lambda ()
      (/ (funcall standard-generator) beta))))
