(in-package :cl-random)

;;; ****************************************************************
;;; Uniform distribution.
;;; ****************************************************************

(defclass uniform (univariate)
  ((left :initarg :left :initform 0d0 :reader left
         :type double-float :documentation "left boundary")
   (right :initarg :right :initform 1d0 :reader right
          :type double-float :documentation "right boundary"))
  (:documentation "Uniform(a,b) distribution."))

(defmethod initialize-instance :after ((rv uniform) &key
                                       &allow-other-keys)
  (bind (((:slots-read-only left right) rv))
    (assert (< left right)))
  rv)
 
(defmethod mean ((rv uniform))
  (/ (+ (left rv) (right rv)) 2d0))

(defmethod variance ((rv uniform))
  (/ (expt (- (right rv) (left rv)) 2) 12d0))

(defmethod pdf ((rv uniform) x &optional unscaled)
  (bind (((:slots-read-only left right) rv))
    (cond
      ((< x left) 0d0)
      ((< right x) 0d0)
      (t (if unscaled
             1d0
             (/ (- right left)))))))

(defmethod cdf ((rv uniform) x)
  (bind (((:slots-read-only left right) rv))
    (cond
      ((< x left) 0d0)
      ((< right x) 1d0)
      (t (/ (- x left) (- right left))))))

(define-cached-slot (rv uniform generator)
  (bind (((:slots-read-only left right) rv)
         (width (- right left)))
    (lambda ()
      (+ left (* width (random 1d0))))))

;;; ****************************************************************
;;; Exponential distribution.
;;;
;;; Also provides the primitive draw-standard-exponential, which is
;;; useful for constructing other distributions.
;;; ****************************************************************

(declaim (inline draw-standard-exponential))
(defun draw-standard-exponential ()
  "Return a random variable from the Exponential(1) distribution,
which has density exp(-x)."
  ;; need 1-random, because there is a remote chance of getting a 0.
  (- (log (- 1d0 (random 1d0)))))

(defclass exponential (univariate)
  ((beta :initarg :beta :initform 1d0 :reader beta
         :type positive-double-float))
  (:documentation "Exponential(beta) distribution, which has density
beta*exp(-beta*x)."))

(define-printer-with-slots exponential beta)

(define-cached-slot (rv exponential generator)
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


;;; ****************************************************************
;;; Normal distribution (univariate).
;;;
;;; Also provides some primitives (mostly for standardized normal)
;;; that are useful for constructing/drawing from other
;;; distributions.
;;; ****************************************************************

(defclass normal (univariate log-pdf-constant)
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

(define-cached-slot (rv normal log-pdf-constant)
  (- (* -0.5d0 (log (* 2 pi)))
     (log (sigma rv))))

(defmethod log-pdf ((rv normal) x &optional unscaled?)
  (scale-log-pdf rv unscaled? (/ (expt (/ (- x (mu rv)) (sigma rv)) 2) -2)))

(defmethod pdf ((rv normal) x &optional unscaled)
  (declare (optimize (speed 3))
           (double-float x)
           (ignore unscaled))
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

(define-cached-slot (rv normal generator)
  (declare (optimize (speed 3)))
  (bind (((:slots-read-only mu sigma) rv))
    (declare (double-float mu sigma))
    (lambda ()
      (from-standard-normal (draw-standard-normal) mu sigma))))


;;; ****************************************************************
;;; Truncated normal distribution (univariate).
;;; ****************************************************************

(defclass truncated-normal (univariate)
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

(defmethod pdf ((rv truncated-normal) x &optional unscaled)
  (declare (ignore unscaled))
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

(define-cached-slot (rv truncated-normal generator)
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
              (lambda* (draw-left-right-truncated-standard-normal
                        left width 1d0)))
             ;; whole support above 0, need to test
             ((< 0d0 left)
              (let ((alpha (truncated-normal-optimal-alpha left)))
                (if (truncated-normal-left-p alpha left right)
                    ;; optimal to try and reject if not good
                    (lambda* (try ((x (draw-left-truncated-standard-normal
                                       left alpha)))
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
  
;;; ****************************************************************
;;; Lognormal distribution
;;; ****************************************************************

(defclass log-normal (univariate log-pdf-constant)
  ((mu :initarg :mu :initform 0d0 :reader mu :type double-float)
   (sigma :initarg :sigma :initform 1d0 :reader sigma
          :type positive-double-float))
  (:documentation "Log-normal distribution with location mu and scale sigma."))

(define-printer-with-slots log-normal mu sigma)

(defmethod mean ((rv log-normal))
  (exp (+ (mu rv) (/ (expt (sigma rv) 2) 2))))

(defmethod variance ((rv log-normal))
  (bind (((:slots-r/o mu sigma) rv)
         (sigma^2 (expt sigma 2)))
    (* (1- (exp sigma^2))
       (exp (+ (* 2 mu) sigma^2)))))

(define-cached-slot (rv log-normal log-pdf-constant)
  (- (+ (log (sigma rv)) (log (sqrt (* 2 pi))))))

(defmethod log-pdf ((rv log-normal) x &optional unscaled?)
  (when (plusp x)
    (bind (((:slots-r/o mu sigma) rv)
           (log-x (log x)))
      (scale-log-pdf rv unscaled? 
                     (- (/ (expt (- log-x mu) 2) (expt sigma 2) -2)
                        log-x)))))

(defmethod cdf ((rv log-normal) x)
  (if (plusp x)
      (cdf-standard-normal (to-standard-normal (log x) (mu rv) (sigma rv)))
      0d0))

(define-cached-slot (rv log-normal generator)
  (bind (((:slots-r/o mu sigma) rv))
    (lambda ()
      (exp (from-standard-normal (draw-standard-normal) mu sigma)))))

;;;; ****************************************************************
;;;; Gamma distribution.
;;;;
;;;; Also provides a generator-standard-gamma, which returns a
;;;; generator for a given alpha.
;;;; ****************************************************************

(defclass gamma (univariate log-pdf-constant)
  ((alpha :initarg :alpha :initform 1d0 
          :type positive-double-float :reader alpha
          :documentation "shape parameter")
   (beta :initarg :beta :initform 1d0
         :type positive-double-float :reader beta
         :documentation "scale parameter"))
  (:documentation "Gamma(alpha,beta) distribution, with density
  proportional to x^(alpha-1) exp(-x*beta)"))

(defmethod initialize-instance :after ((rv gamma) &key &allow-other-keys)
  (check-type* (alpha rv) positive-double-float)
  (check-type* (beta rv) positive-double-float))

(define-printer-with-slots gamma alpha beta)

(defmethod mean ((rv gamma))
  (/ (alpha rv) (beta rv)))

(defmethod variance ((rv gamma))
  (* (alpha rv) (expt (beta rv) -2)))

(define-cached-slot (rv gamma log-pdf-constant)
  (bind (((:slots-r/o alpha beta) rv))
    (- (* alpha (log beta))
       (log-gamma alpha))))

(defmethod log-pdf ((rv gamma) x &optional unscaled?)
  (when (plusp x)
    (scale-log-pdf rv unscaled? 
                   (- (* (1- (alpha rv)) (log x))
                      (* (beta rv) x)))))

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

(define-cached-slot (rv gamma generator)
  (declare (optimize (speed 3)))
  (bind (((:slots-read-only alpha beta) rv)
         (standard-generator (generator-standard-gamma alpha)))
    (declare (double-float alpha beta)
             (univariate-continuous-generator standard-generator))
    (lambda ()
      (/ (funcall standard-generator) beta))))



;;;; ****************************************************************
;;;; Inverse gamma distribution.
;;;; ****************************************************************

(defclass inverse-gamma (univariate log-pdf-constant)
  ((alpha :initarg :alpha :initform 1d0 
          :type positive-double-float :reader alpha
          :documentation "shape parameter")
   (beta :initarg :beta :initform 1d0
         :type positive-double-float :reader beta
         :documentation "scale parameter"))
  (:documentation "Inverse-Gamma(alpha,beta) distribution, with
  density p(x) proportional to x^(-alpha+1) exp(-beta/x)"))

(define-printer-with-slots inverse-gamma alpha beta)

(defmethod initialize-instance :after ((rv inverse-gamma) &key &allow-other-keys)
  (check-type* (alpha rv) positive-double-float)
  (check-type* (beta rv) positive-double-float))

(defmethod mean ((rv inverse-gamma))
  (bind (((:slots-read-only alpha beta) rv))
    (unless (< 1 alpha)
      (error "Mean is defined only for ALPHA > 1"))
    (/ beta (1- alpha))))

(defmethod variance ((rv inverse-gamma))
  (bind (((:slots-read-only alpha beta) rv))
    (unless (< 2 alpha)
      (error "Mean is defined only for ALPHA > 2"))
    (/ (expt beta 2) (expt (1- alpha) 2) (- alpha 2))))

(define-cached-slot (rv inverse-gamma log-pdf-constant)
  (bind (((:slots-r/o alpha beta) rv))
    (- (* alpha (log beta))
       (log-gamma alpha))))

(defmethod log-pdf ((rv inverse-gamma) x &optional unscaled?)
  (when (plusp x)
   (scale-log-pdf rv unscaled? (- (* (- (1+ (alpha rv))) (log x))
                                  (/ (beta rv) x)))))

(define-cached-slot (rv inverse-gamma generator)
  (declare (optimize (speed 3)))
  (bind (((:slots-read-only alpha beta) rv)
         (standard-generator (generator-standard-gamma alpha)))
    (declare (double-float alpha beta)
             (univariate-continuous-generator standard-generator))
    (lambda ()
      (/ beta (funcall standard-generator)))))

;;;; ****************************************************************
;;;; Chi-square and inverse-chi-square distribution (both scaled).
;;;;
;;;; We just reparametrize and rely on GAMMA and INVERSE-GAMMA.
;;;; ****************************************************************

(defclass chi-square (gamma)
  ((nu :accessor nu :initarg :nu)))

(define-printer-with-slots chi-square nu)

(defmethod initialize-instance :before ((rv chi-square) &key nu
                                        (alpha nil alpha-provided-p)
                                        (beta nil beta-provided-p)
                                        &allow-other-keys)
  (declare (ignore alpha beta))
  (assert (and (not alpha-provided-p)
               (not beta-provided-p)) ()
          "Can't specify ALPHA and/or BETA for a CHI-SQUARE
          distribution, those are calculated internally.")
  (with-slots (alpha beta) rv
    (setf alpha (/ nu 2)
          beta 0.5d0)))

(defclass inverse-chi-square (inverse-gamma)
  ((nu :accessor nu :initarg :nu)
   (scale :accessor scale :initarg :scale)))

(define-printer-with-slots inverse-chi-square nu scale)

(defmethod initialize-instance :before ((rv inverse-chi-square) &key nu (scale 1d0)
                                        (alpha nil alpha-provided-p)
                                        (beta nil beta-provided-p)
                                        &allow-other-keys)
  (declare (ignore alpha beta))
  (assert (and (not alpha-provided-p)
               (not beta-provided-p)) ()
          "Can't specify ALPHA and/or BETA for a CHI-SQUARE
          distribution, those are calculated internally.")
  (bind ((nu/2 (/ nu 2))
         ((:slots alpha beta) rv))
    (setf alpha nu/2
          beta (* nu/2 (expt scale 2))))
  rv)

;;;; ****************************************************************
;;;; Beta distribution.
;;;; ****************************************************************

(defclass beta (univariate)
  ((alpha :initarg :alpha :initform 1d0 
          :type positive-double-float :reader alpha
          :documentation "shape parameter alpha")
   (beta :initarg :beta :initform 1d0
         :type positive-double-float :reader beta
         :documentation "shape parameter beta"))
  (:documentation "Beta(alpha,beta) distribution, with density
  proportional to x^(alpha-1)*(1-x)^(beta-1)"))

(define-printer-with-slots beta alpha beta)

(defmethod mean ((rv beta))
  (bind (((:slots-read-only alpha beta) rv))
    (/ alpha (+ alpha beta))))

(defmethod variance ((rv beta))
  (bind (((:slots-read-only alpha beta) rv)
         (sum (+ alpha beta)))
    (/ (* alpha beta) (* (expt sum 2) (1+ sum)))))
    
(define-cached-slot (rv beta generator)
  (declare (optimize (speed 3)))
  (bind (((:slots-read-only alpha beta) rv)
         (alpha-gen (generator-standard-gamma alpha))
         (beta-gen (generator-standard-gamma beta)))
    (declare (univariate-continuous-generator alpha-gen beta-gen))
    (lambda ()
      (let ((alpha (funcall alpha-gen))
            (beta (funcall beta-gen)))
        (/ alpha (+ alpha beta))))))


;;;; ****************************************************************
;;;; Discrete distribution.
;;;; ****************************************************************

;;; ?? The implementation may be improved speedwise with declarations
;;; and micro-optimizations.  Not a high priority.  However,
;;; converting arguments to double-float provided a great speedup,
;;; especially in cases when the normalization resulted in rationals
;;; -- comparisons for the latter are quite slow.

(defclass discrete (univariate)
  ((probabilities :initarg :probabilities
                  :type vector
                  :reader probabilities
                  :documentation "normalized probabilities")
   (mean :type real :reader mean)
   (variance :type real :reader variance))
  (:documentation "General discrete distribution with given
probabilities.  Random variates are integers, starting from 0."))

(defmethod rv-type ((rv discrete))
  'fixnum)

(define-printer-with-slots discrete probabilities)

(defun normalize-vector (vector)
  "Normalize vector, no checks."
  (let ((sum (iter
               (for v :in-vector vector)
               (summing v))))
    (map 'vector-double-float (lambda (x) (coerce (/ x sum) 'double-float)) vector)))

(defmethod initialize-instance :after ((rv discrete) &key normalized-p skip-checks-p
                                       &allow-other-keys)
  "Normalized-p indicates that probabilities have been normalized to
sum to 0, skip-checks-p makes the initializer ignore sanity checks (eg
positive probabilities).  If normalized-p, probabilities has to
be coercible to vector-double-float."
  (with-slots (probabilities) rv
    (cond
      ((and normalized-p skip-checks-p) ; nothing to do, except a typecheck
       (setf probabilities (coerce probabilities 'vector-double-float)))
      (normalized-p                     ; check nonnegativity & type
       (setf probabilities (coerce probabilities 'vector-double-float))
       (check-type probabilities vector-positive-double-float))
      (skip-checks-p                    ; need to normalize
       (check-type probabilities vector)
       (setf probabilities (normalize-vector probabilities)))
      (t                                ; check and normalize
       (check-type probabilities vector)
       (assert (every #'plusp probabilities))
       (setf probabilities (normalize-vector probabilities)))))
  rv)

(define-cached-slot (rv discrete mean)
  (bind (((:slots-read-only probabilities) rv))
    (iter
      (for p :in-vector probabilities)
      (for i :from 0)
      (summing (* i p)))))

(define-cached-slot (rv discrete variance)
  (bind (((:slots-read-only probabilities) rv))
    (- (iter
         (for p :in-vector probabilities)
         (for i :from 0)
         (summing (* i i p)))
       (expt (mean rv) 2))))

(defmethod pdf ((rv discrete) i &optional unscaled)
  (declare (ignore unscaled))
  (bind (((:slots-read-only probabilities) rv))
    (if (or (minusp i) (<= (length probabilities) i))
        0
        (aref probabilities i))))

(defmethod cdf ((rv discrete) i)
  ;; ?? not cached, should we?
  (bind (((:slots-read-only probabilities) rv))
    (cond
      ((minusp i) 0)
      ((<= (length probabilities) i) 1)
      (t (iter
           (for j :from 0 :to i)
           (summing (aref probabilities j)))))))

(define-cached-slot (rv discrete generator)
  (declare (optimize (speed 3)))
  (bind (((:slots-read-only probabilities) rv)
         (n (length probabilities)))
    (declare (vector-double-float probabilities))
    (lambda ()
      (let ((x (random 1d0)))
        (block comparison
          (dotimes (i n)
            (let ((p (aref probabilities i)))
              (if (< x p)
                  (return-from comparison i)
                  (decf x p))))
          ;; fallback, mathematically it has a zero chance
          0)))))
