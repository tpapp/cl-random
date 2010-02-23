(in-package :cl-random)

(use-package :lift)
(asdf:load-system :named-readtables) 
(named-readtables:in-readtable lla:v-syntax)

;; SUPPORT FUNCTIONS

(defparameter *allowed-difference* 1d-5
  ;; default is for catching major mistakes, use smaller value for fine points
  "Maximum allowed difference used by approx=.")

(defun approx= (a b)
  "Approximately equal, by *allowed-difference*."
  (< (abs (- a b)) *allowed-difference*))

(defun relative-difference (a b)
  "Difference between a and b, compared to the smaller one in absolute value."
  (if (and (zerop a) (zerop b))
      0
      (/ (- a b) (max (abs a) (abs b)))))

(defun rel= (a b)
  "Relatively equal, by *allowed-difference*."
  (approx= (relative-difference a b) 0))

(defun calculate-repeated-mean-variance (rv n)
  "Return (values mean variance) of n draws from rv (a random variable
class), also, rv can be a function."
  (let ((mean 0d0)
	(ss 0d0)
	(draw-function (if (functionp rv)
			   rv
			   (lambda () (draw rv)))))
    (dotimes (i n)
      (let ((draw (funcall draw-function)))
	(incf mean (/ draw n))
	(incf ss (/ (expt draw 2) n))))
    (values mean (- ss (expt mean 2)))))

(defun same-mean-variance (rv &key (n 1000000) (z-band 4d0)
			   (var-band 0.1))
  "Quick and dirty test for the sample having the same mean and
variance as the theoretical distribution.  z-band is for comparing a
z-score, var-band is for rel= comparisons.  NOTE: this test doesn't
embody a lot of theory about sampling properties.  Bands are meant to
be quite wide, we are here to catch outrageous implementation
mistakes, and don't want to be stopped by false positives all the
time.  Some distributions have fat tails, they need a bigger
var-band."
  (bind (((:values mean variance)
	  (calculate-repeated-mean-variance rv n))
	 (z (* (- mean (mean rv)) (sqrt (/ n (variance rv)))))
	 (ok-p (and (< (abs z) z-band)
		    (let ((*allowed-difference* var-band))
		      (rel= variance (variance rv))))))
    (unless ok-p
      (format t "~&moment mismatch for distribution ~a~%" rv)
      (format t "theoretical/empirical mean: ~a / ~a, variance: ~a / ~a~%"
	      (mean rv) mean (variance rv) variance))
    ok-p))

;; TEST SUITES

(deftestsuite cl-random-unit-tests () ()
  (:equality-test #'approx=))

;; EXTERNAL

(defun run-cl-random-tests ()
  "Run all the tests for LLA."
  (run-tests :suite 'cl-random-unit-tests))

;; TESTS

;;;;
;;;;  Univariate distributions
;;;;

(addtest (cl-random-unit-tests)
  uniform-draws
  (ensure (same-mean-variance (make-instance 'uniform)))
  (ensure (same-mean-variance (make-instance 'uniform :left -9d0)))
  (ensure (same-mean-variance (make-instance 'uniform :right 107d0)))
  (ensure (same-mean-variance (make-instance 'uniform :left 19d0 :right 57d0))))

;; exponential distribution

(addtest (cl-random-unit-tests)
  exponential-draws
  (ensure (same-mean-variance (make-instance 'exponential)))
  (ensure (same-mean-variance (make-instance 'exponential :beta 19d0) 
                              :var-band 0.3d0)))

;; normal distribution

(addtest (cl-random-unit-tests)
  normal-cdf
  ;; note: currently comparing to values from R, would be more useful
  ;; to use "exact" (ie calculated with arbitrary precision) values
  ;; from eg Maxima
  (ensure-same (cdf-standard-normal -1d0) 0.158655253931457)
  (ensure-same (cdf-standard-normal 5d0) 0.999999713348428)
  (ensure-same (cdf-standard-normal 0.7d0) 0.758036347776927))

(addtest (cl-random-unit-tests)
  normal-pdf
  (let ((rv (make-instance 'normal :mu 5d0 :sigma 19d0)))
    (ensure-same (pdf rv 2d0) 0.02073685169643458d0 :test #'rel=)
    (ensure-same (pdf rv 16d0) 0.01775714089407024d0 :test #'rel=)
    (ensure-same (pdf rv 102d0) 0.0000000459719932600508d0 :test #'rel=)))

(addtest (cl-random-unit-tests)
  normal-draws
  (ensure (same-mean-variance (make-instance 'normal)))
  (ensure (same-mean-variance (make-instance 'normal :mu 10d0)))
  (ensure (same-mean-variance (make-instance 'normal :sigma 12d0))))

;; truncated normal distribution

(addtest (cl-random-unit-tests)
  ;; LEFT
  ;; not including zero
  (ensure (same-mean-variance (make-instance 'truncated-normal :left 0.5d0
                                             :mu 1d0 :sigma 0.72d0)))
  ;; including zero
  (ensure (same-mean-variance (make-instance 'truncated-normal :left -0.7d0
                                             :sigma 7d0)))
  ;; RIGHT
  ;; not including zero
  (ensure (same-mean-variance (make-instance 'truncated-normal :right -0.5d0
                                             :mu 1.5d0 :sigma 2d0)))
  ;; including zero
  (ensure (same-mean-variance (make-instance 'truncated-normal :right 0.7d0
                                             :mu -4.2d0 :sigma 0.1d0)))
  ;; BOTH LEFT AND RIGHT
  ;; wide
  (ensure (same-mean-variance (make-instance 'truncated-normal
                                             :left -0.7d0 :right 4d0)))
  ;; narrow
  (ensure (same-mean-variance (make-instance 'truncated-normal
                                             :left -0.25d0 :right 0.1d0)))
  ;; support above 0
  (ensure (same-mean-variance (make-instance 'truncated-normal
                                             :left 1d0 :right 5d0)))
  ;; support below 0
  (ensure (same-mean-variance (make-instance 'truncated-normal
                                             :left -9d0 :right -5d0)))
  ;; NOT TRUNCATED
  (ensure (same-mean-variance (make-instance 'truncated-normal :mu 5d0 :sigma 9d0))))


;; gamma distribution

(addtest (cl-random-unit-tests)
  gamma-draws
  (ensure-error (make-instance 'gamma :alpha 9)) 
  (ensure-error (make-instance 'gamma :alpha -7d0))
  (ensure (same-mean-variance (make-instance 'gamma)))
  (ensure (same-mean-variance (make-instance 'gamma :alpha 12d0)))
  (ensure (same-mean-variance (make-instance 'gamma :beta 8d0)))
  (ensure (same-mean-variance (make-instance 'gamma :alpha 0.5d0 :beta pi))))

(addtest (cl-random-unit-tests)
  inverse-gamma-draws
  (ensure (same-mean-variance (make-instance 'inverse-gamma :alpha 12d0)))
  (ensure (same-mean-variance (make-instance 'inverse-gamma :alpha 4d0 :beta 8d0)))
  (ensure-error (mean (make-instance 'inverse-gamma :alpha 0.5d0)))
  (ensure-error (variance (make-instance 'inverse-gamma :alpha 1.5d0))))

(addtest (cl-random-unit-tests)
  chi-square-moments
  (let* ((nu 9d0)
         (rv (make-instance 'chi-square :nu nu)))
    (ensure-same (mean rv) nu)
    (ensure-same (variance rv) (* 2 nu))))

(addtest (cl-random-unit-tests)
  inverse-chi-square-moments
  (let* ((nu 9d0)
         (scale pi)
         (rv (make-instance 'inverse-chi-square :nu nu :scale scale)))
    (ensure-same (mean rv) (* (/ nu (- nu 2)) (expt scale 2)))
    (ensure-same (variance rv) (/ (* 2 (expt nu 2) (expt scale 4))
                                  (* (expt (- nu 2) 2) (- nu 4))))))

;; beta distribution

(addtest (cl-random-unit-tests)
  beta-draws
  (ensure (same-mean-variance (make-instance 'beta)))
  (ensure (same-mean-variance (make-instance 'beta :alpha 12d0)))
  (ensure (same-mean-variance (make-instance 'beta :beta 8d0)))
  (ensure (same-mean-variance (make-instance 'beta :alpha 0.5d0 :beta pi))))


;; general discrete distribution

(addtest (cl-random-unit-tests)
  discrete-draws
  (ensure (same-mean-variance (make-instance 'discrete
                                             :probabilities #(1 2.d0 3))))
  (ensure (same-mean-variance (make-instance 'discrete 
                                             :probabilities (vector 9 pi 3))))
  (ensure (same-mean-variance (make-instance 'discrete
                                             :probabilities #(0.5d0 0.3d0 0.2d0)
                                             :normalized-p t))))

;;;; !!!! lines below have not been incorporated into a proper unit
;;;; !!!! testing framework, just "eyeballing" the results, need to fix -- Tamas

;;;;
;;;;  Multivariate distributions
;;;;

(time
(bind ((mean #vd(3 4))
       (variance #2vd:hermitian(1 0.5 0.5 1))
       (rv (make-instance 'mv-normal :mu mean :sigma variance))
       (matrix (xcollect 100000 (lambda () (draw rv))))
       ((:values sample-mean sample-variance)
        (matrix-mean-variance matrix)))
  (values sample-mean
          sample-variance)))


;;; linear regression

(defparameter *beta* #v(1 2))
(defparameter *x* (xcollect 500 (lambda ()
                                  (vector (* 3 (draw-standard-normal))
                                          (+ 5 (draw-standard-normal))))
                            'dense-matrix))
(defparameter *y* (xmap 'numeric-vector-double #'+
                        (mm *x* *beta*)
                        (xcollect 500 (lambda ()
                                        (* 9d0 (draw-standard-normal))))))
(defparameter *ls* (linear-regression *y* *x*))

(defparameter *sigmamean*
  (let ((n 100000))
    (/ (iter
         (repeat n)
         (for (values beta sigma) := (draw *ls*))
         (summing sigma))
       n)))

(bind ((matrix (xcollect 1000000 (generator *ls*)))
       ((:values sample-mean sample-variance)
        (matrix-mean-variance matrix)))
  (defparameter *sample-variance* sample-variance))
x(x/ *sample-variance* (variance *ls*))  ; should be near 1
