;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-random-tests)

(defsuite univariate-tests (tests))

(defun assert-q-cdf-consistency (rv q)
  "Calculate X from Q, then Q from X, and compare."
  (assert-equality #'num= (cdf rv (quantile rv q)) q))

;;;;
;;;;  Univariate distributions
;;;;

(deftest uniform-draws (univariate-tests)
  (assert-true (same-univariate-moments (r-uniform 0 1)))
  (assert-true (same-univariate-moments (r-uniform -9 1)))
  (assert-true (same-univariate-moments (r-uniform 0 107)))
  (assert-true (same-univariate-moments (r-uniform 19 57))))

;; exponential distribution

(deftest exponential-draws (univariate-tests)
  (assert-true (same-univariate-moments (r-exponential 1)))
  (assert-true (same-univariate-moments (r-exponential 19)
                                        :var-band 0.3d0)))

;; normal distribution

(deftest normal-cdf (univariate-tests)
  ;; note: currently comparing to values from R, would be more useful to use "exact" (ie calculated with arbitrary precision) values from eg Maxima
  (let ((rv (r-normal 0 1)))
    (assert-equality #'num= 0.158655253931457 (cdf rv -1d0))
    (assert-equality #'num= 0.999999713348428 (cdf rv 5d0))
    (assert-equality #'num= 0.758036347776927 (cdf rv 0.7d0))))

(deftest normal-pdf (univariate-tests)
  (let ((rv (r-normal 5 (expt 19 2))))
    (assert-equality #'num= 0.02073685169643458d0 (pdf rv 2d0))
    (assert-equality #'num= 0.01775714089407024d0 (pdf rv 16d0))
    (assert-equality #'num= 0.0000000459719932600508d0 (pdf rv 102d0))))

(deftest normal-draws (univariate-tests)
  (assert-true (same-univariate-moments (r-normal 0 1)))
  (assert-true (same-univariate-moments (r-normal 10 1)))
  (assert-true (same-univariate-moments (r-normal 0 144))))

;; truncated normal distribution

(deftest truncated-normal-functions (univariate-tests)
  (let ((rv (r-truncated-normal -0.5 nil)))
    (assert-equality #'num= 0 (pdf rv -0.7))
    (assert-equality #'num= 0.5515669 (pdf rv -0.3))
    (assert-equality #'num= 0 (cdf rv -0.7))
    (assert-equality #'num= -0.5 (quantile rv 0))
    (assert-equality #'num= 0.1063703 (cdf rv -0.3))
    (assert-equality #'num= -0.3 (quantile rv 0.1063703)))
  (let ((rv (r-truncated-normal -0.5 nil -0.4 2.5)))
    (assert-equality #'num= 0 (pdf rv -0.7))
    (assert-equality #'num= 0.3090382 (pdf rv -0.3))
    (assert-equality #'num= 0 (cdf rv -0.7))
    (assert-equality #'num= 0.06184061 (cdf rv -0.3))
    (assert-equality #'num= -0.5 (quantile rv 0))
    (assert-equality #'num= -0.3 (quantile rv 0.06184061))))

(deftest truncated-normal-draws (univariate-tests)
  ;; LEFT
  ;; not including zero
  (assert-true (same-univariate-moments (r-truncated-normal 0.5d0 nil 1d0 0.72d0)))
  ;; (assert-true (same-univariate-moments
  ;;          (r-truncated-normal 100 nil 0 1)))
  ;; including zero
  (assert-true (same-univariate-moments (r-truncated-normal -0.7d0 nil 0d0 7d0)))
  ;; ;; RIGHT
  ;; ;; not including zero
  ;; (assert-true (same-univariate-moments (make-instance 'truncated-normal :right -0.5d0
  ;;                                            :mu 1.5d0 :sigma 2d0)))
  ;; ;; including zero
  ;; (assert-true (same-univariate-moments (make-instance 'truncated-normal :right 0.7d0
  ;;                                            :mu -4.2d0 :sigma 2d0)))
  ;; ;; BOTH LEFT AND RIGHT
  ;; ;; wide
  ;; (assert-true (same-univariate-moments (make-instance 'truncated-normal
  ;;                                            :left -0.7d0 :right 4d0)))
  ;; ;; narrow
  ;; (assert-true (same-univariate-moments (make-instance 'truncated-normal
  ;;                                            :left -0.25d0 :right 0.1d0)))
  ;; ;; support above 0
  ;; (assert-true (same-univariate-moments (make-instance 'truncated-normal
  ;;                                            :left 1d0 :right 5d0)))
  ;; ;; support below 0
  ;; (assert-true (same-univariate-moments (make-instance 'truncated-normal
  ;;                                            :left -9d0 :right -5d0)))
  ;; NOT TRUNCATED
  (assert-true (same-univariate-moments (r-truncated-normal nil nil 5d0 9d0))))

;; log-normal distribution

(deftest log-normal-pdf (univariate-tests)
  (let ((rv (r-log-normal 5 19)))
    (assert-equality #'num= 0.0102321986262048220 (pdf rv 2d0))
    (assert-equality #'num= 0.0013033232558763653d0 (pdf rv 16d0))
    (assert-equality #'num= 0.0002058124737511057d0 (pdf rv 102d0))))

(deftest log-normal-draws (univariate-tests)
  (assert-true (same-univariate-moments (r-log-normal 0 1)))
  (assert-true (same-univariate-moments (r-log-normal 10 1)))
  (assert-true (same-univariate-moments (r-log-normal 0 0.5))))

;;; Student's T distribution

(deftest t-draws (univariate-tests)
  (assert-true (same-univariate-moments (r-t 0 1 4.1)))
  (assert-true (same-univariate-moments (r-t 10 1 5)))
  (assert-true (same-univariate-moments (r-t 0 112 9)))
  (assert-condition error (r-t 0 1 0))
  (assert-condition error (r-t 0 1 -1))
  (assert-condition error (r-t 0 -1 3)))

;; gamma distribution

(deftest gamma-draws (univariate-tests)
  (assert-condition error (r-gamma 9 0))
  (assert-condition error (r-gamma -7 1))
  (assert-true (same-univariate-moments (r-gamma 1 1)))
  (assert-true (same-univariate-moments (r-gamma 12 1)))
  (assert-true (same-univariate-moments (r-gamma 8 1)))
  (assert-true (same-univariate-moments (r-gamma 0.5 pi))))

(deftest gamma-cdf (univariate-tests)
  (assert-equality #'num= 0.6321206d0 (cdf (r-gamma 1 1) 1d0))
  (assert-equality #'num= 0.9926168 (cdf (r-gamma 1.5 2) 3))
  (assert-q-cdf-consistency (r-gamma 2 3) .2)
  (assert-q-cdf-consistency (r-gamma 1 9) .7))

(deftest inverse-gamma-draws (univariate-tests)
  (assert-true (same-univariate-moments (r-inverse-gamma 12 1)))
  (assert-true (same-univariate-moments (r-inverse-gamma 4 8)))
  (assert-condition error (mean (r-inverse-gamma 0.5 1)))
  (assert-condition error (variance (r-inverse-gamma 1.5 1))))

(deftest chi-square-moments (univariate-tests)
  (let* ((nu 9d0)
         (rv (r-chi-square nu)))
    (assert-equality #'num= nu (mean rv))
    (assert-equality #'num= (* 2 nu) (variance rv))))

(deftest chi-square-cdf (univariate-tests)
  (assert-equality #'num= 0.2211992 (cdf (r-chi-square 2) 0.5)))

(deftest inverse-chi-square-moments (univariate-tests)
  (let* ((nu 9)
         (s^2 pi)
         (rv (r-inverse-chi-square nu s^2)))
    (assert-equality #'num= (* (/ nu (- nu 2)) s^2) (mean rv))
    (assert-equality #'num= (/ (* 2 (expt nu 2) (expt s^2 2))
                               (* (expt (- nu 2) 2) (- nu 4)))
        (variance rv))))

;; beta distribution

(deftest beta-draws (univariate-tests)
  (assert-true (same-univariate-moments (r-beta 1 1)))
  (assert-true (same-univariate-moments (r-beta 12 1)))
  (assert-true (same-univariate-moments (r-beta 1 8)))
  (assert-true (same-univariate-moments (r-beta 0.5 pi)))
  (assert-equality #'num= 0.001343620695608248967121
		   (quantile (r-beta 0.3d0 0.6d0) 0.11)))


;; rayleigh distribution

(deftest rayleigh-draws (univariate-tests)
  (assert-true (same-univariate-moments (r-rayleigh 1)))
  (assert-true (same-univariate-moments (r-rayleigh 0.0001d0)))
  (assert-true (same-univariate-moments (r-rayleigh 1d0)))
  (assert-true (same-univariate-moments (r-rayleigh 100)))
  (assert-true (same-univariate-moments (r-rayleigh PI))))

(deftest rayleigh-cdf (univariate-tests)
  (assert-equality #'num= 0.0d0
		   (cdf (r-rayleigh 250d0) 0d0))
  (assert-equality #'num= 7.99997d-6
		   (cdf (r-rayleigh 250d0) 1d0))
  (assert-equality #'num= 0.00079968d0
		   (cdf (r-rayleigh 250d0) 10d0))
  (assert-equality #'num= 0.0768837d0
		   (cdf (r-rayleigh 250d0) 100d0)))



;; general discrete distribution

(defun empirical-frequencies (discrete-rv &key (n 100000))
  "Count realizations for each value."
  (check-type n fixnum)
  (let ((count (make-array (length (probabilities discrete-rv))
                           :element-type 'fixnum :initial-element 0)))
    (loop repeat n
          do (incf (aref count (draw discrete-rv))))
    count))

(defun average-relative-deviation (frequencies probabilities &key
                                                             (n (reduce #'+ frequencies)))
  "Sum of the absolute values of relative deviations from expected probabilities."
  (assert (= (length frequencies) (length probabilities)))
  (clnu.stats:mean (map 'vector
                        (lambda (f p)
                          (let ((e (* n p)))
                            (/ (abs (- f e)) e)))
                        frequencies probabilities)))

(defun discrete-deviation (discrete-rv &key (n 100000))
  "Probably a chi-square test would be better, but this isn't very important."
  (average-relative-deviation (empirical-frequencies discrete-rv :n n)
                              (probabilities discrete-rv)
                              :n n))

(deftest discrete-moments (univariate-tests)
  (let ((rv0 (r-discrete #(1/4 1/4 1/2)))
        (rv1 (r-discrete #(1/3 1/6 1/3)))) ; will be rescaled
    ;; first distribution
    (assert-equality #'num= #(0.25d0 0.25 0.5) (probabilities rv0))
    (assert-equality #'num= 1.25 (mean rv0))
    (assert-equality #'num= 0.6875 (variance rv0))
    (assert-equality #'num= 0.5 (cdf rv0 1))
    ;; second distribution -- this is rescaled to sum to 1
    (assert-equality #'num= #(0.4d0 0.2d0 0.4d0) (probabilities rv1))
    (assert-equality #'num= 1 (mean rv1))
    (assert-equality #'num= 0.8d0 (variance rv1))
    (assert-equality #'num= 0.4d0 (cdf rv1 0))))

(deftest discrete-draws (univariate-tests)
  (assert-true (< (discrete-deviation (r-discrete #(1/6 1/2 1/3))) 0.01))
  (assert-true (< (discrete-deviation (r-discrete #(1/6 1/2 1/3 9 17 21))) 0.05)))

;; Bernoulli distribution

(deftest bernoulli-draws (univariate-tests)
  (assert-true (same-univariate-moments (r-bernoulli 0.001)))
  (assert-true (same-univariate-moments (r-bernoulli 0.3)))
  (assert-true (same-univariate-moments (r-bernoulli 1/3)))
  (assert-true (same-univariate-moments (r-bernoulli 0.8))))

;; binomial distribution

(deftest binomial-draws (univariate-tests)
  (assert-true (same-univariate-moments (r-binomial 0.2 100)))
  (assert-true (same-univariate-moments (r-binomial 0.1 1000)))
  (assert-true (same-univariate-moments (r-binomial 0.9 200)))
  (assert-true (same-univariate-moments (r-binomial 0.5 50))))

;; geometric distribution

(deftest geometric-draws (univariate-tests)
  ;; TODO: What's wrong with p=1?
  ;; (assert-true (same-univariate-moments (r-geometric 1))) 
  (assert-true (same-univariate-moments (r-geometric 0.3)))
  (assert-true (same-univariate-moments (r-geometric 1/6)))
  (assert-true (same-univariate-moments (r-geometric 0.98))))
