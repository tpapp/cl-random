;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-random-tests)

;;;;
;;;;  Univariate distributions
;;;;

(addtest (cl-random-tests)
  uniform-draws
  (ensure (same-sample-mean-variance (uniform 0 1)))
  (ensure (same-sample-mean-variance (uniform -9 1)))
  (ensure (same-sample-mean-variance (uniform 0 107)))
  (ensure (same-sample-mean-variance (uniform 19 57))))

;; exponential distribution

(addtest (cl-random-tests)
  exponential-draws
  (ensure (same-sample-mean-variance (exponential 1)))
  (ensure (same-sample-mean-variance (exponential 19) 
                              :var-band 0.3d0)))

;; normal distribution

(addtest (cl-random-tests)
  normal-cdf
  ;; note: currently comparing to values from R, would be more useful
  ;; to use "exact" (ie calculated with arbitrary precision) values
  ;; from eg Maxima
  (let ((rv (normal 0 1)))
    (ensure-same (cdf rv -1d0) 0.158655253931457)
    (ensure-same (cdf rv 5d0) 0.999999713348428)
    (ensure-same (cdf rv 0.7d0) 0.758036347776927)))

(addtest (cl-random-tests)
  normal-pdf
  (let ((rv (normal 5 19))
        (*lift-equality-test* #'rel=))
    (ensure-same (pdf rv 2d0) 0.02073685169643458d0)
    (ensure-same (pdf rv 16d0) 0.01775714089407024d0)
    (ensure-same (pdf rv 102d0) 0.0000000459719932600508d0)))

(addtest (cl-random-tests)
  normal-draws
  (ensure (same-sample-mean-variance (normal 0 1)))
  (ensure (same-sample-mean-variance (normal 10 1)))
  (ensure (same-sample-mean-variance (normal 0 12))))

;; truncated normal distribution

;; (addtest (cl-random-tests)
;;   ;; LEFT
;;   ;; not including zero
;;   (ensure (same-sample-mean-variance (make-instance 'truncated-normal :left 0.5d0
;;                                              :mu 1d0 :sigma 0.72d0)))
;;   ;; including zero
;;   (ensure (same-sample-mean-variance (make-instance 'truncated-normal :left -0.7d0
;;                                              :sigma 7d0)))
;;   ;; RIGHT
;;   ;; not including zero
;;   (ensure (same-sample-mean-variance (make-instance 'truncated-normal :right -0.5d0
;;                                              :mu 1.5d0 :sigma 2d0)))
;;   ;; including zero
;;   (ensure (same-sample-mean-variance (make-instance 'truncated-normal :right 0.7d0
;;                                              :mu -4.2d0 :sigma 2d0)))
;;   ;; BOTH LEFT AND RIGHT
;;   ;; wide
;;   (ensure (same-sample-mean-variance (make-instance 'truncated-normal
;;                                              :left -0.7d0 :right 4d0)))
;;   ;; narrow
;;   (ensure (same-sample-mean-variance (make-instance 'truncated-normal
;;                                              :left -0.25d0 :right 0.1d0)))
;;   ;; support above 0
;;   (ensure (same-sample-mean-variance (make-instance 'truncated-normal
;;                                              :left 1d0 :right 5d0)))
;;   ;; support below 0
;;   (ensure (same-sample-mean-variance (make-instance 'truncated-normal
;;                                              :left -9d0 :right -5d0)))
;;   ;; NOT TRUNCATED
;;   (ensure (same-sample-mean-variance (make-instance 'truncated-normal :mu 5d0 :sigma 9d0))))


;; log-normal distribution

(addtest (cl-random-tests)
  log-normal-pdf
  (let ((rv (log-normal 5 19))
        (*lift-equality-test* #'rel=))
    (ensure-same (pdf rv 2d0) 0.0102321986262048220)
    (ensure-same (pdf rv 16d0)  0.0013033232558763653d0)
    (ensure-same (pdf rv 102d0)  0.0002058124737511057d0)))

(addtest (cl-random-tests)
  log-normal-draws
  (ensure (same-sample-mean-variance (log-normal 0 1)))
  (ensure (same-sample-mean-variance (log-normal 10 1)))
  (ensure (same-sample-mean-variance (log-normal 0 0.5))))

;; gamma distribution

(addtest (cl-random-tests)
  gamma-draws
  (ensure-error (gamma 9 0)) 
  (ensure-error (gamma -7 1))
  (ensure (same-sample-mean-variance (gamma 1 1)))
  (ensure (same-sample-mean-variance (gamma 12 1)))
  (ensure (same-sample-mean-variance (gamma 8 1)))
  (ensure (same-sample-mean-variance (gamma 0.5 pi))))

(addtest (cl-random-tests)
  inverse-gamma-draws
  (ensure (same-sample-mean-variance (inverse-gamma 12 1)))
  (ensure (same-sample-mean-variance (inverse-gamma 4 8)))
  (ensure-error (mean (inverse-gamma 0.5 1)))
  (ensure-error (variance (inverse-gamma 1.5 1))))

(addtest (cl-random-tests)
  chi-square-moments
  (let* ((nu 9d0)
         (rv (chi-square nu)))
    (ensure-same (mean rv) nu)
    (ensure-same (variance rv) (* 2 nu))))

(addtest (cl-random-tests)
  inverse-chi-square-moments
  (let* ((nu 9)
         (scale pi)
         (rv (inverse-chi-square nu scale)))
    (ensure-same (mean rv) (* (/ nu (- nu 2)) (expt scale 2)))
    (ensure-same (variance rv) (/ (* 2 (expt nu 2) (expt scale 4))
                                  (* (expt (- nu 2) 2) (- nu 4))))))

;; beta distribution

(addtest (cl-random-tests)
  beta-draws
  (ensure (same-sample-mean-variance (beta 1 1)))
  (ensure (same-sample-mean-variance (beta 12 1)))
  (ensure (same-sample-mean-variance (beta 1 8)))
  (ensure (same-sample-mean-variance (beta 0.5 pi))))


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
  (mean (map 'vector 
             (lambda (f p)
               (let ((e (* n p)))
                 (/ (abs (- f e)) e)))
             frequencies probabilities)))

(defun discrete-deviation (discrete-rv &key (n 100000))
  "Probably a chi-square test would be better, but this isn't very important."
  (average-relative-deviation (empirical-frequencies discrete-rv :n n)
                              (probabilities discrete-rv)
                              :n n))

(addtest (cl-random-tests)
  discrete-moments
  (let ((rv0 (discrete #(1/4 1/4 1/2)))
        (rv1 (discrete #(1/3 1/6 1/3))) ; rescaled
        (*lift-equality-test* #'equalp))
    ;; first distribution
    (ensure-same (probabilities rv0) #(0.25d0 0.25 0.5))
    (ensure-same (mean rv0) 1.25)
    (ensure-same (variance rv0) 0.6875)
    (ensure-same (cdf rv0 1) 0.5)
    ;; second distribution -- this is rescaled to sum to 1
    (ensure-same (probabilities rv1) #(0.4d0 0.2d0 0.4d0))
    (ensure-same (mean rv1) 1)
    (ensure-same (variance rv1) 0.8d0)
    (ensure-same (cdf rv1 0) 0.4d0)))

(addtest (cl-random-tests)
  discrete-draws
  (ensure (< (discrete-deviation (discrete #(1/6 1/2 1/3))) 0.01))
  (ensure (< (discrete-deviation (discrete #(1/6 1/2 1/3 9 17 21))) 0.05)))
