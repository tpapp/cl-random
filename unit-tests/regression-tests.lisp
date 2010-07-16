;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-random-unit-tests)

(deftestsuite regressions-tests (cl-random-unit-tests)
  ()
  (:equality-test #'lla=))


;;; linear regression with known variance

(addtest (regressions-tests)
  dummy-observations
  (bind ((mean (clo :double 2 3))
         (variance-right-sqrt (clo :double 
                                   5 7 :/
                                   11 13))
         (variance (mm t variance-right-sqrt))
         (prior (make-instance 'mv-normal :mean mean
                               :variance-right-sqrt variance-right-sqrt))
         ((:values y x) (dummy-observations prior))
         (lr (linear-regression-kv y x)))
    (ensure-same (mean lr) mean)))

;;; linear regression

(addtest (regressions-tests)
  linear-regression
  (bind ((x (clo 1 1 :/
                 1 2
                 1 3))
         (y (clo 1 1 3))
         ((:values lr s^2 r^2) (linear-regression y x :r^2? t))
         ((:accessors-r/o mean variance) (mv-normal lr))
         (*lift-equality-test* #'lla=))
    (ensure-same mean (clo -1/3 1))
    (ensure-same variance (clo :hermitian 
                               14/9 -2/3 :/
                               -2/3 1/3))
    (ensure-same r^2 0.75)))

(defparameter *beta* (clo 1 2))
(defparameter *x* (collect-rows 5000 (lambda ()
                                       (vector (* 3 (draw-standard-normal))
                                               (+ 5 (draw-standard-normal))))
                                'dense-matrix))
(defparameter *y* (e+ (mm *x* *beta*)
                      (collect-vector (nrow *x*) 
                                      (lambda ()
                                        (* 9d0 (draw-standard-normal))))))
(bind (((:values lr s^2 nil) (linear-regression *y* *x*)))
  (defparameter *lr* lr)
  (defparameter *s^2* s^2))

(defparameter *sigmasq-mean*
  (let ((n 100000))
    (* (iter
         (repeat n)
         (for (values beta sigma) := (draw *lr*))
         (summing sigma))
       (/ *s^2* n))))

(bind ((matrix (xcollect 100000 (generator *lr*)))
       ((:values sample-mean sample-variance)
        (column-mean-variances matrix)))
  (defparameter *sample-variance* sample-variance))
(x-rel-diff *sample-variance* (variance *lr*))  ; should be near 0
