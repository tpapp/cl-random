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
  linear-regression-exact
  (bind ((x (clo 1 1 :/
                 1 2
                 1 3))
         (y (clo 1 1 3))
         (lr (linear-regression y x :r^2? t))
         ((:accessors-r/o mean variance) (mv-normal lr)))
    (ensure-same mean (clo -1/3 1))
    (ensure-same variance (clo :hermitian 
                               14/9 -2/3 :/
                               -2/3 1/3))
    (ensure-same (r^2 lr) 0.75)))

(addtest (regressions-tests)
  linear-regression-random
  (let* (;; sample
         (n 1000)
         (sigma 9d0)
         (beta (clo 1 2))
         (x (collect-rows n (lambda ()
                              (vector (* 3 (draw-standard-normal))
                                      (+ 5 (draw-standard-normal))))
                          'dense-matrix))
         (y (e+ (mm x beta)
                (lla-vector n :double (generator* 'normal :sigma sigma))))
         ;; regressions
         (lr (linear-regression y x))
         ;; posterior draws
         (m 1000000)
         (beta-draws (make-matrix m (length beta) :double))
         (sigma-draws (lla-vector m :double)))
    ;; ;; draw posterior
    (iter
      (for index :from 0 :below m)
      (bind (((:values beta sigma) (draw lr)))
        (setf (sub beta-draws index t) beta
              (aref sigma-draws index) sigma)))
    ;; ;; check mean of sigma
    (ensure (< (reldiff (* (s^2 lr) (mean (scaling-factor lr)))
                        (mean sigma-draws))
               1d-2))
    (bind (((:values b-m b-v) (column-mean-variances beta-draws))
           (m-rd (ereldiff (mean lr) b-m))
           (v-rd (ereldiff (variance lr) b-v)))
      (format t "~2&mean: ~A mean reldiff: ~A~%" (mean lr) m-rd)
      (format t "~2&variance: ~A~%variance reldiff: ~A~%" (variance lr) m-rd)
      (ensure (< (emax m-rd) 1d-3))
      (ensure (< (emax v-rd) 1d-2)))))
