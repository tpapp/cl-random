;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-random-unit-tests)

(deftestsuite regressions-tests (cl-random-unit-tests)
  ()
  (:equality-test #'lla=))

;;; utility functions

(defun random-y-x (n k &optional 
                   (x-rv (make-instance 'normal :sigma 9d0))
                   (e-rv (make-instance 'normal :sigma 2d0))
                   (beta (lla-vector k :double (generator* 'normal))))
  "Generate random Y and X for testing regressions."
  (bind ((x (make-matrix n k :double :initial-element (generator x-rv)))
         (y (e+ (mm x beta)
                (lla-vector n :double (generator e-rv)))))
    (values y x)))

;;; linear regression with known variance

(addtest (regressions-tests)
  dummy-observations-kv
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

(addtest (regressions-tests)
  dummy-2phase-kv
  ;; estimate in one and two steps, then compare
  (bind ((k 10)
         (n 100)
         ((:values y x) (random-y-x (* 2 n) k))
         (w (lla-vector (* 2 n) :double (generator* 'gamma))) ; weights
         ;; single step
         (p2 (linear-regression-kv y x :variance-right-sqrt (as-diagonal w)))
         ;; two steps, first half
         (h1 (cons 0 n))
         (p1 (linear-regression-kv (sub y h1) (sub x h1 t)
                                   :variance-right-sqrt 
                                   (as-diagonal (sub w h1))))
         ;; second half, using first half as prior
         (h2 (cons n 0))
         (p2-1 (linear-regression-kv (sub y h2) (sub x h2 t)
                                     :prior p1
                                     :variance-right-sqrt 
                                     (as-diagonal (sub w h2)))))
    (ensure-same (mean p2) (mean p2-1))
    (ensure-same (variance p2) (variance p2-1))))

;;; linear regression

(bind (((:values y x) (random-y-x 10 2))
       (lr (linear-regression y x))
       ((:values y-dummy x-dummy) (dummy-observations lr))
       (lr2 (linear-regression y-dummy x-dummy)))
  (d:v y-dummy x-dummy)
  (d:v (mean lr) :/ (mean lr2))
  (d:v (s^2 lr) :/ (s^2 lr2))
  )

(addtest (regressions-tests)
  dummy-2phase
  (bind ((k 2)
         (n 10)
         ((:values y x) (random-y-x (* 2 n) k))
         ;; single step
         (p2 (linear-regression y x))
         ;; two steps, first half
         (h1 (cons 0 n))
         (p1 (linear-regression (sub y h1) (sub x h1 t)))
         ;; second half, using first half as prior
         (h2 (cons n 0))
         (p2-1 (linear-regression (sub y h2) (sub x h2 t) :prior p1)))
    (ensure-same (mean p2) (mean p2-1))
    (ensure-same (variance p2) (variance p2-1))
    (ensure-same (s^2 p2) (s^2 p2-1))
    (ensure-same (nu p2) (nu p2-1))))

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
