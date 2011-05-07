;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-random-tests)

(deftestsuite regressions-tests (cl-random-tests)
  ()
  (:equality-test #'==))

;;; utility functions

(defun random-y-x (n k &optional 
                   (x-rv (r-normal 0 9))
                   (e-rv (r-normal 0 2))
                   (beta (make-array* k :double
                                      (generator (r-normal 0 1)))))
  "Generate random Y and X for testing regressions."
  (bind ((x (make-array* (list n k) :double (generator x-rv)))
         (y (e+ (mm x beta) (make-array* n :double (generator e-rv)))))
    (values y x)))

;;; linear regression with known variance

;; (addtest (regressions-tests)
;;   dummy-observations-kv
;;   (bind ((mean (clo :double 2 3))
;;          (variance-right-sqrt (clo :double 
;;                                    5 7 :/
;;                                    11 13))
;;          (variance (mm t variance-right-sqrt))
;;          (prior (r-multivariate-normal mean 
;;                                :variance-right-sqrt variance-right-sqrt))
;;          ((:values y x) (dummy-observations prior))
;;          (lr (linear-regression-kv y x)))
;;     (ensure-same (mean lr) mean)))

;; (addtest (regressions-tests)
;;   dummy-2phase-kv
;;   ;; estimate in one and two steps, then compare
;;   (bind ((k 10)
;;          (n 100)
;;          ((:values y x) (random-y-x (* 2 n) k))
;;          (w (lla-vector (* 2 n) :double (generator* 'gamma))) ; weights
;;          ;; single step
;;          (p2 (linear-regression-kv y x :variance-right-sqrt (as-diagonal w)))
;;          ;; two steps, first half
;;          (h1 (cons 0 n))
;;          (p1 (linear-regression-kv (sub y h1) (sub x h1 t)
;;                                    :variance-right-sqrt 
;;                                    (as-diagonal (sub w h1))))
;;          ;; second half, using first half as prior
;;          (h2 (cons n 0))
;;          (p2-1 (linear-regression-kv (sub y h2) (sub x h2 t)
;;                                      :prior p1
;;                                      :variance-right-sqrt 
;;                                      (as-diagonal (sub w h2)))))
;;     (ensure-same (mean p2) (mean p2-1))
;;     (ensure-same (variance p2) (variance p2-1))))

;;; linear regression

;; (bind (((:values y x) (random-y-x 10 2))
;;        (lr (linear-regression y x))
;;        ((:values y-dummy x-dummy) (dummy-observations lr))
;;        (lr2 (linear-regression y-dummy x-dummy)))
;;   (d:v y-dummy x-dummy)
;;   (d:v (mean lr) :/ (mean lr2))
;;   (d:v (s^2 lr) :/ (s^2 lr2))
;;   )

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
                 1 3
                 1 4
                 1 5
                 1 6
                 1 7))
         (y (clo 2 2 3 4 5 6 6))
         (lr (linear-regression y x))
         ((:accessors-r/o mean variance r^2 s^2) lr)
         (ss (sum-of-squares (e- y (mm x mean)))))
    (ensure-same mean (solve (mm t x) (mm (transpose x) y)))
    (ensure-same s^2 (/ ss (reduce #'- (array-dimensions x))))
    (ensure-same variance (e* s^2
                              (invert (mm t x))))
    (ensure-same r^2 (- 1 (/ ss (sum-of-squared-errors y))))))

;; (addtest (regressions-tests)
;;   linear-regression-random
;;   (let* (;; sample
;;          (n 1000)
;;          (sigma 9d0)
;;          (beta (clo :double 1 2))
;;          (x (rs:replicate n (lambda ()
;;                               (vector (* 3 (draw-standard-normal))
;;                                       (+ 5 (draw-standard-normal))))
;;                           :flatten? t :element-type 'double-float))
;;          (y (e+ (mm x beta)
;;                 (make-array* n 'double-float (generator (normal 0 sigma)))))
;;          ;; regressions
;;          (lr (linear-regression y x))
;;          ;; posterior draws
;;          (m 1000000)
;;          (beta-draws (make-matrix m (length beta) :double))
;;          (sigma-draws (lla-vector m :double))
;;          (*==-tolerance* 1d-2))
;;     ;; ;; draw posterior
;;     (iter
;;       (for index :below m)
;;       (bind (((:values beta sigma) (draw lr)))
;;         (setf (sub beta-draws index t) beta
;;               (aref sigma-draws index) sigma)))
;;     ;; ;; check mean of sigma
;;     (ensure-same (* (s^2 lr) (mean (scaling-factor lr)))
;;                  (mean sigma-draws))
;;     (bind (((:values b-m b-v) (column-mean-variances beta-draws))
;;            (m-rd (ereldiff (mean lr) b-m))
;;            (v-rd (ereldiff (variance lr) b-v)))
;;       (format t "~2&mean: ~A mean reldiff: ~A~%" (mean lr) m-rd)
;;       (format t "~2&variance: ~A~%variance reldiff: ~A~%" (variance lr) m-rd)
;;       (ensure (< (emax m-rd) 1d-3))
;;       (ensure (< (emax v-rd) 1d-2)))
;; ))
