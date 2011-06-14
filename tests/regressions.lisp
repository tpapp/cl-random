;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-random-tests)

(deftestsuite regressions-tests (cl-random-tests)
  ()
  (:equality-test #'==))

(addtest (regressions-tests)
  dummy-regenerate
  (let+ ((k 2)
         (n 10)
         ((&values y x) (random-y-x n k))
         (r1 (linear-regression y x))
         (d1 (linear-regression-dummies r1))
         (r2 (linear-regression (car d1) (cdr d1))))
    (ensure-same (mean r1) (mean r2))
    (ensure-same (variance r1) (variance r2))))

(addtest (regressions-tests)
  dummy-2phase
  (let+ ((k 2)
         (n 10)
         ((&values y x) (random-y-x (* 2 n) k))
         ;; single step
         (p2 (linear-regression y x))
         ;; two steps, first half
         (h1 (cons 0 n))
         (p1 (linear-regression (sub y h1) (sub x h1 t)))
         ;; second half, using first half as prior
         (h2 (cons n nil))
         (p2-1 (linear-regression (sub y h2) (sub x h2 t) :prior p1)))
    (ensure-same (mean p2) (mean p2-1))
    (ensure-same (variance p2) (variance p2-1))
    (ensure-same (s^2 p2) (s^2 p2-1))
    (ensure-same (nu p2) (nu p2-1))))

(addtest (regressions-tests)
  linear-regression-small
  (let+ ((x (clo 1 1 :/
                 1 2
                 1 3
                 1 4
                 1 5
                 1 6
                 1 7))
         (y (clo 2 2 3 4 5 6 6))
         ((&values lr r^2) (linear-regression y x))
         ((&accessors-r/o mean variance s^2 nu) lr)
         (ss (sse (e- y (mm x mean)) 0)))
    (ensure-same mean (solve (mm t x) (mm (transpose x) y)))
    (ensure-same s^2 (/ ss (reduce #'- (array-dimensions x))))
    (ensure-same variance (e* s^2 (invert (mm t x)) (/ nu (- nu 2d0))))
    (ensure-same r^2 (- 1 (/ ss (sse y))))))

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
