;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-random-tests)

(deftestsuite multivariate-tests (cl-random-tests)
  ())

;;; multivariate normal

(addtest (multivariate-tests)
  multivariate-normal-pdf
  (let* ((a (dense 'lla-double
              (1 2)
              (3 4)))
         (v (mm t a))
         (rv (r-multivariate-normal (vec 'lla-double 1 1) v))
         (*lift-equality-test* (lambda (a b)
                                 (<= (abs (- a b)) 1d-6))))
    ;; (ensure-same (cl-random::log-pdf-constant rv)
    ;;              (- (* (logdet v) -0.5)
    ;;                 (log (* 2 pi))))
    (ensure-same (pdf rv (vec 'lla-double 1 2))
                 0.02279933)
    (ensure-same (pdf rv (vec 'lla-double 3 4))
                 0.061975)))

(addtest (multivariate-tests)
  multivariate-normal-draws
  (let ((rv (r-multivariate-normal (vec 'lla-double 1 2)
                                   (mm t (dense 'lla-double
                                           (1 2)
                                           (3 4))))))
    (same-sample-mean-variance rv :n 100000)))

;; (time
;;  (bind ((mean (clo 'lla-double 3 4))
;;         (variance (clo :hermitian 'lla-double
;;                        3 0.5 :/
;;                        0.5 2))
;;         (rv (make-instance 'mv-normal :mean mean :variance variance))
;;         (matrix (collect-rows 100000 (lambda () (draw rv)) 'dense-matrix))
;;         ((:values sample-mean sample-variance)
;;          (column-mean-variances matrix)))
;;    (values sample-mean
;;            sample-variance)))

;;; multivariate t

(addtest (multivariate-tests)
  multivariate-t-pdf
  (let* ((sigma (mm t (dense 'lla-double
                        (1 2)
                        (3 4))))
         (rv (r-multivariate-t (vec 'lla-double 0 0) sigma 8))
         (*lift-equality-test* (lambda (a b)
                                 (<= (abs (- a b)) 1d-6))))
    (ensure-same (log-pdf rv (vec 'lla-double 0 0))
                 -2.531024)
    (ensure-same (log-pdf rv (vec 'lla-double 1 2))
                 -3.119939)
    (ensure-same (pdf rv (vec 'lla-double 3 4))
                 0.04415984)))

(addtest (multivariate-tests)
  multivariate-normal-draws
  (let ((rv (r-multivariate-t (vec 'lla-double 1 2)
                              (mm t (dense 'lla-double
                                      (1 2)
                                      (3 4)))
                              4d0)))
    (same-sample-mean-variance rv :n 100000)))


;; (time
;;  (bind ((mean (clo 3 4))
;;         (sigma (clo :hermitian
;;                        1 0.5 :/
;;                        0.5 1))
;;         (rv (make-instance 'mv-t :mean mean :sigma sigma :nu 10))
;;         (matrix (collect-rows 100000 (lambda () (draw rv)) 'dense-matrix))
;;         ((:values sample-mean sample-variance)
;;          (column-mean-variances matrix)))
;;    (values sample-mean
;;            sample-variance)))

;; ;;;  !!! Currently, results from the code below are just "eyeballed",
;; ;;;      they need to be incorporated into the unit testing framework
;; ;;;      -- Tamas



;; ;;; Wishart

(addtest (multivariate-tests)
  wishart-draws
  (let* ((rv (r-wishart 7 (mm (dense 'lla-double
                                (1 2)
                                (3 4))
                              t)))
         (sample (rs:replicate 50000 (compose #'as-array (generator rv))))
         (sample-mean (mean sample)))
    (ensure (< (relative-difference sample-mean (mean rv)) 0.01))))

(addtest (multivariate-tests)
  inverse-wishart-draws
  (let* ((rv (r-inverse-wishart 7 (mm (dense 'lla-double
                                        (1 2)
                                        (3 4))
                                      t)))
         (sample (rs:replicate 50000 (compose #'as-array (generator rv))))
         (sample-mean (mean sample)))
    (ensure (< (relative-difference sample-mean (mean rv)) 0.01))
    (values sample-mean (mean rv))))

;; (mean (vector (draw  (r-inverse-wishart 7 (mm (dense 'lla-double
;;                                                 (1 2)
;;                                                 (3 4))
;;                                               t)))))



;; (bind ((nu 400)
;;        (S (mm t (clo :dense 1 2 :/ 3 4)))
;;        (rv (make-instance 'wishart :nu nu :scale S))
;;        (draws (xcollect 10000 (lambda () (as 'numeric-vector (draw rv)))))
;;        ((:values mean variance) (matrix-mean-variance draws)))
;;   (defparameter *rv* rv)
;;   (x-rel-diff (mean rv)
;;               (reshape mean 2 2)))
;;   ;; (values (x- (reshape mean 2 2) (mean rv))
;;   ;;     (xmap t #'abs (mean rv))))

;; ;;; Inverse Wishart

;; (bind ((nu 500)
;;        (S (mm t (x* 1000 (clo :dense 1 2 :/ 9 4))))
;;        (rv (make-instance 'inverse-wishart :nu nu :inverse-scale S))
;;        (draws (xcollect 10000 (lambda () (as 'numeric-vector (draw rv)))))
;;        ((:values mean variance) (matrix-mean-variance draws)))
;;   (x-rel-diff (mean rv) (reshape mean 2 2)))
