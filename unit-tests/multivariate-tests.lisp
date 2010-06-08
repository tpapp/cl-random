;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-random-unit-tests)

;;;  !!! Currently, results from the code below are just "eyeballed",
;;;      they need to be incorporated into the unit testing framework
;;;      -- Tamas


(deftestsuite multivariate-tests (cl-random-unit-tests)
  ())

;;; multivariate normal

(addtest (multivariate-tests)
  mv-normal-pdf
  (let* ((a (clo 1 2 :/
                 3 4))
         (v (mm t a))
         (rv (make-instance 'mv-normal :mean (clo 1 1) :variance v))
         (*lift-equality-test* (lambda (a b)
                                 (<= (abs (- a b)) 1d-6))))
    (ensure-same (cl-random::log-pdf-constant rv)
                 (- (* (logdet v) -0.5)
                    (log (* 2 pi))))
    (ensure-same (pdf rv (clo 1 2))
                 0.02279933)
    (ensure-same (pdf rv (clo 3 4))
                 0.061975)))

(time
 (bind ((mean (clo :double 3 4))
        (variance (clo :hermitian :double
                       3 0.5 :/
                       0.5 2))
        (rv (make-instance 'mv-normal :mean mean :variance variance))
        (matrix (collect-rows 100000 (lambda () (draw rv)) 'dense-matrix))
        ((:values sample-mean sample-variance)
         (column-mean-variances matrix)))
   (values sample-mean
           sample-variance)))

;;; multivariate t

(addtest (multivariate-tests)
  mv-t-pdf
  (let* ((sigma (mm t (clo 1 2 :/
                           3 4)))
         (rv (make-instance 'mv-t :sigma sigma :nu 8))
         (*lift-equality-test* (lambda (a b)
                                 (<= (abs (- a b)) 1d-6))))
    (defparameter *t* rv)
    (ensure-same (log-pdf rv (clo 0 0))
                 -2.531024)
    (ensure-same (log-pdf rv (clo 1 2))
                 -3.119939)
    (ensure-same (pdf rv (clo 3 4))
                 0.04415984)))

(time
 (bind ((mean (clo 3 4))
        (sigma (clo :hermitian
                       1 0.5 :/
                       0.5 1))
        (rv (make-instance 'mv-t :mean mean :sigma sigma :nu 10))
        (matrix (collect-rows 100000 (lambda () (draw rv)) 'dense-matrix))
        ((:values sample-mean sample-variance)
         (column-mean-variances matrix)))
   (values sample-mean
           sample-variance)))

;;; linear regression


(addtest (multivariate-tests)
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

;;; Wishart

(bind ((nu 400)
       (S (mm t (clo :dense 1 2 :/ 3 4)))
       (rv (make-instance 'wishart :nu nu :scale S))
       (draws (xcollect 10000 (lambda () (as 'numeric-vector (draw rv)))))
       ((:values mean variance) (matrix-mean-variance draws)))
  (defparameter *rv* rv)
  (x-rel-diff (mean rv)
              (reshape mean 2 2)))
  ;; (values (x- (reshape mean 2 2) (mean rv))
  ;;     (xmap t #'abs (mean rv))))

;;; Inverse Wishart

(bind ((nu 500)
       (S (mm t (x* 1000 (clo :dense 1 2 :/ 9 4))))
       (rv (make-instance 'inverse-wishart :nu nu :inverse-scale S))
       (draws (xcollect 10000 (lambda () (as 'numeric-vector (draw rv)))))
       ((:values mean variance) (matrix-mean-variance draws)))
  (x-rel-diff (mean rv) (reshape mean 2 2)))
