;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-random-unit-tests)
(in-readtable lla:v-syntax)

;;;  !!! Currently, results from the code below are just "eyeballed",
;;;      they need to be incorporated into the unit testing framework
;;;      -- Tamas


;;; multivariate normal

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
(defparameter *y* (xmap '(numeric-vector :lla-type :double) #'+
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
(x-rel-diff *sample-variance* (variance *ls*))  ; should be near 1

;;; Wishart

(bind ((nu 4)
       (S (mm t #2v(1 2 3 4)))
       (rv (make-instance 'wishart :nu nu :scale S))
       (draws (xcollect 100000 (lambda () (as 'numeric-vector (draw rv)))))
       ((:values mean variance) (matrix-mean-variance draws)))
  (defparameter *rv* rv)
  (x-rel-diff (mean rv)
              (reshape mean 2 2)))
  ;; (values (x- (reshape mean 2 2) (mean rv))
  ;;     (xmap t #'abs (mean rv))))


;;; Inverse Wishart

(bind ((nu 5)
       (S (mm t #2v(1 2 9 4)))
       (rv (make-instance 'inverse-wishart :nu nu :inverse-scale S))
       (draws (xcollect 100000 (lambda () (as 'numeric-vector (draw rv)))))
       ((:values mean variance) (matrix-mean-variance draws)))
  (x-rel-diff (mean rv) (reshape mean 2 2)))
