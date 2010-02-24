;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-random-unit-tests)

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
(defparameter *y* (xmap 'numeric-vector-double #'+
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
(/ *sample-variance* (variance *ls*))  ; should be near 1
