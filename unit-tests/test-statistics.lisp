;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-random-unit-tests)

(deftestsuite statistics-tests (cl-random-unit-tests)
  ()
  (:equality-test #'lla=))

(addtest (statistics-tests)
  matrix-mean-variance
  (bind (((:values mean variance)
          (column-mean-variances (clo :double
                                      1 4 :/
                                      3 8))))
    (ensure-same mean (clo :double 2 6))
    (ensure-same variance (clo :double :hermitian
                               2 4 :/
                               * 8))))

(addtest (statistics-tests)
  rescale-by-sd
  (bind ((m (clo :double
                 1 4 :/
                 2 6
                 3 8))
         ((:values rescaled sd) (rescale-by-sd m)))
    (ensure-same rescaled (clo :double
                               -1 -1 :/
                               0 0 
                               1 1))
    (ensure-same sd (clo :double 1 2))))
