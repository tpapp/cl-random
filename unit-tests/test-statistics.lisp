;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-random-unit-tests)

(deftestsuite statistics-tests (cl-random-unit-tests)
  ())

(addtest (statistics-tests)
  matrix-mean-variance
  (bind (((:values mean variance)
          (matrix-mean-variance #2v(1 4
                                      3 8)))
         (*lift-equality-test* #'x=))
    (ensure-same mean #v(2 6))
    (ensure-same variance #2v:hermitian(1 2 0 4))))

