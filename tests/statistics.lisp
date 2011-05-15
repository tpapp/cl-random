;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-random-tests)

(deftestsuite statistics-tests (cl-random-tests)
  ()
  (:equality-test #'==))

(addtest (statistics-tests)
  test-matrix-mean
  (let ((m (array* '(3 4) 'double-float
                   1 2 3 4
                   2 4 6 8
                   3 6 9 12)))
    (ensure-same (matrix-mean m)
                 (vector* 'double-float 2 4 6 8))))

(addtest (statistics-tests)
  test-matrix-sse
  (let* ((*lift-equality-test* #'==)
         (m (array* '(2 2) t
                    1 2
                    -1 -2))
         (m2 (clo :double
                  1 8 :/
                  7 19
                  9 44
                  26 5))
         ;; calculating another way
         (mean (matrix-mean m2))
         (demeaned (e- m2 (recycle mean :h)))
         (sse (mm t demeaned)))
    (ensure-same (matrix-sse m)
                 (clo :hermitian :double
                      2 % :/
                      4 8))
    (ensure-same (matrix-sse m2)
                 sse)))

(addtest (statistics-tests)
  matrix-mean-variance
  (bind (((:values mean variance)
          (matrix-mean-and-variance (clo :double
                                         1 4 :/
                                         3 8))))
    (ensure-same mean (clo :double 2 6))
    (ensure-same variance (clo :double :hermitian
                               2 % :/
                               4 8))))

;; (addtest (statistics-tests)
;;   rescale-by-sd
;;   (bind ((m (clo :double
;;                  1 4 :/
;;                  2 6
;;                  3 8))
;;          ((:values rescaled sd) (rescale-by-sd m)))
;;     (ensure-same rescaled (clo :double
;;                                -1 -1 :/
;;                                0 0 
;;                                1 1))
;;     (ensure-same sd (clo :double 1 2))))
