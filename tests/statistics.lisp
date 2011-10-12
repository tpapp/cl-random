;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-random-tests)

(deftestsuite statistics-tests (cl-random-tests)
  ()
  (:equality-test #'==))

(addtest (statistics-tests)
  test-matrix-mean
  (let ((m (dense 'lla-double
             (1 2 3 4)
             (2 4 6 8)
             (3 6 9 12))))
    (ensure-same (matrix-mean m)
                 (vec 'lla-double 2 4 6 8))))

(addtest (statistics-tests)
  test-matrix-sse
  (let* ((*lift-equality-test* #'==)
         (m (dense 'lla-double
              (1 2)
              (-1 -2)))
         (m2 (dense 'lla-double
               (1 8)
               (7 19)
               (9 44)
               (26 5)))
         ;; calculating another way
         (mean (matrix-mean m2))
         (demeaned (e- m2 (recycle mean :h)))
         (sse (mm t demeaned)))
    (ensure-same (as-matrix (matrix-sse m))
                 (hermitian 'lla-double
                   (2 %)
                   (4 8)))
    (ensure-same (as-matrix (matrix-sse m2)) sse)))

(addtest (statistics-tests)
  matrix-mean-variance
  (let+ (((&values mean variance)
          (matrix-mean-and-variance (dense 'lla-double
                                      (1 4)
                                      (3 8)))))
    (ensure-same mean (vec 'lla-double 2 6))
    (ensure-same (as-matrix variance)
                 (hermitian 'lla-double
                   (2 %)
                   (4 8)))))

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
