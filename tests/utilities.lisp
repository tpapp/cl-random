;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-random-tests)

(defun z-score (n mean variance sample-mean)
  "Calculate abs(z) based on theoretical mean and variance and sample mean."
  (abs (* (- sample-mean mean)
          (sqrt (/ n variance)))))

(defun same-sample-mean-variance (rv &key (n 100000) (z-band 4d0)
                                          (var-band 0.1))
  "Generate a sample of length N from RV, then use
COMPARE-SAMPLE-MEAN-VARIANCE.  For univariate and multivariate distributions."
  (let+ (((&accessors-r/o mean variance) rv)
         (multivariate? (vectorp mean))
         (sample (sweep (if multivariate?
                            (conforming-accumulator 'mean )
                            'variance)
                        (replicating rv n))))
    (if multivariate?
        (every (curry #'> z-band)
               (map 'vector (curry #'z-score n) mean (diagonal variance)
                    (mean sample)))
        (and (< (z-score n mean variance (mean sample)) z-band)
             (== variance (variance sample) var-band)))))

(defgeneric relative-difference (a b)
  (:documentation "Relative difference of A and B.")
  (:method ((a number) (b number))
    (/ (abs (- a b))
       (max 1 (abs a) (abs b))))
  (:method ((a array) (b array))
    "Relative difference of A and B."
    (assert (equalp (array-dimensions a) (array-dimensions b)))
    (with-accumulator (r &maximize)
      (map nil (lambda (a b)
                 (r (relative-difference a b)))
           (flatten-array a) (flatten-array b))))
  (:method ((a array) b)
    (relative-difference a (as-array b))))

(defun random-y-x (n k
                   &optional
                     (x-rv (r-normal 0 9))
                     (e-rv (r-normal 0 2))
                     (beta (generate-array k (generator (r-normal 0 1))
                            'lla-double)))
  "Generate random Y and X for testing regressions."
  (let* ((x (generate-array (list n k) (generator x-rv) 'lla-double))
         (y (e+ (mm x beta) (generate-array n (generator e-rv) 'lla-double))))
    (values y x)))
