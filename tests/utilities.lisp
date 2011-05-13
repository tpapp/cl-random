;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-random-tests)

(defun compare-sample-mean-variance (rv sample &key (z-band 4d0) (var-band 0.1))
  "Quick and dirty test for the sample having the same mean and
variance as the theoretical distribution.  z-band is for comparing a
z-score, var-band is for rel= comparisons.  NOTE: this test doesn't
embody a lot of theory about sampling properties.  Bands are meant to
be quite wide, we are here to catch outrageous implementation
mistakes, and don't want to be stopped by false positives all the
time.  Some distributions have fat tails, they need a bigger
var-band."
  (bind ((n (length sample))
         ((:values mean variance) (mean-and-variance sample))
	 (z (* (- mean (mean rv)) (sqrt (/ n (variance rv)))))
	 (ok-p (and (< (abs z) z-band)
                    (== variance (variance rv) var-band))))
    (unless ok-p
      (format t "~&moment mismatch for distribution ~a~%" rv)
      (format t "theoretical/empirical mean: ~a / ~a, variance: ~a / ~a~%"
	      (mean rv) mean (variance rv) variance))
    ok-p))

(defun same-sample-mean-variance (rv &key (n 100000) (z-band 4d0) (var-band 0.1))
  "Generate a sample of length N from RV, then use COMPARE-SAMPLE-MEAN-VARIANCE.  For
univariate and multivariate distributions."
  (let* ((sample (rs:replicate n (generator rv) :flatten? t)))
    (ecase (array-rank sample)
      (1 (compare-sample-mean-variance rv sample
                                       :z-band z-band :var-band var-band))
      (2 (iter
           (for column :below (array-dimension sample 1))
           (always (compare-sample-mean-variance
                    (sub rv column) (sub sample t column)
                    :z-band z-band :var-band var-band)))))))

(defun number-relative-difference (a b)
  "Relative difference of A and B."
  (/ (abs (- a b))
     (max 1 (abs a) (abs b))))

(defgeneric relative-difference (a b)
  (:method ((a number) (b number))
    (number-relative-difference a b))
  (:method (a b)
    (emax (emap t #'number-relative-difference (as-array a) (as-array b)))))

(defun random-y-x (n k &optional 
                   (x-rv (r-normal 0 9))
                   (e-rv (r-normal 0 2))
                   (beta (make-array* k :double
                                      (generator (r-normal 0 1)))))
  "Generate random Y and X for testing regressions."
  (bind ((x (make-array* (list n k) :double (generator x-rv)))
         (y (e+ (mm x beta) (make-array* n :double (generator e-rv)))))
    (values y x)))
