;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-random-tests)

(defparameter *allowed-difference* 1d-5
  ;; default is for catching major mistakes, use smaller value for fine points
  "Maximum allowed difference used by approx=.")

(defun approx= (a b)
  "Approximately equal, by *allowed-difference*."
  (< (abs (- a b)) *allowed-difference*))

(defun relative-difference (a b)
  "Difference between a and b, compared to the smaller one in absolute value."
  (if (and (zerop a) (zerop b))
      0
      (/ (- a b) (max (abs a) (abs b)))))

(defun rel= (a b)
  "Relatively equal, by *allowed-difference*."
  (approx= (relative-difference a b) 0))

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
		    (let ((*allowed-difference* var-band))
		      (rel= variance (variance rv))))))
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

(defun reldiff (x y)
  "Relative difference. (X should be the \"true\" value if the concept
is meaningful).  The returnes value is always nonnegative."
  (let ((diff (- x y)))
    (if (zerop x)
        (if (zerop y)
            0
            most-positive-double-float)
        (abs (/ diff x)))))

(defun ereldiff (a b)
  "Relative difference between two objects, elementwise."
  (emap #'reldiff a b))
