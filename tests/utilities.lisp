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

(defun same-sample-mean-variance (rv &key (n 1000000) (z-band 4d0)
                                   (var-band 0.1))
  "Quick and dirty test for the sample having the same mean and
variance as the theoretical distribution.  z-band is for comparing a
z-score, var-band is for rel= comparisons.  NOTE: this test doesn't
embody a lot of theory about sampling properties.  Bands are meant to
be quite wide, we are here to catch outrageous implementation
mistakes, and don't want to be stopped by false positives all the
time.  Some distributions have fat tails, they need a bigger
var-band."
  (bind (((:values mean variance) (sample-mean-variance n rv))
	 (z (* (- mean (mean rv)) (sqrt (/ n (variance rv)))))
	 (ok-p (and (< (abs z) z-band)
		    (let ((*allowed-difference* var-band))
		      (rel= variance (variance rv))))))
    (unless ok-p
      (format t "~&moment mismatch for distribution ~a~%" rv)
      (format t "theoretical/empirical mean: ~a / ~a, variance: ~a / ~a~%"
	      (mean rv) mean (variance rv) variance))
    ok-p))

(defun sample-mean-variance (n rv)
  (let ((sample (rs:replicate n rv)))
    (values (mean sample) (variance sample))))

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
