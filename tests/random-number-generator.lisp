;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-random-tests)

(defun valid-rng (rng &key (n 100000) (limit 100) (z-band 4d0) (var-band 0.1))
  "Get N random numbers in [0,LIMIT) from RNG. Verify their type, range, and moments, using

  1. the z-score of the mean (should be below Z-BAND in absolute value),

  2. the variance with NUM= using VAR-BAND as a tolerance."
  (let ((mean (/ (if (integerp limit) (1- limit) limit) 2))
	(variance (/ (if (integerp limit) (1- (* limit limit)) (* limit limit)) 12))
	(sample-moments (clnu.stats:central-sample-moments nil)))
    (loop repeat n for r = (next rng limit)
	  unless (and (<= 0 r) (< r limit)) return NIL
	  unless (typep r (type-of limit)) return NIL
          do (clnu.stats:add sample-moments r))
    (and (< (z-score n mean variance (clnu.stats:mean sample-moments)) z-band)
         (num= variance (clnu.stats:variance sample-moments) var-band))))

(defsuite random-number-generator-tests (tests))

(deftest cl-rng-tests (random-number-generator-tests)
  (assert-true (valid-rng (make-instance 'cl-rng) :limit 100))
  (assert-true (valid-rng (make-instance 'cl-rng) :limit 100.0))
  (assert-true (valid-rng (make-instance 'gsll-rng) :limit 100))
  (assert-true (valid-rng (make-instance 'gsll-rng) :limit 100.0)))
