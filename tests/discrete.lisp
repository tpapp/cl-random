;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-random-tests)

(defun count-draws-if (count generator &optional (predicate #'identity))
  "Call GENERATOR COUNT times, return the number of draws that satisfy
PREDICATE."
  (loop repeat count
        summing (if (funcall predicate (funcall generator)) 1 0)))

(defun same-proportion? (probability count generator
                         &key (predicate #'identity)
                              (tail 0.01))
  "Test if out of COUNT draws of GENERATOR, the proportion of those satisfying
PREDICATE is `close' to PROBABILITY.  Uses a normal approximation to the
binomial distribution, with the rejection probability 2*TAIL, symmetrically."
  (check-type probability (real 0 1))
  (assert (< 100 count))
  (let ((rv (r-normal (* count probability)
                      (max (* count probability (1c probability)) 1e-4)))
        (result (count-draws-if count generator predicate)))
    (values (< (quantile rv tail) result (quantile rv (1c tail)))
            (float (/ result count) 1d0))))

(addtest (cl-random-tests)
  bernoulli-tests
  (ensure (same-proportion? 0.3 10000
                            (lambda () (draw-bernoulli-rational 3 10))))
  (ensure (same-proportion? 1/31 100000
                            (lambda () (draw-bernoulli-rational 1 31))))
  (ensure (same-proportion? 0.99d0 10000
                            (lambda () (draw-bernoulli 0.99d0))))
  (ensure (same-proportion? 0.1 10000
                            (lambda () (draw-bernoulli 0.10)))))

(addtest (cl-random-tests)
  distinct-random-integers
  (flet ((test (number count limit &key (n-draws 10000))
           "Test (DISTRINCT-RANDOM-INTEGERS COUNT LIMIT) by counting the times
NUMBER shows up in draws."
           (assert (< -1 number limit))
           (same-proportion? (/ count limit)
                             n-draws
                             (lambda () (distinct-random-integers count limit))
                             :predicate (lambda (result) (find number result)))))
    (ensure (test 42 5 100))
    (ensure (test 93 6 100))
    (ensure (test 999 6 1000))
    (ensure (test 9 10 10))))
