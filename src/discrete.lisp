;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-random)

;; TODO: Move bernoulli to univeriate, and implement as an rv. Idem for binomial, geometric, negative-binomial, poisson, ...

(declaim (inline draw-bernoulli))
(defun draw-bernoulli (p &key (rng *random-state*))
  "Return T with probability p, otherwise NIL. Rationals are handled exactly."
  (etypecase p
    (integer (ecase p
               (0 NIL)
               (1 T)))
    (rational (let+ (((&accessors-r/o numerator denominator) p))
                (assert (<= numerator denominator))
                (< (next denominator rng) numerator)))
    (float (< (next (float 1 p) rng) p))))

(defun draw-bernoulli-bit (p &key (rng *random-state*))
  (if (draw-bernoulli p :rng rng) 1 0))

;; TODO: Add shuffle! and elt (see :alexandria).

(defun distinct-random-integers (count limit &key (rng *random-state*))
  "Return a vector of COUNT distinct random integers, in increasing order,
drawn from the uniform discrete distribution on {0 , ..., limit-1}."
  (assert (<= count limit))
  (distinct-random-integers-dense count limit))

(defun distinct-random-integers-dense (count limit &key (rng *random-state*))
  "Implementation of DISTINCT-RANDOM-INTEGERS when count/limit is (relatively)
high. Implements algorithm S from @cite{taocp3}, p 142."
  (let ((result (make-array count)))
    (loop with selected = 0
          for index below limit
          do (when (draw-bernoulli (/ (- count selected)
                                      (- limit index)))
               (setf (aref result selected) index)
               (incf selected)
               (when (= selected count)
                 (return))))
    result))
