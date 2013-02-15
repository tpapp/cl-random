;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-random)

(declaim (inline draw-bernoulli))
(defun draw-bernoulli (p)
  "Return T with probability p, otherwise NIL.  Rationals are handled exactly."
  (etypecase p
    (integer (ecase p
               (0 0)
               (1 1)))
    (rational (let+ (((&accessors-r/o numerator denominator) p))
                (assert (<= numerator denominator))
                (< (random denominator) numerator)))
    (float (< (random (float 1 p)) p))))

(defun distinct-random-integers (count limit)
  "Return a vector of COUNT distinct random integers, in increasing order,
drawn from the uniform discrete distribution on {0 , ..., limit-1}."
  (assert (<= count limit))
  (distinct-random-integers-dense count limit))

(defun distinct-random-integers-dense (count limit)
  "Implementation of DISTINCT-RANDOM-INTEGERS when count/limit is (relatively)
high. Implements algorithm S from @cite{taocp3}, p 142."
  (let ((result (make-array count)))
    (loop with selected = 0
          for index below limit
          do (when (draw-bernoulli (/ (- count selected)
                                      (- limit index)))
               (setf (aref result selected) index)
               (incf selected)
               (when (= selected limit)
                 (return))))
    result))
