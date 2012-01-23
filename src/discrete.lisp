;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-random)

(declaim (notinline draw-bernoulli-rational))
(defun draw-bernoulli-rational (numerator denominator)
  "Draw T with probability a/b, otherwise NIL."
  (check-type denominator (integer 1))
  (check-type numerator (integer 0))
  (assert (<= numerator denominator))
  (< (random denominator) numerator))

(declaim (inline draw-bernoulli))
(defun draw-bernoulli (p)
  "Draw T with probability p, otherwise NIL."
  (etypecase p
    (integer (ecase p
               (0 0)
               (1 1)))
    (rational (draw-bernoulli-rational (numerator p) (denominator p)))
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
          do (when (draw-bernoulli-rational (- count selected)
                                            (- limit index))
               (setf (aref result selected) index)
               (incf selected)
               (when (= selected limit)
                 (return))))
    result))
