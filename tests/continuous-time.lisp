;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-random-tests)

(addtest (cl-random-tests)
  uniformized-markov-jump
  (let* ((n 100000)
         (keys #(:a :b :c))
         (rates #(1 2 3))
         (rv (uniformized-markov-jump rates :transition-rate 10 :keys keys :no-change :no-change))
         (possible-keys (concatenate 'vector (vector :no-change) keys))
         (durations (make-array n :element-type 'double-float))
         (jumps (make-array n))
         (*allowed-difference* 0.1d0)
         (*lift-equality-test* #'rel=))
    (dotimes (i n)
      (setf (values (aref durations i) (aref jumps i)) (draw rv)))
    (ensure-same (mean durations) 0.1)
    (ensure-same (count :a jumps) (* 0.1 n))
    (ensure-same (count :b jumps) (* 0.2 n))
    (ensure-same (count :c jumps) (* 0.3 n))
    (ensure-same (count :no-change jumps) (* 0.4 n))
    (ensure (every #'nonnegative? durations))
    (ensure (every (lambda (j) (find j possible-keys)) jumps))))
