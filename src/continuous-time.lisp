;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-random)

(define-rv uniformized-markov-jump (rates &key transition-rate keys no-change)
  "Define a random variable for uniformized markov jumps, which returns two values:
the time spent in the state, and the index of the next state (or the corresponding
element in KEYS, which is a vector of the same length).  TRANSITION-RATES defaults to
the sum of keys, however, it can be specified to be larger, in which case the markov
chain may remain in the same state and return NO-CHANGE as the second value.  Defines
features DURATION and JUMP, which return the two underlying random variables."
  (bind ((rates (as-double-float-vector rates))
         (keys (when keys (coerce keys 'vector)))
         (n (length rates))
         (total-rate (sum rates))
         ((:values transition-rate probabilities) 
          (if transition-rate
              (let ((no-change-rate (- transition-rate total-rate)))
                (assert (<= 0 no-change-rate) ()
                        "Transition rate has to be >= the sum of rates.")
                (values
                  transition-rate
                  (concatenate 'double-float-vector rates (vector no-change-rate))))
              (values
                total-rate
                rates)))
         (duration (exponential transition-rate))
         (jump (discrete probabilities)))
    (assert (every #'plusp rates) () "Rates need to be positive.")
    (features ((duration duration)
               (jump jump))
      (flambda
        (values (draw duration)
                (let ((j (draw jump)))
                  (cond 
                    ((= j n) no-change)
                    (keys (aref keys j))
                    (t j))))))))
