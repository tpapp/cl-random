;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-random-tests)


(deftestsuite special-functions-tests (cl-random-tests)
  ())

;;; utility functions

(defun gamma-int (i)
  "Exactly calculated Gamma function for positive integers."
  (check-type i (integer 1))
  (iter
    (for j :from 2 :below i)
    (multiply j)))

(defun gamma-relative-precision (i)
  (let ((g (gamma-int i)))
    (reldiff g (gamma-function i))))

(addtest (special-functions-tests)
  gamma-integer
  ;; comparison with integers
  (ensure (< (gamma-relative-precision 3) 1d-13))
  (ensure (< (gamma-relative-precision 7) 1d-13))
  (ensure (< (gamma-relative-precision 11) 1d-13))
  (ensure (< (gamma-relative-precision 8) 1d-13))
  (ensure (< (gamma-relative-precision 112) 1d-12)))

(addtest (special-functions-tests)
  gamma-fraction
  ;; values calculated by Maxima
  (ensure (< (reldiff (gamma-function 0.5) 1.772453850905516d0) 1d-13))
  (ensure (< (reldiff (gamma-function 1.01) 0.99432585119151d0) 1d-8))
  (ensure (< (reldiff (gamma-function 0.999999) 1.000000577216654) 1d-7)))

(addtest (special-functions-tests)
  gamma-negative
  ;; values calculated by Maxima
  (ensure (< (reldiff (gamma-function -7.1) 0.0016478244570263d0) 1d-5))
  (ensure (< (reldiff (log-gamma-function -5.5) -4.517832174007742) 1d-7))
  ;; not defined, should signal error
  (ensure-error (gamma-function -7))
  (ensure-error (gamma-function 0))
  (ensure-error (log-gamma-function 0)))
