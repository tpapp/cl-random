;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-random)

(defmacro gamma-sum% ()
  "Capturing the variable Z, return the sum of the series for
calculating the Gamma function using the Lanczos method."
  ;; coefficients are from http://lib.stat.cmu.edu/apstat/245
  ;; !!! compare with http://home.att.net/~numericana/answer/info/godfrey.htm
  (let ((coefficients #(0.9999999999995183d0 676.5203681218835d0 
                                     -1259.139216722289d0 771.3234287757674d0 
                                     -176.6150291498386d0 12.50734324009056d0 
                                     -0.1385710331296526d0 0.9934937113930748d-05 
                                     0.1659470187408462d-06)))
                 `(+ ,(aref coefficients 0) ,@(iter
                                                (for c :in-vector coefficients :from 1)
                                                (for a :from 0)
                                                (collecting `(/ ,c (+ z ,a)))))))

(declaim (inline gamma-reflection%))

(defun gamma-reflection% (z)
  "Gamma(z-1)Gamma(z) = (gamma-reflection% z)"
  (/ pi (sin (* pi z))))

(defun within-integer? (z &optional (tol 0))
  "Return non-NIL iff Z is within TOL of an integer (in either
direction).  TOL should be nonnegative (this is not checked)."
  (bind (((:values nil diff) (round z)))
    (<= (abs diff) tol)))

(defun log-gamma% (z)
  "Internal function implementing for calculating Log-gamma(z) for z > 0,
z double-float.  Not safe to be called directly, unless these
conditions can be ensured."
  (declare (double-float z)
           (optimize (speed 3) (safety 0)))
  (assert (plusp z))
  (let ((z% (+ z 6.5d0)))
    (+ (the double-float (log (gamma-sum%))) (log (sqrt (* 2 pi))) (- z%)
       (* (- z 0.5d0) (log z%)))))

(defun log-gamma (z)
  "Log gamma function (using the Lanczos method)."
  (let ((z (coerce z 'double-float)))
    (cond
      ((plusp z)
       (log-gamma% z))
      ((minusp z)
       (when (within-integer? z)
         (error 'division-by-zero))
       (- (log (gamma-reflection% z)) (log-gamma% (- 1d0 z))))
      (t
       (error 'division-by-zero)))))

(defun gamma% (z) 
  "Internal function implementing for calculating Gamma(z) for z > 0,
z double-float.  Not safe to be called directly, unless these
conditions can be ensured."
 (declare (double-float z)
           (optimize (speed 3) (safety 0)))
  (assert (plusp z))
  (let ((z% (+ z 6.5d0)))
    (* (gamma-sum%) (sqrt (* 2 pi)) (exp (- z%))
       (expt z% (- z 0.5d0)))))

(defun gamma (z)
  "Gamma function (using the Lanczos method)."
  (let ((z (coerce z 'double-float)))
    (cond
      ((plusp z)
       (gamma% z))
      ((minusp z)
       (when (within-integer? z)
         (error 'division-by-zero))
       (/ (gamma-reflection% z) (gamma% (- 1d0 z))))
      (t
       (error 'division-by-zero)))))
