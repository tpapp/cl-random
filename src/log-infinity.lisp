;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-random)

;;; Arithmetic operations starting with ~ follow the convention that
;;; NIL represents -Infinity.  They are intended for operating on log
;;; probability densities..  ~LOG and ~EXP can be used to transform
;;; between logs and actual values.  Only real numbers are supported.

(define-condition plus-infinity (error)
  ()
  (:documentation "Can't represent -NIL (= plus infinity)."))

(defun ~- (a &rest args)
  "Subtraction, treating NIL as -Infinity."
  (if args
      (let ((args-sum 
             (iter
               (for arg :in args)
               (unless arg
                 (error 'plus-infinity))
               (summing arg))))
        (if a
            (- a args-sum)
            nil))
      (if a
          (- a)
          (error 'plus-infinity))))

(defun ~+ (&rest args)
  "Addition, treating NIL as -Infinity."
  (iter
    (for arg :in args)
    (unless arg
      (return-from ~+ nil))
    (summing arg)))

(defun ~log (number)
  "Natural logarithm.  (~LOG 0) returns NIL, which is used to
represent -Infinity.  Only works on nonnegative real numbers."
  (check-type number (real 0) "a nonnegative real number")
  (if (zerop number)
      nil
      (log number)))

(defun ~exp (number)
  "Exponentiation, with (~EXP NIL) returning 0.  Only works on real
numbers or NIL."
  (if number
      (progn
        (check-type number real)
        (exp number))
      0))
