;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; -*-
(cl:defpackage #:cl-random.internals
  (:use #:cl))

(in-package #:cl-random.internals)

;;; internal representation of floats

(deftype internal-float ()
  "Type used for internal representation of floats in the CL-RANDOM library."
  'double-float)

(deftype float-vector (&optional n)
  `(simple-array internal-float (,n)))

(declaim (inline as-float))
(defun as-float (x)
  "Return the argument coerced to the CL-RANDOM library's internal float type."
  (coerce x 'internal-float))

(defmacro with-floats ((&rest variables) &body body)
  "Rebind each variable, coerced to a the internal float type used by CL-RANDOM."
  `(let ,(mapcar (lambda (variable)
                   `(,variable (as-float ,variable)))
          variables)
     ,@body))

(declaim (inline as-float-vector))
(defun as-float-vector (vector &key copy?)
  (if (or copy? (not (typep vector 'float-vector)))
      (map 'float-vector #'as-float vector)
      vector))

(defun as-float-probabilities (vector)
  "Normalize vector as probabilities, assert that all are positive, return them as a VECTOR-DOUBLE-FLOAT.  Vector is always copied."
  (let ((sum (as-float (clnu:sum vector))))
    (declare (type internal-float sum))
    (map 'float-vector
         (lambda (x)
           (declare (type double-float x))
           (assert (<= 0 x) (x) "Element is not positive.")
           (/ x sum))
         vector)))

(defconstant +pi+ (as-float pi)
  "The mathematical constant pi, with double precision.  Defined because CL:PI is LONG-FLOAT and that may not coincide with INTERNAL-FLOAT.")

(defconstant +normal-log-pdf-constant+ (as-float (* -1/2 (log (* 2 pi))))
  "Normalizing constant for the standard normal probability density.")

;;;; Macro for rejection methods.

(defmacro try ((&rest bindings) condition value)
  "Evaluate bindings (expanding into LET+, so all features can be used) until
condition is satisfied, then return value."
  (with-unique-names (top)
    `(prog ()
        ,top
        (let+ ,bindings
          (if ,condition
              (return ,value)
              (go ,top))))))
