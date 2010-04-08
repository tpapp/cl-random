;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; -*-

(in-package #:cl-random)

(defun vector-mean (vector)
  "Calculate the mean of the elements."
  (bind (((:slots-read-only lla-type elements)
          (if (typep vector 'numeric-vector-like)
              vector
              (as 'numeric-vector vector)))
         (length (length elements)))
    (/ (lla::sum-elements lla-type elements 0 length)
       length)))

(defun matrix-mean (matrix)
  "Calculate the mean of each column."
  (bind (((:slots-read-only lla-type elements nrow ncol)
          (if (typep matrix 'dense-matrix-like)
              (set-restricted matrix)
              (as 'dense-matrix matrix)))
         ((:lla-vector mean) (make-nv lla-type ncol)))
    (dotimes (col ncol)
      (setf (mean col) 
            (/ (lla::sum-elements lla-type elements (cm-index2 nrow 0 col)
                                     (cm-index2 nrow nrow col))
               nrow)))
    mean))

(defun demean-vector (vector &optional (mean (vector-mean vector)))
  "Subtract mean, return vector."
  (bind ((vector (copy-as 'numeric-vector vector))
         ((:slots-read-only lla-type elements) vector))
    (lla::subtract-from-elements lla-type elements 0 (length elements) mean)
    vector))

(defun demean-matrix (matrix &optional (mean (matrix-mean matrix)))
  "Subtract mean of columns (for multivariate observations stacked in
  the rows of a matrix.  Return demeaned-matrix."
  (check-type matrix dense-matrix-like)
  (bind ((matrix (copy-as 'dense-matrix matrix))
         ((:slots-read-only lla-type elements nrow ncol) matrix)
         ((:lla-vector mean) mean))
    ;; calculate & subtract mean
    (dotimes (col ncol)
      (lla::subtract-from-elements lla-type elements (cm-index2 nrow 0 col)
                                   (cm-index2 nrow nrow col) (mean col)))
    matrix))

(defun matrix-mean-variance (matrix)
  "For multivariate observations stacked in the rows of a matrix,
  return sample mean and (co)variance matrix as (values mean var).
  Their types are numeric-vector and hermitian-matrix, respectively."
  ;; values
  (bind ((mean (matrix-mean matrix))
         (matrix (demean-matrix matrix mean)))
    (values mean
            (mm t matrix (/ (nrow matrix))))))

(defun vector-sum-of-squares (vector)
  "Sum of the squares of elements."
  (bind (((:slots-read-only lla-type elements) vector))
    (lla::sum-squared-elements lla-type elements 0 (length elements))))

(defun empirical-quantiles (vector quantiles)
  "Empirical quantiles of VECTOR (copied with COPY-AS).  QUANTILES has
to be a sequence, and the result is of the same type.  Elements are
interpolated linearly."
  (let* ((vector (sort (copy-as 'vector vector) #'<=))
         (n (length vector)))
    (map (type-of quantiles)
         (lambda (q)
           (assert (<= 0 q 1) () "Quantile ~A is not in [0,1]." q)
           (bind ((r (* q (1- n)))
                  ((:values int frac) (floor r))
                  (left (aref vector int)))
             (if (zerop frac)
                 left
                 (convex-combination% left (aref vector (1+ int)) frac))))
         quantiles)))
