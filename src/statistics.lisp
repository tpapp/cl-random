;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; -*-

(in-package #:cl-random)

(defun matrix-mean (matrix)
  "Calculate the mean of each column."
  (bind (((:accessors-r/o leading-dimension nrow ncol elements)
          (if (typep matrix 'dense-matrix-like)
              (set-restricted matrix)
              (as 'dense-matrix matrix)))
         ((:lla-vector mean) (make-nv (lla-type matrix) ncol)))
    (dotimes (col ncol)
      (let* ((start (cm-index2 leading-dimension 0 col))
             (end (cm-index2 leading-dimension 0 (1+ col))))
        (setf (mean col) (/ (reduce #'+ elements
                                    :start start :end end)
                            nrow))))
    mean))

(defun demean-matrix (matrix &optional (mean (matrix-mean matrix)))
  "Remove mean of columns (for multivariate observations stacked in
  the rows of a matrix.  Return (values demeaned-matrix mean)."
  (check-type matrix dense-matrix-like)
  (bind ((matrix (copy-as 'dense-matrix matrix))
         ((:slots-read-only nrow ncol elements) matrix)
         ((:lla-vector mean) mean))
    ;; calculate & subtract mean
    (dotimes (col ncol)
      (let ((col-mean (mean col)))
        (iter
          (for index :from (cm-index2 nrow 0 col)
               :below (cm-index2 nrow 0 (1+ col)))
          (decf (aref elements index) col-mean))))
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
