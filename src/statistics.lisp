;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; -*-

(in-package #:cl-random)

(defgeneric add-constant-column (object &optional constant)
  (:documentation "Prepend a column of CONSTANTs (defaults to 1) before object."))

(defmethod add-constant-column ((matrix dense-matrix-like) &optional (constant 1))
  "Add a constant column before a vector or a matrix,
returning a dense matrix."
  (bind (((:slots-r/o elements nrow ncol) matrix))
    (aprog1 (make-matrix nrow (1+ ncol) (array-lla-type elements)
                         :initial-element constant)
      (set-restricted matrix)
      (copy-elements elements 0
                     (elements it) nrow
                     (* nrow ncol)))))

(defmethod add-constant-column ((vector vector) &optional (constant 1))
  (add-constant-column (as-column vector) constant))

(defun column-sums (matrix)
  "Calculate the sum of each column, returned as a numeric-vector."
  (bind (((:slots-r/o  elements nrow ncol)
          (if (typep matrix 'dense-matrix-like)
              (set-restricted matrix)
              (as-matrix matrix)))
         ((:lla-vector sum) (make-similar-vector elements ncol)))
    (dotimes (col ncol)
      (setf (sum col) 
            (lla::sum-elements% elements (cm-index2 nrow 0 col)
                                (cm-index2 nrow nrow col))))
    sum))

(defun column-means (matrix)
  "Calculate the mean of each column, returned as a numeric-vector."
  (e/ (column-sums matrix) (nrow matrix)))

(defun subtract-from-elements% (elements start end number)
  (declare (optimize speed (safety 0))
           (fixnum start end))
  (lla::with-vector-type-expansion (elements)
    (lambda (lla-type)
      `(let ((number (coerce* number ,lla-type)))
         (declare (type ,(lla::lla->lisp-type lla-type) number))
         (iter
           (for (the fixnum i) :from start :below end)
           (declare (iterate:declare-variables))
           (decf (aref elements i) number)))))
  (values))

(defun demean-vector (vector &optional (mean (mean vector)))
  "Subtract mean, return vector."
  (aprog1 (copy-vector vector)
    (subtract-from-elements% it 0 (length it) mean)))

(defun demean-columns (matrix &optional (means (column-means matrix)))
  "Subtract mean of columns (for multivariate observations stacked in
  the rows of a matrix.  Return demeaned-matrix."
  (check-type matrix dense-matrix-like)
  (aprog1 (copy-matrix matrix :copy? t :kind :dense)
    ;; subtract mean
    (bind (((:slots-r/o elements nrow ncol) it))
      (dotimes (col ncol)
        (subtract-from-elements% elements (cm-index2 nrow 0 col)
                                 (cm-index2 nrow nrow col) (aref means col))))
    matrix))

(defun column-variances (matrix &optional (means (column-means matrix)))
  "Variance matrix of columns.  If MEANS is nil, it is not subtracted."
  (when means
    (setf matrix (demean-columns matrix means)))
  (mm t matrix (/ (1- (nrow matrix)))))

(defun column-mean-variances (matrix)
  "For multivariate observations stacked in the rows of a matrix,
  return sample mean and (co)variance matrix as (values mean var).
  Their types are numeric-vector and hermitian-matrix, respectively."
  ;; values
  (bind ((means (column-means matrix))
         (matrix (demean-columns matrix means)))
    (values means
            (column-variances matrix nil))))

(defun rescale-by-sd (matrix)
  "Rescale matrix by standard deviations, which are returned as a second value."
  (let* ((matrix (demean-columns matrix))
         (variances (column-variances matrix nil))
         (sd (esqrt (as-diagonal variances))))
    (values (mm matrix (e/ sd))
            (elements sd))))

(defun empirical-quantiles (vector quantiles &key destructive? sorted?)
  "Empirical quantiles of VECTOR (copied with COPY-SEQ).  QUANTILES has to be a
sequence, and the result is of the same type.  Elements are interpolated
linearly.  If SORTED?, copying and sorting is skipped (and of course the vector
is not modified)."
  (let* ((vector (cond
                   (sorted? vector)
                   (destructive? (sort vector #'<=))
                   (t (sort (copy-seq vector) #'<=))))
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
