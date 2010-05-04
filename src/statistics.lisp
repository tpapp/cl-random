;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; -*-

(in-package #:cl-random)

(defgeneric add-constant-column (object &optional constant)
  (:documentation "Prepend a column of CONSTANTs before object."))

(defmethod add-constant-column ((matrix dense-matrix-like) &optional (constant 1))
  "Add a constant column before a numeric vector or a matrix,
returning a dense matrix."
  (bind (((:slots-r/o lla-type elements nrow ncol) matrix))
    (aprog1 (make-matrix lla-type nrow (1+ ncol)
                         :initial-element constant)
      (set-restricted matrix)
      (copy-elements (* nrow ncol)
                     elements 0 lla-type
                     (elements it) nrow))))

(defmethod add-constant-column ((nv numeric-vector) &optional (constant 1))
  (add-constant-column (vector->column nv) constant))

(defun column-means (matrix)
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

(defun demean-vector (vector &optional (mean (mean vector)))
  "Subtract mean, return vector."
  (bind ((vector (copy-as 'numeric-vector vector))
         ((:slots-read-only lla-type elements) vector))
    (lla::subtract-from-elements lla-type elements 0 (length elements) mean)
    vector))

(defun demean-columns (matrix &optional (means (column-means matrix)))
  "Subtract mean of columns (for multivariate observations stacked in
  the rows of a matrix.  Return demeaned-matrix."
  (check-type matrix dense-matrix-like)
  (bind ((matrix (copy-as 'dense-matrix matrix))
         ((:slots-read-only lla-type elements nrow ncol) matrix)
         ((:lla-vector mean) means))
    ;; calculate & subtract mean
    (dotimes (col ncol)
      (lla::subtract-from-elements lla-type elements (cm-index2 nrow 0 col)
                                   (cm-index2 nrow nrow col) (mean col)))
    matrix))

(defun column-mean-variances (matrix)
  "For multivariate observations stacked in the rows of a matrix,
  return sample mean and (co)variance matrix as (values mean var).
  Their types are numeric-vector and hermitian-matrix, respectively."
  ;; values
  (bind ((means (column-means matrix))
         (matrix (demean-columns matrix means)))
    (values means
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
