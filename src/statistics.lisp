;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; -*-

(in-package #:cl-random)

(defun demean-matrix (matrix)
  "Remove mean of columns (for multivariate observations stacked in
  the rows of a matrix.  Return (values demeaned-matrix mean)."
  (check-type matrix dense-matrix-like)
  (bind ((matrix (copy-as 'dense-matrix matrix))
         ((:slots-read-only nrow ncol elements) matrix)
         (mean (make-nv (lla-type matrix) ncol))
         (mean-elements (elements mean)))
    ;; calculate & subtract mean
    (dotimes (col ncol)
      (let* ((start (cm-index2 nrow 0 col))
             (end (cm-index2 nrow 0 (1+ col)))
             (col-mean (/ (reduce #'+ elements
                                  :start start :end end)
                          nrow)))
        (setf (aref mean-elements col) col-mean)
        (iter
          (for index :from start :below end)
          (decf (aref elements index) col-mean))))
    (values matrix mean)))

(defun matrix-mean-variance (matrix)
  "For multivariate observations stacked in the rows of a matrix,
  return sample mean and (co)variance matrix as (values mean var).
  Their types are numeric-vector and hermitian-matrix, respectively."
  ;; values
  (bind (((:values matrix mean) (demean-matrix matrix)))
    (values mean
            (mm t matrix (/ (nrow matrix))))))
         
