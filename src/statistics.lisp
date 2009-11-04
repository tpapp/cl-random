(in-package :cl-random)

(defun matrix-mean-var (matrix)
  "For multivariate observations stacked in the rows of a matrix,
  return sample mean and (co)variance matrix as (values mean var).
  Their types are numeric-vector and hermitian-matrix, respectively."
  (bind ((matrix (take 'dense-matrix matrix)) ; now we can modify
         ((nrow ncol) (xdims matrix))
         (mean (make-nv ncol (lla-type matrix) 0)))
    ;; using the underlying vector directly
    (copy-data matrix)
    (let ((matrix-data (nv-data (data matrix)))
          (mean-data (nv-data mean)))
      ;; calculate & subtract mean
      (dotimes (col ncol)
        (let* ((start (cm-index2 nrow 0 col))
               (end (cm-index2 nrow 0 (1+ col)))
               (col-mean (/ (reduce #'+ matrix-data
                                   :start start :end end)
                            nrow)))
          (setf (aref mean-data col) col-mean)
          (iter
            (for index :from start :below end)
            (decf (aref matrix-data index) col-mean)))))
    ;; values
    (values mean
            (update-syhe matrix 'hermitian-matrix t :alpha (/ nrow)))))
         
