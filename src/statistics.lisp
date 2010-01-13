(in-package :cl-random)

(defun matrix-mean-variance (matrix)
  "For multivariate observations stacked in the rows of a matrix,
  return sample mean and (co)variance matrix as (values mean var).
  Their types are numeric-vector and hermitian-matrix, respectively."
  (bind ((matrix (copy-matrix matrix)))
    (ensure-unshared matrix)
    (bind (((:slots-read-only nrow ncol elements) matrix)
           (mean (make-nv ncol (lla-type matrix)))
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
    ;; values
    (values mean
            (mm t matrix (/ nrow))))))
         
