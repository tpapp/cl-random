;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; -*-

(in-package #:cl-random)

(defgeneric matrix-mean (matrix)
  (:documentation "Mean of a matrix, columnwise.")
  (:method ((matrix array))
    (let+ ((dimensions (array-dimensions matrix))
           (means (filled-array (second dimensions) #'mean-accumulator)))
      (row-major-loop (dimensions row-major-index row-index col-index)
        (add (aref means col-index)
             (row-major-aref matrix row-major-index)))
      (map 'vector #'mean means))))

(defun demean-matrix (matrix &optional (mean (matrix-mean matrix)))
  (e- matrix (recycle mean :horizontal)))

;; (defun matrix-sse (matrix)
;;   "Return sum of squared errors from the matrix row mean (as a Hermitian
;; matrix) and the mean as the second value."
;;   (bind (((nrow ncol) (array-dimensions matrix))
;;          (mean (make-array ncol :initial-element 0d0))
;;          (sse (make-array (list ncol ncol) :initial-element 0d0)))
;;     (dotimes (row-index nrow)
;;       (dotimes (col-index ncol)
;;         (let ((difference (- (aref matrix row-index col-index)
;;                              (aref mean col-index))))
;;           (incf (aref mean col-index) (/ difference (1+ row-index)))
;;           (dotimes (inner-index (1+ col-index))
;;             (incf (aref sse col-index inner-index)
;;                   (* (- (aref matrix row-index inner-index)
;;                         (aref mean inner-index))
;;                      difference))))))
;;     (values (make-instance 'hermitian-matrix :elements sse) mean)))

(defun matrix-sse (matrix)
  "Return sum of squared errors (possibly as a factorization), and the mean as
a second value."
  ;; let D=(demean matrix), D=QR SSE=D^TD=R^TQ^TQR = R^TR
  (let ((mean (matrix-mean matrix)))
    (values (xx
             (transpose* (qr-r (qr (demean-matrix matrix mean)))))
            mean)))

(defun matrix-variance (matrix)
  "Return the variance-covariance matrix."
  (e/ (matrix-sse matrix) (1- (nrow matrix))))

(defun matrix-mean-and-variance (matrix)
  "Return the variance-covariance matrix."
  (let+ (((&values sse mean) (matrix-sse matrix)))
    (values mean (e/ sse (1- (nrow matrix))))))

;; (defgeneric add-constant-column (object &optional constant)
;;   (:documentation "Prepend a column of CONSTANTs (defaults to 1) before object."))

;; (defmethod add-constant-column ((matrix dense-matrix-like) &optional (constant 1))
;;   "Add a constant column before a vector or a matrix,
;; returning a dense matrix."
;;   (bind (((:slots-r/o elements nrow ncol) matrix))
;;     (aprog1 (make-matrix nrow (1+ ncol) (array-lla-type elements)
;;                          :initial-element constant)
;;       (set-restricted matrix)
;;       (copy-elements elements 0
;;                      (elements it) nrow
;;                      (* nrow ncol)))))

;; (defmethod add-constant-column ((vector vector) &optional (constant 1))
;;   (add-constant-column (as-column vector) constant))

;; (defun column-sums (matrix)
;;   "Calculate the sum of each column, returned as a numeric-vector."
;;   (bind (((:slots-r/o  elements nrow ncol)
;;           (if (typep matrix 'dense-matrix-like)
;;               (set-restricted matrix)
;;               (as-matrix matrix)))
;;          ((:lla-vector sum) (make-similar-vector elements ncol)))
;;     (dotimes (col ncol)
;;       (setf (sum col) 
;;             (lla::sum-elements% elements (cm-index2 nrow 0 col)
;;                                 (cm-index2 nrow nrow col))))
;;     sum))

;; (defun column-means (matrix)
;;   "Calculate the mean of each column, returned as a numeric-vector."
;;   (e/ (column-sums matrix) (nrow matrix)))

;; (defun subtract-from-elements% (elements start end number)
;;   (declare (optimize speed (safety 0))
;;            (fixnum start end))
;;   (lla::with-vector-type-expansion (elements)
;;     (lambda (lla-type)
;;       `(let ((number (coerce* number ,lla-type)))
;;          (declare (type ,(lla::lla->lisp-type lla-type) number))
;;          (iter
;;            (for (the fixnum i) :from start :below end)
;;            (declare (iterate:declare-variables))
;;            (decf (aref elements i) number)))))
;;   (values))

;; (defun demean-vector (vector &optional (mean (mean vector)))
;;   "Subtract mean, return vector."
;;   (aprog1 (copy-vector vector)
;;     (subtract-from-elements% it 0 (length it) mean)))

;; (defun demean-columns (matrix &optional (means (column-means matrix)))
;;   "Subtract mean of columns (for multivariate observations stacked in
;;   the rows of a matrix.  Return demeaned-matrix."
;;   (check-type matrix dense-matrix-like)
;;   (aprog1 (copy-matrix matrix :copy? t :kind :dense)
;;     ;; subtract mean
;;     (bind (((:slots-r/o elements nrow ncol) it))
;;       (dotimes (col ncol)
;;         (subtract-from-elements% elements (cm-index2 nrow 0 col)
;;                                  (cm-index2 nrow nrow col) (aref means col))))
;;     matrix))

;; (defun column-variances (matrix &optional (means (column-means matrix)))
;;   "Variance matrix of columns.  If MEANS is nil, it is not subtracted."
;;   (when means
;;     (setf matrix (demean-columns matrix means)))
;;   (mm t matrix (/ (1- (nrow matrix)))))

;; (defun column-mean-variances (matrix)
;;   "For multivariate observations stacked in the rows of a matrix,
;;   return sample mean and (co)variance matrix as (values mean var).
;;   Their types are numeric-vector and hermitian-matrix, respectively."
;;   ;; values
;;   (bind ((means (column-means matrix))
;;          (matrix (demean-columns matrix means)))
;;     (values means
;;             (column-variances matrix nil))))

;; (defun rescale-by-sd (matrix)
;;   "Rescale matrix by standard deviations, which are returned as a second value."
;;   (let* ((matrix (demean-columns matrix))
;;          (variances (column-variances matrix nil))
;;          (sd (esqrt (as-diagonal variances))))
;;     (values (mm matrix (e/ sd))
;;             (elements sd))))

;; (defun variance->correlation (variance)
;;   "Calculate correlation matrix from variance matrix."
;;   (let ((scaling (emap (lambda (d) (/ (sqrt d))) (as-diagonal variance))))
;;     (mmm scaling variance scaling)))
