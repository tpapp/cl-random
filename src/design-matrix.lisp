;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-random)

;;; building blocks for a DSL for design matrices

;;; (list term1 ...)
;;; 
;;; term := constant | covariate | interaction
;;; 
;;; covariate := symbol | (^ symbol exponent)
;;; 
;;; interaction := (* covariate1 covariate2 ...)

(defun interaction-matrix (&rest matrices)
  "Return the interaction matrix.  Last indexes change the fastest."
  (bind ((matrices (mapcar (lambda (matrix)
                             (typecase matrix
                               (dense-matrix-like
                                  (set-restricted matrix)
                                  matrix)
                               (vector (as-column matrix))
                               (otherwise (as-matrix matrix))))
                           matrices))
         (nrow (nrow (first matrices)))
         (ncols (mapcar #'ncol matrices))
         (n (length ncols))
         (elements (map 'vector #'elements matrices))
         (lla-type (reduce #'lla::common-lla-type elements :key #'array-lla-type))
         (zero (zero* lla-type))
         (interaction (make-matrix nrow (reduce #'* ncols) lla-type)))
    (assert (every (lambda (matrix) (= nrow (nrow matrix))) matrices))
    (with-range-indexing ((make-array n :initial-element t)
                          ncols next-index :end? end? :counters counters)
      (iter
        (let ((indexes (map 'vector (lambda (counter)
                                      (cm-index2 nrow 0 counter))
                            counters))
              (col (next-index)))
          (dotimes (row nrow)
            (setf (mref interaction row col)
                  (iter interaction
                    (for index :in-vector indexes)
                    (for elements% :in-vector elements)
                    (let ((element (aref elements% (+ index row))))
                      (when (zerop element)
                        (return-from interaction zero))
                      (multiplying element))))))
        (until end?)))
    interaction))

(defun process-factor (vector &key (key #'identity)
                       (predicate #'<) (test #'=))
  "Return (VALUES INDEXES LEVELS), where KEYS is a vector that contains the
  levels of the factor (formed using KEY, tested for uniqueness using TEST), and
  INDEXES is a vector of FIXNUMS, containing the index of the level
  corresponding to the elements of VECTOR.  If PREDICATE is given, the levels
  are sorted."
  (bind ((keys (coerce (delete-duplicates (map 'list key vector) :test test)
                       'vector)))
    (when predicate
      (setf keys (sort keys predicate)))
    (values (map '(simple-array fixnum (*))
                 (lambda (element)
                   (position (funcall key element) keys :test test))
                 vector)
            keys)))

(defun factor-matrix (indexes levels)
  "Return a design matrix for a factor.  First column is dropped, otherwise
the matrix would be full rank."
  (let* ((nrow (length indexes))
         (matrix (make-matrix nrow (1- (length levels)) :integer)))
    (iter
      (for row :from 0)
      (for index :in-vector indexes)
      (unless (zerop index)
        (setf (mref matrix row (1- index)) 1)))
    matrix))

(defun polynomial-matrix (vector power)
  "Matrix for a polynomial."
  (check-type power (integer 1))
  (let* ((length (length vector))
         (matrix (make-matrix length power (array-lla-type vector))))
    (iter
      (for row :from 0)
      (for v :in-vector vector)
      (dotimes (col power)
        (setf (mref matrix row col) (expt v (1+ col)))))
    matrix))

(defun interaction-name (names)
  "Names for interactions.  Return either a symbol (for size 1), or (list
symbol size)."
  (iter
    (for name :in names)
    (unless (first-iteration-p)
      (collecting '#:* :into interaction-names))
    (if (atom name)
        (collecting name :into interaction-names)
        (progn 
          (collecting (first name) :into interaction-names)
          (multiplying (second name) :into size)))
    (finally
     (let ((interaction-name (apply #'make-symbol* interaction-names)))
       (return
         (if (= 1 size)
             interaction-name
             (list interaction-name size)))))))

(defun design-matrix (matrix ix specifications &key factors
                      (constant :constant) rescale?)
  "Build design matrix from columns of MATRIX, using SPECIFICATIONS, which
refers to columns via the index IX.  FACTORS should be a list, of either a name
in IX or (IX &rest OPTIONS), where OPTIONS are passed directly to
PROCESS-FACTOR.  When CONSTANT is non-nil, a constant column with this name will
be added.  Return IX specification, the matrix, and a list of factors and levels
as values.

Example: 
  (design-matrix (clo :integer
                  1 2 3 :/
                  4 5 6)
                 (make-ix '(a b c))
                 '(a b (:poly c 2) (* b c))
                 :factors '(a))(:CONSTANT (A 1) B (C-POLY 2) B*C)
 =>
  (:CONSTANT (A 1) B (C-POLY 2) B*C)
  #<DENSE-MATRIX :INTEGER with 2 x 6 elements
  1 0 2 3  9  6
  1 1 5 6 36 30>,
  ((A #(1 4)))
"
  (bind (((:flet column (name))
          (sub matrix t (ix ix name)))
         (factors (mapcar (lambda (factor)
                            (bind (((name &rest options) (ensure-list factor))
                                   ((:values indexes levels)
                                    (apply #'process-factor
                                           (column name) options)))
                              (list name (sub levels '(1 . 0))
                                    (factor-matrix indexes levels))))
                          factors)))
    (labels ((find-factor (name)
               (find name factors :key #'car))
             (traverse-list (specifications &optional (top-level? t))
               (iter
                 (for spec :in specifications)
                 (bind (((:values ix matrix) (traverse spec top-level?)))
                   (collecting ix :into ixs)
                   (collecting matrix :into matrices))
                 (finally
                  (return (values ixs matrices)))))
             (traverse (spec top-level?)
               ;; Return two values: an ix spec, and matrices; or lists of
               ;; these, for interactions.
               (cond
                 ((atom spec) (aif (find-factor spec)
                                   (let ((matrix (third it)))
                                     (values (list spec (ncol matrix))
                                             matrix))
                                   (values spec (as-column (column spec)))))
                 ((eq (car spec) :poly)
                  (bind (((name power) (cdr spec)))
                    (assert (not (find-factor name)) ()
                            "Can't use factors of polynomials.")
                    (values (list (make-symbol* name '#:-poly) power)
                            (polynomial-matrix (column name) power))))
                 ((eq (car spec) '*)
                  (assert top-level? ()
                          "Interactions are only allowed at the top level.")
                  (bind (((:values names matrices) (traverse-list (cdr spec) nil)))
                    (values (interaction-name names)
                            (apply #'interaction-matrix matrices))))
                 (t (error "Invalid spec ~A" spec)))))
      (bind (((:values ixs matrices) (traverse-list specifications)))
        (when constant
          (setf ixs (cons constant ixs)
                matrices (cons (lla-vector (nrow (first matrices)) :integer 1)
                               matrices)))
        (values (make-ix ixs)
                (apply #'stack-horizontally (flatten matrices))
                (mapcar (lambda (factor)
                          (list (first factor) (second factor)))
                        factors))))))
