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

(defun dm-size (specification &optional (top-level? t))
  (cond
    ((atom specification) nil)
    ((eq (car specification) '^)
     (bind (((symbol exponent) (cdr specification)))
       (check-type symbol symbol)
       (check-type exponent (integer 0))
       exponent))
    ((eq (car specification) '*)
     (assert top-level? () "Interactions are allow allowed at the top level.")
     (map 'vector (lambda (s) (dm-size s nil)) (cdr specification)))
    (t (error "Invalid specification ~A" specification))))

(dm-size 'foo)
(dm-size '(^ foo 4))
(dm-size '(* foo (^ bar 4)))

(defclass factor ()
  ()
  )

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
         (interaction (make-matrix lla-type nrow (reduce #'* ncols))))
    (assert (every (lambda (matrix) (= nrow (nrow matrix))) matrices))
    (with-range-indexing ((make-array n :initial-element t)
                          ncols next-index :end? end? :counters counters)
      (iter
        (until end?)
        (let ((indexes (map 'vector (lambda (counter)
                                      (cm-index2 nrow 0 counter))
                            counters))
              (col (next-index)))
          (d:v col indexes)
          (dotimes (row nrow)
            (d:v row)
            (setf (mref interaction row col)
                  (iter interaction
                    (for index :in-vector indexes)
                    (for elements% :in-vector elements)
                    (let ((element (aref elements% (+ index row))))
                      (d:v element)
                      (when (zerop element)
                        (return-from interaction zero))
                      (multiplying element))))))))
    interaction))

(interaction-matrix ;(clo 1 2 3) 
                    
                    (clo 0 1 :/
                         1 0
                         0 1)
                    (as-matrix (clo :diagonal 1 1 1)))

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
  "Return a design matrix for a factor."
  (let* ((nrow (length indexes))
         (matrix (make-matrix :integer nrow (length levels))))
    (iter
      (for row :from 0)
      (for index :in-vector indexes)
      (setf (mref matrix row index) 1))
    matrix))

(defun polynomial-matrix (vector power)
  "Matrix for a polynomial."
  (check-type power (integer 1))
  (let* ((length (length vector))
         (matrix (make-matrix (array-lla-type vector) length power)))
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
                      (constant :constant))
  "Build design matrix from columns of MATRIX, using SPECIFICATIONS, which
refers to columns via the index IX.  FACTORS should be a list, of either a name
in IX or (IX &rest OPTIONS), where OPTIONS are passed directly to
PROCESS-FACTOR.  When CONSTANT is non-nil, a constant column with this name will
be added.

Example: 
  (design-matrix (clo :integer
                  1 2 3 :/
                  4 5 6)
                 (make-ix '(a b c))
                 '(a b (:poly c 2) (* b c))
                 :factors '(a))
 =>
  (:CONSTANT (A 2) B (C-POLY 2) B*C),
  #<DENSE-MATRIX :INTEGER with 2 x 7 elements
  1 1 0 2 3  9  6
  1 0 1 5 6 36 30>
"
  (bind (((:flet column (name))
          (sub matrix t (ix ix name)))
         (factors (map 'vector
                      (lambda (factor)
                        (bind (((name &rest options) (ensure-list factor))
                               ((:values indexes levels)
                                (apply #'process-factor
                                       (column name) options)))
                          (list name (factor-matrix indexes levels))))
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
                                   (let ((matrix (second it)))
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
                matrices (cons (lla-vector :integer (nrow (first matrices)) 1)
                               matrices)))
        (values ixs
                (apply #'stack-horizontally (flatten matrices)))))))
