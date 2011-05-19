(in-package :cl-random)

;;;; !! At some point, this file should be split up for clarity.  That
;;;; should be done once the structure of the library stabilizes. -- Tamas

;;; ?? these 2 are rather useful macros, this is the 3rd library when
;;; I see them, maybe put into a common library? not really worth the
;;; overhead in work I guess -- Tamas

(defun concat-to-string (args)
  (apply #'concatenate 'string
                 (mapcar (lambda (arg)
                           (etypecase arg
                             (symbol (symbol-name arg))
                             (string arg)))
                         args)))

(defun make-symbol* (&rest args)
  "build a symbol by concatenating each element of ARGS, and intern it
  in the current package.  Elements can be strings or symbols."
  (intern (concat-to-string args)))

;;; Macro for shortcut functions.  They are useful if you just need an
;;; rv for a single calculation and then throw it away.

(defmacro def* (name other-args &optional docstring)
  "Define a name* shortcut function."
  (check-type name symbol)
  (check-type docstring (or string null))
  `(defun ,(make-symbol* name "*") (rv ,@other-args &rest args)
     ,@(if docstring (list docstring) nil)
     (check-type rv symbol)
     (,name (apply #'make-instance rv args) ,@other-args)))


;;;; Types
;;;;

(deftype truncation-boundary ()
  '(or double-float null))

(deftype double-float-vector (&optional n)
  `(simple-array double-float (,n)))

;;;; Comparisons for truncated distributions.
;;;;
;;;; The convention is that nil indicates no truncation (from that
;;;; direction).  For the functions below, only the second argument is
;;;; allowed to be nil.  These are inlined for speed.
;;;;
;;;; ?? the idea of using most-positive-double-float etc has occured,
;;;; but looks rather inelegant -- Tamas

(declaim (inline <* >*))

(defun <* (a b)
  "Always t if b is nil, otherwise (< a b)."
  (if b
      (< a b)
      t))

(defun >* (a b)
  "Always t if b is nil, otherwise (> a b)."
  (if b
      (> a b)
      t))

;;;; Macro for rejection methods.

(defmacro try ((&rest bindings) condition value)
  "Evaluate bindings (expanding into bind, so all features can be
used) until condition is satisfied, then return value."
  (with-unique-names (top)
    `(prog ()
        ,top
        (bind ,bindings
          (if ,condition
              (return ,value)
              (go ,top))))))

;;;; An acceptable default way of printing: just enumerate slots that
;;;; characterize the distribution.

;; (defun print-with-slots (rv stream slots)
;;   "A slot can be a symbol (used with slot-value)."
;;   (print-unreadable-object (rv stream :type t)
;;     (dolist (slot slots)
;;       (format stream " ~A=~A" (symbol-name slot) (slot-value rv slot)))))

;; (defmacro define-printer ((class &key (instance 'rv) (stream 'stream)) &body body)
;;   (check-type class symbol)
;;   (check-type instance symbol)
;;   (check-type stream symbol)
;;   `(defmethod print-object ((,instance ,class) ,stream)
;;      (print-unreadable-object (,instance ,stream :type t)
;;        ,@body)))

;; (defmacro define-printer-with-slots (class &rest slots)
;;   (check-type class symbol)
;;   (check-type slots list)
;;   (assert (every #'symbolp slots))
;;   `(defmethod print-object ((rv ,class) stream)
;;      (print-with-slots rv stream ',slots)))

;; ;;;; Slots calculated when needed.

;; (defmacro define-cached-slot ((instance-variable class slot-name) &body body)
;;   "Define a slot-unbound method for slot-name, using the value
;; returned by body."
;;   (check-type instance-variable symbol)
;;   (check-type class symbol)
;;   (check-type slot-name symbol)
;;   (with-unique-names (value)
;;     `(defmethod slot-unbound (class (,instance-variable ,class)
;;                               (slot-name (eql ',slot-name)))
;;        (let ((,value (locally ,@body)))
;;          (setf (slot-value ,instance-variable ',slot-name) ,value)
;;          ,value))))

;;; we use doubles for most calculations

(defconstant +pi+ (float pi 1d0)
  "Pi, with double precision.  Defined because cl:pi is long-float and we need
  double-float.")

(defconstant +normal-log-pdf-constant+ (* -0.5d0 (log (* 2 +pi+))))

(declaim (inline as-double-float as-double-float-vector))

(defun as-double-float (x)
  "Return the argument coerced to a DOUBLE-FLOAT."
  (coerce x 'double-float))

(deftype double-float-vector (&optional (n '*))
  `(simple-array double-float (,n)))

(defun as-double-float-vector (vector &key copy?)
  (if (or copy? (not (typep vector 'double-float-vector)))
      (map 'double-float-vector #'as-double-float vector)
      vector))

(defun as-double-float-probabilities (vector)
  "Normalize vector as probabilities, assert that all are positive, return
them as a VECTOR-DOUBLE-FLOAT.  Vector is always copied."
  ;; !! still gives notes, make this faster if necessary
  (declare (optimize speed))
  (let* ((vector (as-double-float-vector vector))
         (sum (reduce #'+ vector)))
    (declare (type double-float sum)
             (type double-float-vector vector))
    (map 'double-float-vector
         (lambda (x)
           (declare (type double-float x))
           (assert (<= 0 x) (x) "Element is not positive.")
           (/ x sum))
         vector)))

(defmacro with-doubles (bindings &body body)
  "Coerces value to DOUBLE-FLOAT, and binds it to VAR in (VAR VALUE) bindings.
If the binding is a symbol, or VALUE is missing, VAR will be used instead.  All
variables are declared DOUBLE-FLOAT in the body."
  (let ((bindings (mapcar (lambda (binding)
                            (bind (((variable &optional (value variable))
                                    (if (atom binding)
                                        (list binding binding)
                                        binding)))
                              (check-type variable (and symbol (not null)))
                              `(,variable (as-double-float ,value))))
                          bindings)))
    `(let ,bindings
       (declare (type double-float ,@(mapcar #'first bindings)))
       ,@body)))

