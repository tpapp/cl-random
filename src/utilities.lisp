(in-package :cl-random)

;;;; !! At some point, this file should be split up for clarity.  That
;;;; should be done once the structure of the library stabilizes. -- Tamas

;;; ?? these 2 are rather useful macros, this is the 3rd library when
;;; I see them, maybe put into a common library? not really worth the
;;; overhead in work I guess -- Tamas

(defmacro define-abstract-class (classname super-list &body body)
  "A wrapper for DEFCLASS that lets you define abstract base classes.
   If you try to instantiate an object of this class, a warning is signaled."
  `(progn
     (defclass ,classname ,super-list ,@body)

     ;; Protect against abstract class instantiation.

     ;; We could remove this programmatically later using a
     ;; compile-time constant (or even check the optimization options
     ;; and remove it if SAFETY is set low enough).
     (defmethod initialize-instance :before ((x ,classname) &key)
       (if (eql (type-of x) ',classname)
	   (warn "~A is an abstract base class and not to be instantiated." 
                 (quote ',classname))))))

(defun make-symbol* (&rest args)
  "build a symbol by concatenating each element of ARGS, and intern it
  in the current package.  Elements can be strings or symbols."
  (intern (apply #'concatenate 'string
                 (mapcar (lambda (arg)
                           (etypecase arg
                             (symbol (symbol-name arg))
                             (string arg)))
                         args))))


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


;;;; Conditions
;;;;
;;;; Errors are used to signal missing features (which are planned in
;;;; the future) and things that are missing because they don't make
;;;; sense (undefined, eg the mean of a Cauchy distribution) or are
;;;; impractical/not needed/not planned (not-implemented, eg
;;;;  multivariate distribution functions).

(define-condition missing (error)
  ())

(define-condition not-implemented (error)
  ())

(define-condition undefined (error)
  ())

;;;; Types
;;;;

(deftype positive-double-float ()
  `(and double-float
	(satisfies plusp)))

(deftype truncation-boundary ()
  '(or double-float null))

(deftype vector-double-float (&optional n)
  `(simple-array double-float (,n)))

(defun vector-plusp (v)
  (every #'plusp v))

(deftype vector-positive-double-float (&optional n)
  `(and (vector-double-float ,n)
        (satisfies vector-plusp)))

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

(defun print-with-slots (rv stream slots)
  (print-unreadable-object (rv stream :type t)
    (dolist (slot slots)
      (format stream " ~A=~A" (symbol-name slot) (slot-value rv slot)))))

(defmacro define-printer-with-slots (class &rest slots)
  (check-type class symbol)
  (check-type slots list)
  (assert (every #'symbolp slots))
  `(defmethod print-object ((rv ,class) stream)
     (print-with-slots rv stream ',slots)))

;;;; Slots calculated when needed.

(defmacro cached-slot ((instance-variable class slot-name) &body body)
  "Define a slot-unbound method for slot-name, using the value
returned by body."
  (check-type instance-variable symbol)
  (check-type class symbol)
  (check-type slot-name symbol)
  (with-unique-names (value)
    `(defmethod slot-unbound (class (,instance-variable ,class)
                              (slot-name (eql ',slot-name)))
       (let ((,value (locally ,@body)))
         (setf (slot-value ,instance-variable ',slot-name) ,value)
         ,value))))
