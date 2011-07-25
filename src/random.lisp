(in-package :cl-random)

(defmacro define-rv (name constructor-lambda-list options slots
                     constructor-form &rest methods)
  "Define a random variable, abstracting from the representation.  Syntax:

NAME is a symbol

CONSTRUCTOR-LAMBDA-LIST will be used to wrap the CONSTRUCTOR-FORM, which can
use the locally define macro (MAKE :slot-name value1 ...) to initialize slots.

SLOTS is a list of (slot-name &key type read-only reader) slot specifications.
When READER is T, SLOT-NAME is used instead, otherwise a method is defined
using the given symbol.

OPTIONS is (&key documentation instance), the default instance is a gensym.

METHODS are (function-name lambda-list &body body), with (INSTANCE NAME)
prepended to the lambda-list, ie the instance is accessible using INSTANCE.
Also, within BODY, slots are accessible by their names."
  (check-type name symbol)
  (let+ ((slots (mapcar #'ensure-list slots))
         ((&key documentation (instance (gensym+ name))) options))
    (labels ((local-slots (body)
               ;; !! read-only slots could be expanded using LET for extra speed
               `(symbol-macrolet
                    ,(loop for slot in slots collect
                      (let+ ((slot-name (car slot))
                             (accessor `(,(make-symbol* name '#:- slot-name)
                                          ,instance)))
                        `(,slot-name ,accessor)))
                  ,@body)))
      ;; collect extra information from slot definitions
      (loop for slot in slots do
        (let+ (((slot-name &key reader &allow-other-keys) slot))
          (awhen reader
            (let ((reader (if (eq reader t) slot-name reader)))
              (push (list reader nil slot-name) methods)))))
      ;; define form
      `(progn
         (defstruct ,name
           ,documentation
           ,@(loop for slot in slots collect
             (let+ (((slot-name &key type read-only &allow-other-keys) slot))
               `(,slot-name nil
                            ,@(awhen type `(:type ,it))
                            ,@(awhen read-only `(:read-only ,it))))))
         (defun ,name ,constructor-lambda-list
           (macrolet ((make (&rest arguments)
                        `(,',(make-symbol* '#:make- name) ,@arguments)))
             ,constructor-form))
         ,@(loop for (method-name lambda-list . body) in methods collect
                 `(defmethod ,method-name ((,instance ,name) ,@lambda-list)
                    ,(local-slots body)))))))

;;; standard methods (MEAN and VARIANCE already defined in CL-NUM-UTILS)

(defgeneric draw (random-variable &key &allow-other-keys)
  (:documentation "Draw random variates.  Can also be used on generators.")
  (:method ((function function) &key)
    (funcall function)))

(defgeneric generator (random-variable)
  (:documentation "Return a closure that returns random draws.")
  (:method (random-variable)
    (lambda ()
      (draw random-variable))))

(defstruct (replicating
             (:constructor replicating (random-variable n)))
  "Wrapper structure for drawing from a random variable."
  (random-variable)
  (n 0 :type fixnum))

(defmethod sweep (accumulator (replicating replicating) &key (key #'identity))
  (let+ (((&structure-r/o replicating- random-variable n) replicating))
    (with-accumulator (accumulator add)
      (loop repeat n :do (add (funcall key (draw random-variable)))))))

(defgeneric cdf (random-variable x)
  (:documentation "Cumulative distribution function of RANDOM-VARIABLE at
  X."))

(declaim (inline check-probability))
(defun check-probability (p &optional limits)
  "Assert that P is a probability (ie a real number between 0 and 1)."
  (assert (<= 0 p 1) () "~A is not a valid probability." p)
  (when limits
    (let ((msg "The given probability is only attained in the limit."))
     (ecase limits
       (:both (assert (/= p 0 1) () msg))
       (:left (assert (/= p 0) () msg))
       (:right (assert (/= p 1) () msg)))))
  t)

(defgeneric quantile (random-variable q)
  (:documentation "Quantile of RANDOM-VARIABLE at Q."))

(defgeneric log-pdf (random-variable x &optional ignore-constant?)
  (:documentation "Log of probability distribution function of RANDOM-VARIABLE
  at X.  NIL corresponds to log(-infinity).  When IGNORE-CONSTANT?, the result
  may be shifted by an arbitrary real constant that does not change between
  calls of the same RANDOM-VARIABLE.  This may save computation, and is useful
  for MCMC methods, etc."))

(defun pdf (rv x &optional ignore-constant?)
  "Probability distribution function of RANDOM-VARIABLE at X.  See LOG-PDF for
the semantics of IGNORE-CONSTANT?."
  (~exp (log-pdf rv x ignore-constant?)))

(defmacro maybe-ignore-constant (ignore-constant? value constant)
  "Handle a constant that is calculated only when IGNORE-CONSTANT? is NIL and
VALUE is not negative infinity (represented by NIL)."
  (once-only (value)
    `(when ,value
       (if ,ignore-constant?
           ,value
           (+ ,value ,constant)))))
