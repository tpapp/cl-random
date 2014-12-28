(in-package :cl-random)

(defmacro define-rv (name constructor-lambda-list options slots
                     constructor-form &rest methods)
  "Define a random variable, abstracting from the representation.  Syntax:

NAME is a symbol, and will name the class and the creation function.

CONSTRUCTOR-LAMBDA-LIST will be used to wrap the CONSTRUCTOR-FORM, which can use the locally define macro (MAKE :slot-name value1 ...) to initialize slots.

SLOTS is a list of (slot-name &key type read-only reader) slot specifications.  When READER is T, SLOT-NAME is used instead, otherwise a method is defined using the given symbol.

OPTIONS is (&key documentation instance), the default instance is a gensym.

METHODS are (function-name lambda-list &body body), with (INSTANCE NAME) prepended to the lambda-list, ie the instance is accessible using INSTANCE.  Also, within BODY, slots are accessible by their names."
  (check-type name symbol)
  (let+ ((slots (mapcar #'ensure-list slots))
         ((&key documentation (instance (make-gensym name)) num=-slots include)
          options))
    (labels ((local-slots (body)
               ;; !! read-only slots could be expanded using LET for extra speed
               `(symbol-macrolet
                    ,(loop for slot in slots collect
                              (let+ ((slot-name (car slot))
                                     (accessor `(,(symbolicate name '#:- slot-name)
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
         (defstruct (,name ,@(clnu:splice-when include `(:include ,include)))
           ,documentation
           ,@(loop for slot in slots collect
                      (let+ (((slot-name &key type read-only &allow-other-keys) slot))
                        `(,slot-name nil
                                     ,@(awhen type `(:type ,it))
                                     ,@(awhen read-only `(:read-only ,it))))))
         (defun ,name ,constructor-lambda-list
           (macrolet ((make (&rest arguments)
                        `(,',(symbolicate '#:make- name) ,@arguments)))
             ,constructor-form))
         ,@(loop for (method-name lambda-list . body) in methods collect
                    `(defmethod ,method-name ((,instance ,name) ,@lambda-list)
                       ,@(let+ (((&values body declarations) (parse-body body)))
                           `(,@declarations
                             ,(local-slots body)))))
         ,@(when num=-slots
             (with-unique-names (a b tolerance)
               `((defmethod num= ((,a ,name) (,b ,name)
                                  &optional (,tolerance *num=-tolerance*))
                   (and ,@(mapcar
                           (lambda (slot)
                             (let ((accessor (symbolicate name #\- slot)))
                               `(num= (,accessor ,a) (,accessor ,b)
                                      ,tolerance)))
                           num=-slots))))))))))

(defgeneric draw (random-variable &key rng &allow-other-keys)
  (:documentation "Draw random variates.  Can also be used on generators.")
  (:method ((function function) &key (rng *random-state*))
    (funcall function)))

(defgeneric generator (random-variable &key rng)
  (:documentation "Return a closure that returns random draws.")
  (:method (random-variable &key (rng *random-state*))
    (lambda ()
      (draw random-variable :rng rng))))

(defgeneric mean (random-variable)
  (:documentation "Mean of random variable."))

(defgeneric variance (random-variable)
  (:documentation "Variance of random variable."))

(defgeneric cdf (random-variable x)
  (:documentation "Cumulative distribution function of RANDOM-VARIABLE at
  X."))

(declaim (inline check-probability))
(defun check-probability (p &optional open)
  "Assert that P is a probability (ie a real number between 0 and 1).  When OPEN is given, it is checked that p is not 0 (:LEFT), 1 (:RIGHT), or 0/1 (:BOTH)."
     ;; special-functions
  #+sbcl (declare (sb-ext:muffle-conditions sb-ext:code-deletion-note))
  (assert (<= 0 p 1) () "~A is not a valid probability." p)
  (when open
    (let ((msg "The given probability is only attained in the limit."))
      (ecase open
        (:both (assert (/= p 0 1) () msg))
        (:left (assert (/= p 0) () msg))
        (:right (assert (/= p 1) () msg)))))
  t)

(defgeneric log-pdf (random-variable x &optional ignore-constant?)
  (:documentation "Log of probability distribution function of RANDOM-VARIABLE at X.  NIL corresponds to log(-infinity).  When IGNORE-CONSTANT?, the result may be shifted by an arbitrary real constant that does not change between calls of the same RANDOM-VARIABLE.  This may save computation, and is useful for MCMC methods, etc."))

(defun pdf (rv x &optional ignore-constant?)
  "Probability distribution function of RANDOM-VARIABLE at X.  See LOG-PDF for
the semantics of IGNORE-CONSTANT?."
  (aif (log-pdf rv x ignore-constant?)
       (exp it)
       (as-float 0)))
