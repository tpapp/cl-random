(in-package :cl-random)

(deftype undefined-feature-status ()
  '(member missing undefined not-implemented))

(define-condition undefined-feature (error)
  ((rv :accessor rv :initarg :rv)
   (feature :accessor feature :initarg :feature)
   (status :accessor status :initarg :status :initform 'missing
           :type undefined-feature-status
           :documentation "UNDEFINED is for concepts which do not exist/make
  sense for a given distribution (eg the mean of a Cachy distribution).  MISSING
  is for features which should be present, but are not yet implemented (this is
  a bug in the library).  NOT-IMPLEMENTED means that it was impractical to write
  and left it out, but may be included if somebody writes it.")))

(defmethod print-object ((undefined-feature undefined-feature) stream)
  (with-slots (rv feature status) undefined-feature
    (format stream "~A is ~A for ~A random variables."
            feature status rv)))

(defmacro flambda (&body body)
  "Defined as a toplevel macro to facilitate formatting in the editor.
Overridden locally within FEATURES of DEFINE-RV."
  (declare (ignore body)))

(defmacro features ((&rest clauses) &body body)
  "Defined as a toplevel macro to facilitate formatting in the editor.
Overridden locally within DEFINE-RV.  Anaphoric, captures the variable X."
  (declare (ignore clauses body)))

(defmacro define-rv (name arguments &rest body)
  "Define a function with given NAME and ARGUMENTS that returns a closure."
  (check-type name symbol)
  (with-unique-names (feature features undefined-feature argument)
    `(defun ,name ,arguments
       (macrolet 
           ((features ((&rest clauses) &body features-body)
              `(labels ((,',features (,',feature x)
                          (declare (ignorable x))
                          (flet ((,',undefined-feature (status)
                                   (error 'undefined-feature :rv ',',name
                                          :feature ,',feature :status status)))
                            (case ,',feature
                              ,@(mapcar 
                                   (lambda (clause)
                                     (bind (((feature . definition) clause))
                                       `(,feature
                                         ,@(if (and (= 1 (length definition))
                                                    (typep (car definition)
                                                           'undefined-feature-status))
                                               `((,',undefined-feature ',(car definition)))
                                               definition))))
                                   clauses)
                           (otherwise
                              (,',undefined-feature 'missing))))))
                 (macrolet ((flambda (&body flambda-body)
                              `(lambda (&optional ,',',feature ,',',argument)
                                 (if ,',',feature
                                     (,',',features ,',',feature ,',',argument)
                                     (progn
                                       ,@flambda-body)))))
                   ,@features-body))))
         ,@body))))

(defmacro define-query-function (name &key generic? arguments (feature name))
  `(,(if generic? 'defmethod 'defun) ,feature
     (,(if generic? '(rv function) 'rv) ,@(ensure-list arguments))
     (funcall rv ',feature ,@(typecase arguments
                               (null nil)
                               (list `((list ,@arguments)))
                               (otherwise (list arguments))))))

(define-query-function mean :generic? t)
(define-query-function variance :generic? t)
(define-query-function cdf :arguments x)
(define-query-function quantile :arguments x)

(defun log-pdf (rv x &optional (normalized? t))
  (funcall rv 'log-pdf (cons x normalized?)))

(defun pdf (rv x &optional (normalized? t))
  (exp (log-pdf rv x normalized?)))

(defmacro log-pdf* (expression &optional constant)
  "Used within FEATURES, this macro expands to a LOG-PDF clause, automatically
taking care of argument destructuring and normalization."
  (with-unique-names (normalized? expression*)
    `(log-pdf
      ,(if constant
           `(let ((x (car x))
                  (,normalized? (cdr x)))
              (let ((,expression* ,expression))
                (if ,normalized?
                    (when ,expression*  ; may be NIL=-Infinity
                      (+ ,expression* ,constant))
                    ,expression*)))
           `(let ((x (car x)))
              ,expression)))))

(defmacro normalized (expression &optional constant)
  "Used within FEATURES, this macro expands to a LOG-PDF clause, automatically
taking care of argument destructuring and normalization."
  (with-unique-names (normalized? expression*)
    (if constant
        `(let ((x (car x))
               (,normalized? (cdr x)))
           (let ((,expression* ,expression))
             (if ,normalized?
                 (when ,expression*    ; may be NIL=-Infinity
                   (+ ,expression* ,constant))
                 ,expression*)))
        `(let ((x (car x)))
           ,expression))))
