;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-random-unit-tests)

(deftestsuite optimization-tests (cl-random-unit-tests)
  ()
  (:equality-test #'lla=))

(addtest (optimization-tests)
  num-diff-tests
  (use-locally ((cl-random richardson-derivative3 num-gradient num-hessian))
    (let+ (((&flet g (x) (expt x 2)))
           (h 1d-3)
           (x 3d0)
           (*lla=-difference* 1d-10)
           ((&flet gg (x)              ; a linear function
              (let+ ((#(x1 x2) x))
                (+ (* 3d0 x1) (* 5d0 x2)))))
           ((&flet hessian-test (x)
              (let+ ((#(x1 x2) x))
                (+ (* 2d0 (expt x1 2))
                   (* 3d0 x1 x2)
                   (* 4d0 (expt x2 2)))))))
      ;; richardson extrapolation
      (ensure-same (richardson-derivative3 h (g x) (g (+ x (/ h 2))) (g (+ x h)))
                   (* 2 x))
      ;; numerical gradient
      (ensure-same (num-gradient #'gg (clo :double 99 17))
                   (clo :double 3d0 5d0))
      ;; numerical hessian
      (let ((*lla=-difference* 1d-5))
        (ensure-same (num-hessian #'hessian-test (clo :double 7 11))
                     (clo :double :hermitian
                          4 3 :/
                          * 8))))))

;; (addtest (optimization-tests)
;;   minmax-tests
;;   (use-locally ((cl-random minmax))
;;     (ensure-same (minmax 1 2 3) 2)
;;     (ensure-same (minmax 2.2 2 3) 2.2)
;;     (ensure-same (minmax 4 2 3) 3)))

;; (addtest (optimization-tests)
;;   quadratic-minimum-tests
;;   (use-locally ((cl-random quadratic-minimum))
;;     ;; (x-1)^2: at 0, f=1, f'=-2; at 3, f=4
;;     (ensure-same (quadratic-minimum 0 1 -2 3 4 1 4) 1)
;;     (ensure-same (quadratic-minimum 0 1 -2 3 4 1.5 4) 1.5)
;;     (ensure-same (quadratic-minimum 0 1 -2 3 4 1 1.5) 1)
;;     ;; same, but at other end, at 3, f'=4
;;     (ensure-same (quadratic-minimum 3 4 4 0 1 4 1) 1)
;;     (ensure-same (quadratic-minimum 3 4 4 0 1 4 1.5) 1.5)
;;     (ensure-same (quadratic-minimum 3 4 4 0 1 1.5 1) 1)
;;     ;; with a < 0: -(x-2)^2+3.  at -1, f=-6, f'=6; at 4, f=-1, f'=-4
;;     (ensure-same (quadratic-minimum -1 -6 6 4 -1 -1 4) -1) ; because -6 < 4
;;     (ensure-same (quadratic-minimum -1 -6 6 4 -1 -1 1.5)
;;                  -1)                    ; because 3-(1.5-2)^2=3-0.25=2.75 > -6
;;     (ensure-same (quadratic-minimum  4 -1 -4 -1 -6 -1 4) -1) ; because -6 < -1
;;     (ensure-same (quadratic-minimum  4 -1 -4 -1 -6 3 0)
;;                  0)))                   ; because f(3)=2 > f(0)=-1
    
;; (addtest (optimization-tests) 
;;   cubic-minimum-tests
;;   ;; x^3+2x^2-3x+1.  At x=-1, f=3, f'=-4; at y=2, f=9, f'=17, min at ≃0.5352
;;   (let ((min 0.5351838))
;;     (use-locally ((cl-random cubic-minimum))
;;       (ensure-same (cubic-minimum -1 3 -4 2 9 17 -1 2) min)
;;       (ensure-same (cubic-minimum 2 9 17 -1 3 -4 2 -1) min)
;;       (ensure-same (cubic-minimum 2 9 17 -1 3 -4 0.2 1) min)
;;       (ensure-same (cubic-minimum 2 9 17 -1 3 -4 .5 .6) min)
;;       (ensure-same (cubic-minimum -1 3 -4 2 9 17 -0.4 -1)
;;                    -0.4) ; because f(-1)=3 > f(-0.4)=0.456
;;       (ensure-same (cubic-minimum 2 9 17 -1 3 -4 1.1 1.4)
;;                    1.1)))) ; because f(1.4)=1.46 > f(1.1)=-0.549

;; (addtest (optimization-tests)
;;   linesearch-tests
;;   ;; Example from Fletcher (2000), 2.6
;;   (flet ((f (alpha)
;;            (+ (* 100 (expt alpha 4))
;;               (square (- 1 alpha))))
;;          (df (alpha)
;;            (- (* 400 (expt alpha 3))
;;               (* 2 (- 1 alpha)))))
;;     (ensure-same (cl-random::linesearch-sw #'f #'df (f 0) (df 0) 1 1d-6
;;                                            (make-instance 'bfgs-parameters
;;                                                           :maxiter 100
;;                                                           :alpha-max 5d0
;;                                                           :rho 0.01d0
;;                                                           :sigma 0.1d0
;;                                                           :tau1 9d0
;;                                                           :tau2 0.1d0
;;                                                           :tau3 0.5d0))
;;                  0.1609215939d0
;;                  :test #'lla=)))


(addtest (optimization-tests)
  negative-quadratic-form-test
  (let* ((A (clo :double :hermitian 
                 1 3 :/
                 * 17))
         (x (clo :double 5 7))
         (negative-Ax (lla-vector (length x) :double))
         (negative-Ax2 (mm A x -1))
         (negative-xAx (cl-random::negative-quadratic-form x (elements A) negative-Ax)))
    (ensure-same negative-xAx (dot x negative-Ax2))
    (ensure-same negative-Ax negative-Ax2)))

(addtest (optimization-tests)
  x+direction-tests
  (let* ((x (clo :double 1 2 3))
         (direction (clo :double 10 20 30))
         (result (make-similar-vector x)))
    (use-locally ((cl-random x+direction))
      (x+direction x direction 1d0 result)
      (ensure-same result (clo :double 11 22 33))
      (x+direction x direction (/ (epsilon* :double) 8) result)
      (values result x))))

(addtest (optimization-tests)
  bfgs-updating-tests
  (let* ((H (mm t (clo :hermitian :double
                       1 2 :/
                       * 4)))
         (y (clo :double 5 7))
         (s (clo :double 11 13))
         (z (make-similar-vector s))
         ;; alternative formula (Nocedal and Wright (6.14) and (6.17)
         (rho (/ (dot s y)))
         (A (e- (eye (length y)) (mm s y rho)))
         (H-next1 (e+ (copy-matrix (mmm A H (transpose A))
                                   :kind :hermitian :copy? nil)
                      (mm s t rho)))
         (H-next2 (copy-matrix H :copy? t)))
    (cl-random::bfgs-update-inverse-hessian (elements H-next2) s y z nil)
    (ensure-same H-next1 H-next2)))

;;; testing the BFGS minimizer

(defmacro define-ls-function ((name start min &rest aux)
                              &body forms)
  "Define a function NAME for least squares approximation, using the given
forms.  The function will return the vector corresponding to START and MIN when
called with :START and :MIN, respectively.  Vectors are given as list of
numbers, which are converted to double float vectors.  The elements of the
argument are bound to X1, X2, ...  AUX gives pairs, which are used in LET+
before forms are evaluated, squared and summed."
  (let+ (((docstring &rest forms)
          (if (stringp (first forms)) forms (cons nil forms)))
         (n (length start)))
    (assert (= n (length min)) () "Incompatible dimensions.")
    `(defun ,name (x)
       ,(awhen docstring it)
       (case x
         (:start (clo :double ,@start))
         (:min (clo :double ,@min))
         (otherwise 
            (let+ ((#(,@(iter
                          (for i from 0 below n)
                          (collecting (intern (format nil "X~A" (1+ i))))))
                    x)
                   ,@aux)
              (+ ,@(mapcar (lambda (form) `(expt ,form 2))
                           forms))))))))

;;; Functions here are from Moré et al (1981).

(define-ls-function (rosenbrock (-1.2 1) (1 1))
  "Rosenbrock \"(banana)\" function."
  (* 10d0 (- x2 (expt x1 2)))
  (- 1d0 x1))

(define-ls-function (powell-bs (0 1) (1.098d-5 9.106))
  "Powell badly scaled function."
  (1- (* 1d4 x1 x2))
  (+ (exp (- x1)) (exp (- x2)) -1.0001))

(define-ls-function (brown-bs (1 1) (1d6 2d-6))
  "Brown badly scaled function."
  (- x1 1d6)
  (- x2 2d-6)
  (- (* x1 x2) 2d0))

(define-ls-function (helical-valley (-1 0 0) (1 0 0)
                                    (theta (if (plusp x1)
                                               (/ (atan (/ x2 x1)) 2d0 pi)
                                               (+ (/ (atan (/ x2 x1)) 2d0 pi)
                                                  0.5d0))))
  "Helical valley function."
  (* 10d0 (- x3 (* 10d0 theta)))
  (* 10d0 (1- (sqrt (+ (expt x1 2) (expt x2 2)))))
  x3)

(define-ls-function (wood (-3 -1 -3 -1) (1 1 1 1))
  "Wood function."
  (* 10d0 (- x2 (expt x1 2)))
  (- 1d0 x1)
  (* (sqrt 90d0) (- x4 (expt x3 2)))
  (- 1d0 x3)
  (* (sqrt 10) (+ x2 x4 -2d0))
  (* (expt 10 -0.5d0) (- x2 x4)))

(defmacro test-ls-function (function &optional
                            (allowed-difference 1d-4)
                            &rest parameters)
  "Test convergence within ALLOWED-DIFFERENCE for FUNCTION, which may be a
function name, or a form yielding a function."
  (with-unique-names (function-designator minimum start results)
    `(let* ((,function-designator ,(if (symbolp function)
                                       `',function
                                       function))
            (,minimum (funcall ,function-designator :min))
            (,start (funcall ,function-designator :start))
            (,results (multiple-value-list
                          (bfgs-minimize ,function-designator ,start
                                         ,@parameters)))
            (*lla=-difference* ,allowed-difference))
       (ensure-same (car ,results) ,minimum :test #'lla=)
       (cons (e- (car ,results) ,minimum) ,results))))

(defun extend-ls-function (original m)
  "Extend an R^n->R function to R^(m*n)->R, by summing original function on
subsequences of the argument.  Also handles :MIN and :START.  For use on
functions similar to those constructed by DEFINE-LS-FUNCTION.  Return the
difference between the expected and calculated minimum location, and all the
values as a list."
  (let+ ((original-min (funcall original :min))
         (n (length original-min))
         (min (rep original-min m))
         (start (rep (funcall original :start) m)))
    (lambda (x)
      (case x
        (:min min)
        (:start start)
        (otherwise
           (let ((length (length x)))
             (assert (zerop (rem length n)))
             (iter
               (for index :from 0 :below length :by n)
               (summing (funcall original (subseq x index (+ index n)))))))))))

(addtest (optimization-tests)
  bfgs-tests
  (test-ls-function rosenbrock 1d-3)
  (test-ls-function powell-bs)
  (test-ls-function brown-bs)
  (test-ls-function helical-valley)
  (test-ls-function wood)
  (test-ls-function (extend-ls-function #'rosenbrock 10) 1d-3)
  ;; (test-ls-function (extend-ls-function #'powell-bs 2))
  )

;; (defun rosenbrock-grad (x)
;;   "Numerical gradient of the Rosenbrock function."
;;   (bind ((#(x1 x2) x)
;;          (dx2 (* 200d0 (- x2 (square x1)))))
;;     (clo :double
;;          (+ (* -2d0 dx2 x1) (* -2d0 (- 1d0 x1)))
;;          dx2)))

;;; nonsmooth function

;; (defun nc1 (x)
;;   "Bivariate function, nonsmooth."
;;   (case x
;;     (:start (clo :double 5 5))
;;     (:min (clo :double 1 1))
;;     (otherwise
;;        (bind ((#(x1 x2) x))
;;          (+ (square (- 1d0 x1))
;;             (abs (- x2 (square x1))))))))

;; (bfgs-minimize #'nc1 (nc1 :start) :linesearch :armijo)

;; (test-ls-function nc1 1d-2)
