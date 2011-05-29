;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-random)

;;; numerical differentiation

(defconstant +numdiff-epsilon+ 0.001d0)

(deftype bfgs-objective ()
  '(function ((simple-array double-float (*))) (or null double-float)))

(deftype simple-double-vector ()
  '(simple-array double-float (*)))

(declaim (inline richardson-derivative3))
(defun richardson-derivative3 (h f fh/2 fh)
  (/ (- (* 4 (- fh/2 f)) (- fh f)) h))

(defun num-gradient (f x &key (fx (funcall f x))
                     (epsilon +numdiff-epsilon+)
                     (dfx (lla-array (length x) :double)))
  "Calculate numerical gradient of F at X, and place it in DFX, which is
returned.  Uses Richardson extrapolation w/ forward differencing."
  (declare (optimize speed)
           (function f))
  (check-type dfx simple-double-vector)
  (let* ((x (if (typep x 'simple-double-vector) x (copy-vector x :double)))
         (fx (coerce fx 'double-float))
         (epsilon (coerce epsilon 'double-float))
         (epsilon/2 (/ epsilon 2))
         (y (copy-vector x :double)))
    (declare (type simple-double-vector x y dfx))
    (dotimes (i (length y))
      (let ((x-i (aref x i)))
        (setf (aref y i) (+ x-i epsilon/2))
        (let ((fh/2 (funcall f y)))
          (setf (aref y i) (+ x-i epsilon))
          (setf (aref dfx i) (richardson-derivative3 epsilon fx fh/2 (funcall f y)))
          (setf (aref y i) x-i)))))
  dfx)

(defun num-hessian (f x &key (fx (funcall f x))
                    (epsilon +numdiff-epsilon+))
  "Calculate and return the numerical Hessian of F at X."
  (declare (optimize speed)
           (type (simple-array double-float (*)) x)
           (double-float fx epsilon)
           (type bfgs-objective f))
  (let* ((n (length x))
         (y (copy-vector x :double))
         (fy (lla-array n :double))
         (ddfx (lla-array (expt n 2) :double))
         (epsilon^2 (expt epsilon 2)))
    (declare (type (simple-array double-float (*)) y fy))
    (dotimes (i n)                      ; column
      ;; changing only one coordinate
      (incf (aref y i) epsilon)
      (setf (aref fy i) (funcall f y))
      ;; hessian
      (loop
        for j from 0 to i               ; row
        do (incf (aref y j) epsilon)
           (setf (aref ddfx (cm-index2 n j i))
                 (/ (- (+ (funcall f y) fx)
                       (+ (aref fy i) (aref fy j)))
                    epsilon^2))
           (setf (aref y j) (aref x j))))
    (lla::make-matrix% n n ddfx :kind :hermitian)))

(defmacro loop-max-iter ((max-iter 
                          &optional (end-form '(error 'reached-max-iter)))
                         &body body)
  "Execute loop count down using max-iter, call end-form when it reaches zero."
  (check-type max-iter symbol)
  `(loop
     (when (zerop ,max-iter)
       ,end-form)
     (decf ,max-iter)
     ,@body))

(defclass bfgs-parameters ()
  ((linesearch-max-iter :initarg :linesearch-max-iter :initform 100)
   (numdiff-epsilon :initarg :numdiff-epsilon :initform +numdiff-epsilon+
                    :documentation "Step size for numerical differentiation.")
   (relative :initarg :relative :initform 16d0
             :documentation "added to elements before comparing with =")
   ;; ;; parameters common to all line search algorithms
   (rho :initarg :rho :initform 1d-4 :documentation "Parameter for the
    sufficient decrease condition, ie f(alpha) <= f(0) + alpha rho f'(0).
    Typical values is rho=1d-4.")
   (sigma :initarg :sigma :initform 0.9d0 :documentation "Parameter for the weak
    or strong curvature condition, eg |f'(alpha)| <= -sigma f'(0).  Should be
    between 0.1 and 0.9.")
   ;; parameters for sophisticated line search algorithm: Sections 2.4-2.5 of
   ;; Fletcher (2000) and Chapter 3 of Nocedal and Wright (2006).  Nomenclature
   ;; loosely follows the former.
   (alpha-max :initarg :alpha-max :initform 5d0 :documentation "Maximum alpha we
    are willing to accept.")
   (tau1 :initarg :tau1 :initform 9d0 :documentation "> 1, used to blow up the
    interval for bracketing.  9 is suggested.")
   (tau2 :initarg :tau2 :initform 0.1d0 :documentation "0 < tau2 < tau3 <= 0.5
    keep the bracketing algorithm away from the endpoints of the interval.  tau2
    <= sigma is suggested, typical values are tau2=0.1 and tau3=0.5.")
   (tau3 :initarg :tau3 :initform 0.5d0 
         :documentation "see tau2 of the same class.")
   ;; parameters for Armijo line search algorithm
   (step-reduction :initarg :step-reduction :initform 0.2d0
                   :documentation "step reduction in Armijo line search,
    strictly between 0 and 1.")
   ;; parameters for the weak Wolfe line search
   (max-bisections :initarg :max-bisections :initform 30 :documentation
                   "Maximum number of bisections before giving up.")
   (max-expansions :initarg :max-expansions :initform 10 :documentation
                   "Maximum number of expansions before giving up."))
  (:documentation "See Nocedal and Wright (2006, chapter 3), Fletcher (2000,
  sections 2.4-5), and Lewis and Overton (2010)."))

(defmethod initialize-instance :after ((bfgs-parameters bfgs-parameters) &key
                                       &allow-other-keys)
  ;; various checks
  (let+ (((&slots-r/o linesearch-max-iter numdiff-epsilon relative rho sigma
                      alpha-max tau1 tau2 tau3 step-reduction
                      max-bisections max-expansions) bfgs-parameters))
    (check-type linesearch-max-iter positive-fixnum)
    (check-type numdiff-epsilon positive-double-float)
    (check-type relative (double-float 0d0))
    (check-type rho positive-double-float)
    (check-type sigma positive-double-float)
    (assert (< rho sigma))
    ;; sophisticated search algorithm
    (check-type alpha-max (double-float 1d0))
    (check-type tau1 (double-float 1d0))
    (check-type tau2 double-float)
    (check-type tau3 (double-float 0d0 0.5d0))
    (assert (and (< 0 tau2 tau3) (<= tau2 sigma)))
    ;; Armijo
    (check-type step-reduction double-float)
    (assert (< 0d0 step-reduction 1d0))
    ;; weak Wolfe
    (check-type max-bisections (integer 10 100))
    (check-type max-expansions (integer 5 30))))

(defparameter *default-bfgs-parameters* (make-instance 'bfgs-parameters))

;; ;;; Algorithm for strong Wolfe conditions.

;; (defun sw-find-acceptable-alpha (f df a fa dfa b fb dfb
;;                                  f0 slope curvature max-iter epsilon 
;;                                  bfgs-parameters)
;;   ;; notes: dfb may be NIL, in which case quadratic interpolation is used
;;   (bind (((:slots-r/o tau2 tau3) bfgs-parameters))
;;    (loop-max-iter (max-iter)
;;      (let* ((w (- b a))
;;             (min (+ a (* tau2 w)))
;;             (max (- b (* tau3 w)))
;;             (alpha (if dfb
;;                        (cubic-minimum a fa dfa b fb dfb min max)
;;                        (quadratic-minimum a fa dfa b fb min max)))
;;             (f-alpha (funcall f alpha)))
;;        (when (and epsilon (<= (* (- a alpha) dfa) (* epsilon (1+ (abs fa)))))
;;          ;; no progress is made
;;          (return (values alpha f-alpha 'no-progress)))
;;        (if (or (> f-alpha (+ f0 (* slope alpha)))
;;                (>= f-alpha fa))
;;            (setf (values b fb dfb) (values alpha f-alpha nil))
;;            (let ((df-alpha (funcall df alpha)))
;;              (if (< (abs df-alpha) curvature)
;;                  (return (values alpha f-alpha))
;;                  (setf (values b fb dfb)
;;                        (if (<= 0 (* (- b a) df-alpha))
;;                            (values a fa dfa)
;;                            (values b fb dfb))
;;                        (values a fa dfa) (values alpha f-alpha df-alpha)))))))))

;; (defun linesearch-sw (f df f0 df0 alpha epsilon bfgs-parameters)
;;   (bind (((:slots-r/o linesearch-max-iter alpha-max rho sigma tau1)
;;           bfgs-parameters)
;;          (slope (* rho df0))
;;          (curvature (- (* sigma df0)))
;;          (alpha-prev 0d0)
;;          (f-alpha-prev f0)
;;          (df-alpha-prev df0))
;;     (loop-max-iter (linesearch-max-iter)
;;       (let ((f-alpha (funcall f alpha)))
;;         (when (or (> f-alpha (+ f0 (* slope alpha)))
;;                   (>= f-alpha f-alpha-prev))
;;           (return
;;             (sw-find-acceptable-alpha
;;              f df 
;;              alpha-prev f-alpha-prev df-alpha-prev
;;              alpha f-alpha nil
;;              f0 slope curvature linesearch-max-iter epsilon bfgs-parameters)))
;;         (let ((df-alpha (funcall df alpha)))
;;           (when (<= (abs df-alpha) curvature)
;;             (return (values alpha f-alpha)))
;;           (when (<= 0 df-alpha)
;;             (return 
;;               (sw-find-acceptable-alpha
;;                f df 
;;                alpha f-alpha df-alpha
;;                alpha-prev f-alpha-prev df-alpha-prev 
;;                f0 slope curvature linesearch-max-iter epsilon bfgs-parameters)))
;;           (let* ((left (- (* 2 alpha) alpha-prev))
;;                  (alpha-next 
;;                   (if (<= alpha-max left)
;;                       alpha-max
;;                       (cubic-minimum alpha f-alpha df-alpha
;;                                      alpha-prev f-alpha-prev df-alpha-prev
;;                                      left
;;                                      (min alpha-max
;;                                           (+ alpha 
;;                                              (* tau1 (- alpha alpha-prev))))))))
;;             (setf alpha-prev alpha
;;                   f-alpha-prev f-alpha
;;                   df-alpha-prev df-alpha
;;                   alpha alpha-next)))))))



;; (defun bfgs-minimize (f x &key (df nil df?) 
;;                       (delta (expt double-float-epsilon 1/2))
;;                       (epsilon (expt double-float-epsilon 1/2))
;;                       gamma
;;                       (H (eye (length x) :lla-type :double :kind :hermitian) H?)
;;                       (max-iter 200)
;;                       use-df-for-linesearch?
;;                       (linesearch #'linesearch-ww)
;;                       count-f-eval?
;;                       (bfgs-parameters *default-bfgs-parameters*))
;;   "Parameters:
   
;;    f : An R^n=>R function to be optimized.

;;    x : Initial point.

;;    df : Gradient function.  If not given, one will be calculated using finite
;;      differences (recommended).

;;    delta : Convergence criterion in x:
;;      |change in x|_2 <= delta*(1+|x|_2).  Ignored when nil.

;;    epsilon : Convergence criterion in f(x):
;;      |change in f(x)| <= epsilon*(1+|f(x)|).  Ignored when nil.

;;    gamma : Convergence criterion in df(x). 
;;      |change in df(x)|_sup <= gamma.  Ignored when nil.

;;    Convergence happens when all three are met (except for those which are
;;    ignored).

;;    H : Inital inverse Hessian.  If not given, initialized with a identity matrix
;;      and scaled after the first iteration.

;;    max-iter : maximum number of iterations.

;;    use-df-for-linesearch? : When non-nil, and df is given, the latter will be
;;      used also to calculate gradients in line search.  Otherwise, a numerical
;;      difference will be used.  Note that the latter is quite cheap, since it
;;      requires only two evaluations.

   

;; Return
;;   (values
;;     x              ; the optimum
;;     fx             ; value at x
;;     dfx            ; gradient at x
;;     H              : approximate inverse hessian
;;     iter-count     ; iter-count
;;     f-eval-count)  ; count of evaluations, when requested, o/w nil"
;;    ;; various consistency checks
;;   (check-type delta (or null (double-float 0d0)))
;;   (check-type epsilon (or null (double-float 0d0)))
;;   (check-type gamma (or null (double-float 0d0)))
;;   (check-type H hermitian-matrix)
;;   (check-type max-iter positive-fixnum)
;;   ;; loop
;;   (bind (((:slots-r/o numdiff-epsilon) bfgs-parameters)
;;          (f-eval-count 0)
;;          (f (if count-f-eval?
;;                 (lambda (x) (incf f-eval-count) (funcall f x))
;;                 f))
;;          (df (if df? df (numdiff* f numdiff-epsilon)))
;;          (fx (funcall f x))
;;          (dfx (funcall df x))
;;          ((:flet done (iter-count))
;;           (return-from bfgs-minimize
;;             (values x fx dfx H
;;                     iter-count (when count-f-eval? f-eval-count)))))
;;     (iter
;;       (d:v x)
;;       (d:v fx)
;;       (d:v dfx)
;;       (d:v H)
;;       (for iter-count :from 0 :below max-iter)
;;       ;; check convergence (using gradient)
;;       (when (below? (norm2 dfx) gamma)
;;         (done iter-count))
;;       (bind ((p (d:p (mm H dfx -1)))
;;              (f-uni (lambda (alpha) (funcall f (e+ x (e* p alpha)))))
;;              (df-uni (if (and df? use-df-for-linesearch?)
;;                          (lambda (alpha) (dot (funcall df (e+ x (e* p alpha))) p))
;;                          (numdiff1* f-uni numdiff-epsilon)))
;;              ((:values alpha f-alpha) 
;;               (funcall linesearch f-uni df-uni fx (dot dfx p) 1d0 
;;                        epsilon bfgs-parameters))
;;              (s (e* p alpha)))
;;         ;; check convergence (using relative changes)
;;         (when (and (below? (/ (abs (- fx f-alpha)) (1+ (abs fx))) epsilon)
;;                    (below? (/ (norm2 s) (1+ (norm2 x))) delta))
;;           (d:d "rel conv: ~a, ~a" 
;;                (/ (abs (- fx f-alpha)) (1+ (abs fx)))
;;                (/ (norm2 s) (1+ (norm2 x))))
;;           (done iter-count))
;;         ;; update value and derivatives
;;         (let ((dfx-prev dfx))
;;           (setf x (e+ x s)
;;                 fx f-alpha
;;                 dfx (funcall df x)
;;                 H (bfgs-update-inverse-hessian H (e- dfx dfx-prev) s 
;;                                                (and (not H?) 
;;                                                     (first-iteration-p)))))))))

(defun negative-quadratic-form (x A negative-Ax)
  "Calculate -Ax and place it in NEGATIVE-AX, return -x^TAx.  A contains the
elements of an NxN Hermitian matrix, in column-major order, of which only the
upper half is used.  x and negative-Ax are vector, of dimension N (not checked).
All vectors are assumed to have element type DOUBLE-FLOAT."
  (declare (optimize speed)
           (type simple-double-vector x A negative-Ax))
  (let ((negative-xAx 0d0)
        (n (length x)))
    (dotimes (i n)
      (let ((negative-sum 0d0)
            (column-start (* n i)))
        (declare (double-float negative-sum)
                 (fixnum column-start))
        ;; upper half
        (loop
          for x-index :from 0 :to i
          for A-index :from column-start
          do (decf negative-sum (* (aref x x-index) (aref A A-index))))
        ;; lower half
        (loop
          for x-index :from (1+ i) :below n
          for A-index :from (+ column-start n i) :by n
          do (decf negative-sum (* (aref x x-index) (aref A A-index))))
        ;; save and accumulate
        (incf negative-xAx (* negative-sum (aref x i)))
        (setf (aref negative-Ax i) negative-sum)))
    negative-xAX))

(defun x+direction (x direction alpha &optional 
                    (z (lla-array (length x) :double)) (relative 16d0))
  "Calculate X+ALPHA*DIRECTION, place it in Z.  All vectors (X, DIRECTION,
Z) are assumed to have element type double-float (not checked), and the same
length (not checked).  ALPHA is a double-float.  RELATIVE is added to elements
of X and T before comparison, and the function returns NIL as the first value
iff all are =.  Z is returned as the second value."
  (let ((different? nil)
        (n (length x)))
    (declare (optimize speed)
             (type simple-double-vector x direction z)
             (double-float alpha relative))
    (dotimes (i n)
      (let* ((x-elt (aref x i))
             (z-elt (+ x-elt (* alpha (aref direction i)))))
        (setf (aref z i) z-elt)
        (unless (= (+ x-elt relative) (+ z-elt relative))
          (setf different? t))))
    (values different? z)))

;;; Calling conventions for line search functions
;;;
;;; Parameters:
;;;   f - the multivatiate function
;;;   df-uni - univariate derivative, called with alpha
;;;   x - the starting point
;;;   df0 - the univariate derivative at 0
;;;   alpha - starting alpha
;;;   z - resulting x ends up here
;;;   bfgs-parameters - all other parameters picked from here
;;; 
;;; Return values
;;;   alpha - the suggested alpha
;;;   f-alpha - the value at this alpha
;;;   convergence indicator: NIL if OK, :ZERO if converged to X, 

(defun linesearch-armijo (f df-uni x direction fx df0 alpha z bfgs-parameters)
  "Armijo line search.  Last x evaluated is available in z."
  (declare (ignore df-uni))
  (let+ (((&slots-r/o linesearch-max-iter relative rho step-reduction)
          bfgs-parameters))
    (loop-max-iter (linesearch-max-iter)
      (let* ((different? (x+direction x direction alpha z relative))
             (fz (funcall f z))
             (accepted? (and fz (<= fz (+ fx (* rho alpha df0))))))
        (if (or accepted? (not different?))
            (return-from linesearch-armijo (values alpha fz
                                                   (if accepted? nil :zero)))
            (multf alpha step-reduction))))))

(defun linesearch-ww (f df-uni x direction fx df0 alpha z bfgs-parameters)
  "Uses the weak Wolfe conditions f(alpha) <= rho alpha f'(0) and f'(alpha) >=
sigma f'(0).  Algorithm from Lewis and Overton (2010).  Return ALPHA, F-ALPHA
and ACCEPTED?"
  (let+ ((left 0d0)
         (right nil)
         ((&slots-r/o linesearch-max-iter rho sigma relative
                      max-expansions max-bisections) bfgs-parameters)
         (slope (* rho df0))
         (curvature (* sigma df0))
         (bisections 0)
         (expansions 0))
    (iter
      (repeat linesearch-max-iter)
      (let+ ((different? (x+direction x direction alpha z relative))
             (f-alpha (funcall f z))
             ((&flet done (status))
              (return-from linesearch-ww (values alpha f-alpha status))))
        (cond
          ;; no progress
          ((not different?) (done :zero))
          ;; sufficient decrease condition fails, or function is not defined
          ((or (not f-alpha) (> f-alpha (+ fx (* slope alpha))))
           (setf right alpha))
          ;; curvature condition fails
          ((< (funcall df-uni alpha f-alpha) curvature) (setf left alpha))
          ;; both hold
          (t (done nil)))
        (setf alpha 
              (if right
                  (if (<= (incf bisections) max-bisections)
                      (/ (+ left right) 2d0)
                      (done :bracketed))
                  (if (<= (incf expansions) max-expansions)
                      (* 2d0 alpha)
                      (done :failed)))))))
  (error 'reached-max-iter))

(defun bfgs-update-inverse-hessian (H s y z rescale?)
  "Uses z as a workspace, modifies H.  Return nil if update is impossible (sy
too small), in this case, no argument is changed.."
  (declare (optimize speed)
           (type (simple-array double-float (*)) H s y z))
  (let ((n (length s))
        (sy 0d0))
    ;; dot product
    (dotimes (i n)
      (incf sy (* (aref s i) (aref y i))))
    ;; check sy
    (unless (plusp sy)
      (return-from bfgs-update-inverse-hessian nil))
    ;; rescale?
    (when rescale?
      (let ((y2 0d0))
        (dotimes (i n)
          (incf y2 (expt (aref y i) 2)))
        (let ((scale (/ sy y2)))
          (dotimes (i (the fixnum (expt n 2)))
            (multf (aref H i) scale)))))
    ;; modify upper half of Hessian
    (let* ((negative-yBy (the double-float (negative-quadratic-form y H z)))
           (scale (- 1d0 (/ negative-yBy sy))))
      (dotimes (j n)                    ; column
        (loop
          for i :from 0 :to j           ; row
          for H-index :from (the fixnum (* n j))
          do (incf (aref H H-index)
                   (/ (+ (* scale (aref s i) (aref s j))
                         (* (aref z i) (aref s j))
                         (* (aref z j) (aref s i)))
                      sy)))))
    t))


(defun bfgs-minimize (f x &key (df nil df?) 
 ;                     (delta (expt double-float-epsilon 1/2))
                      (epsilon (expt double-float-epsilon 1/2))
                      ;; gamma
                      (H (eye (length x) :kind :hermitian :lla-type :double) H?)
                      (max-iter 200)
                      ;; use-df-for-linesearch?
                      (linesearch :ww)
                      (bfgs-parameters *default-bfgs-parameters*)
                      finish-silently?
                      tracer)
  (let+ (((&slots-r/o numdiff-epsilon relative) bfgs-parameters)
         (linesearch (ecase linesearch
                       (:armijo #'linesearch-armijo)
                       (:ww #'linesearch-ww)))
         (n (length x))
         (x (copy-vector x :double))
         (z (lla-array n :double))     ; work area: x+direction*alpha
         (s (lla-array n :double))     ; direction, then stepsize
         (y (lla-array n :double))     ; work area
         dfx                            ; gradient
         (f-eval-count 0)
         ((:flet f (x))
          (incf f-eval-count)
          (awhen (funcall f x)
            (coerce it 'double-float)))
         (df (if df?                    ; incorporate fx!
                 df
                 (lambda (x) (num-gradient #'f x :epsilon numdiff-epsilon))))
         (fx (f x))
         (H (if H? (copy-matrix H :lla-type :double :copy? t) H))
         (update-count 0)
         (last-reset -1)
         (reset-count 0)
         ((:flet reset (reason))
          (unless (< last-reset update-count)
            (error "Iteration got stuck after ~A updates (reason: ~A)."
                   update-count reason))
          (setf last-reset update-count)
          (incf reset-count)
          ;; reset H with the identity matrix
          (let ((offset (1+ n))
                (H (elements H)))
            (dotimes (i (length H))
              (setf (aref H i) (if (zerop (rem i offset)) 1d0 0d0)))))
         ((:flet done ())
          (return-from bfgs-minimize
            (values x fx dfx H update-count f-eval-count))))
    (unless fx
      (error "Function is not defined at initial point."))
    (unless H?
      (reset "initialing"))
    (setf dfx (funcall df x))
    (loop-max-iter (max-iter (if finish-silently?
                                  (done)
                                  (error 'reached-max-iter)))
      (when tracer
        (funcall tracer update-count :fx fx :x x))
      (let ((df0 (negative-quadratic-form dfx (elements H) s)))
        (if (minusp df0)
            ;; descent direction, try line search
            (let+ (((&flet df-uni (alpha f-alpha))
                    ;; using y, no one else is
                    (let* ((epsilon (/ numdiff-epsilon (norm2 s)))
                           (changed? (x+direction x s (+ alpha (/ epsilon 2))
                                                  y relative)))
                      (if changed?
                          (let ((fh/2 (funcall f y)))
                            (x+direction x s (+ alpha epsilon) y relative)
                            (richardson-derivative3 epsilon f-alpha fh/2
                                                    (funcall f y)))
                          0d0)))
                   ((&values alpha f-alpha status)
                    (funcall linesearch #'f #'df-uni x s fx df0 1d0 z 
                             bfgs-parameters)))
              (when (<= (/ (abs (- fx f-alpha)) (+ (abs fx) epsilon)) epsilon)
                (done))
              (if (not (eq status :zero))
                  (progn
                    ;; s is scaled to be the step
                    (dotimes (i n)
                      (multf (aref s i) alpha))
                    ;; calculate new gradient and difference, 
                    ;; overwrite old gradient
                    (setf y (funcall df z))
                    (rotatef y dfx)      ; y: old, dfx: new
                    (map-into y #'- dfx y)
                    ;; save new value
                    (setf fx f-alpha)
                    (rotatef x z)
                    ;; update
                    (incf update-count)
                    (unless (bfgs-update-inverse-hessian (elements H) s y z nil

                          ;; (and (not H?)
                          ;;      (zerop update-count))
                          )
                      (reset "update can't preserve positive definiteness")))
                  ;; could not find acceptable alpha, try to reset
                  (reset "linesearch converged to current point")))
            ;; nonnegative gradient, try to reset
            (reset "nonnegative gradient"))))))

