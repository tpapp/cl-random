;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(defpackage #:cl-random-tests
  (:use #:cl
        #:clunit
        #:cl-num-utils.elementwise
        #:cl-num-utils.matrix-shorthand
        #:cl-num-utils.num=
        #:cl-random
        #:let-plus
        #:lla)
  (:export #:run))

(in-package #:cl-random-tests)

(defsuite tests ())

(defun run (&optional interactive?)
  "Run all the tests for CL-RANDOM."
  (run-suite 'tests :use-debugger interactive?))


(defun z-score (n mean variance sample-mean)
  "Calculate abs(z) based on theoretical mean and variance and sample mean."
  (abs (* (- sample-mean mean)
          (sqrt (/ n variance)))))

(defun same-univariate-moments (rv &key (n 100000) (z-band 4d0)
                                        (var-band 0.1))
  (let+ (((&accessors-r/o mean variance) rv)
         (sample-moments (clnu.stats:central-sample-moments nil)))
    (loop repeat n
          do (clnu.stats:add sample-moments (draw rv)))
    (and (< (z-score n mean variance (clnu.stats:mean sample-moments)) z-band)
         (num= variance (clnu.stats:variance sample-moments) var-band))))

;; (defun same-sample-mean-variance (rv &key )
;;   "Generate a sample of length N from RV, then compare moments
;; COMPARE-SAMPLE-MEAN-VARIANCE.  For univariate and multivariate distributions."
;;     (if (vectorp mean)
;;         (let ((moments (clnu:central-sample-moments
;;                         (aops:generate (funcall ))) )))
;;         )

;;     (multivariate? (vectorp mean))
;;     (sample (sweep (if multivariate?
;;                        (conforming-accumulator 'mean mean)
;;                        'variance)
;;                    (replicating rv n)))
;;     (if multivariate?
;;         (every (curry #'> z-band)
;;                (map 'vector (curry #'z-score n) mean (diagonal variance)
;;                     (mean sample)))
;;         )))

(defgeneric relative-difference (a b)
  (:documentation "Relative difference of A and B.")
  (:method ((a number) (b number))
    (/ (abs (- a b))
       (max 1 (abs a) (abs b))))
  (:method ((a array) (b array))
    "Relative difference of A and B."
    (assert (equalp (array-dimensions a) (array-dimensions b)))
    (reduce #'max
            (map 'vector #'relative-difference
                 (aops:flatten a) (aops:flatten b))))
  (:method ((a array) b)
    (relative-difference a (aops:as-array b))))

(defun random-y-x (n k
                   &optional
                   (x-rv (r-normal 0 9))
                   (e-rv (r-normal 0 2))
                   (beta (aops:generate* 'lla-double (generator (r-normal 0 1)) k)))
  "Generate random Y and X for testing regressions."
  (let* ((x (aops:generate* 'lla-double (generator x-rv) (list n k)))
         (y (e+ (mm x beta) (aops:generate* 'lla-double n (generator e-rv)))))
    (values y x)))
