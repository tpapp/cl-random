;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-random-unit-tests)

(named-readtables:in-readtable lla:v-syntax)

(deftestsuite cl-random-unit-tests () ()
  (:equality-test #'approx=))

;; EXTERNAL

(defun run-cl-random-tests ()
  "Run all the tests for LLA."
  (run-tests :suite 'cl-random-unit-tests))
