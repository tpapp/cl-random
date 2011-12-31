;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-random-tests)

(deftestsuite cl-random-tests () ()
  (:equality-test #'==))

;; EXTERNAL

(defun run ()
  "Run all the tests for LLA."
  (run-tests :suite 'cl-random-tests))
