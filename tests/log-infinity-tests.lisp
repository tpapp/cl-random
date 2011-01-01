;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-random-unit-tests)

(deftestsuite log-infinity-tests (cl-random-unit-tests)
  ())

(addtest (log-infinity-tests)
  log-infinity-plus-minus
  (let ((*lift-equality-test* #'eql))
    (ensure-same (~+ 8 9d0) 17d0)
    (ensure-same (~+ 8 nil) nil)
    (ensure-same (~+ nil nil 0 nil) nil)
    (ensure-same (~- 8 9d0) -1d0)
    (ensure-same (~- nil 8) nil)
    (ensure-error (~- nil))
    (ensure-error (~- 8 nil))
    (ensure-error (~- 8 9 nil))
    (ensure-same (~- nil 8 9) nil)
    (ensure-same (~- 9 8 1d0) 0d0)))

(addtest (log-infinity-tests)
  log-infinity-plus-minus
  (let ((*lift-equality-test* #'eql))
    (ensure-same (~exp 12) (exp 12))
    (ensure-same (~exp nil) 0)
    (ensure-error (~exp #C(0 1)))
    (ensure-same (~log 12) (log 12))
    (ensure-same (~log 0d0) nil)
    (ensure-same (~log 0) nil)
    (ensure-error (~log -12))
    (ensure-error (~log #C(0 1)))))

