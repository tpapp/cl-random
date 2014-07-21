;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-random)



;;;; Random number generator (RNG) base class

(defclass random-number-generator () 
  ((state :initarg :state
	  :accessor state))
  (:documentation "Base class for random number generators."))

(defgeneric next (rng limit &optional pos)
  (:documentation "Return a non-negative or positive (if POS is T) random number less than LIMIT and of the same type.")
  (:method ((rng random-number-generator) limit &optional pos)
    (error "not implemented"))
  (:method ((rng random-state) limit &optional pos)
    (do ((r (random limit rng)))
	((not (and pos (zerop r))) r))))

(defgeneric clone (rng)
  (:documentation "Return a deep copy of RNG. The stream of random numbers drawn from RNG and its clone should be the same (given you draw according to the same distributions).")
  (:method ((rng random-number-generator))
    (error "not implemented"))
  (:method ((rng random-state))
    (make-random-state rng)))

;; TODO: We may want to read/write a RNGs state.



;;;; Wrapper around your Common Lisp implementation's RNG. 

(defclass cl-rng (random-number-generator)
  ()
  (:documentation "A wrapper around the built-in random number generator. For SBCL this is MT19973, which is fast and good enough for scientific experiments. Furthermore, it is faster than calling GSL's version of MT19973 through GSLL. It may not be portable between different platforms."))

(defmethod initialize-instance :after ((rng cl-rng) &key (seed NIL) (state T) &allow-other-keys)
  (setf (state rng)
	(if seed ;TODO: Other implementations that support seeding?
	    #+sbcl (sb-ext:seed-random-state seed)
	    #-sbcl (error "not implemented")
	    (make-random-state state))))

(defmethod next ((rng cl-rng) limit &optional pos)
  (next (state rng) limit pos))

(defmethod clone ((rng cl-rng))
  (make-instance 'cl-rng :state (clone (state rng))))



;;;; Wrapper around GNU Scientific Library's RNGs via GSLL and FFI.

;;; Calling C libraries can be quite slow. More specifically, (next gsll-rng 1.0) is 5 times slower than and conses twice as much as (next cl-rng 1.0). But GSL provides many RNGs which are not (yet) implemented in Common Lisp or can be used for test purposes.

(defclass gsll-rng (random-number-generator)
  ()
  (:documentation "A wrapper around the GSL RNGs via GSLL and FFI. The default generator is MT19937 with its original seed."))

(defmethod initialize-instance :after ((rng gsll-rng) &key (seed 0) (type gsll:+MT19937+)
							&allow-other-keys)
  ;; For MT19937, SEED is an integer in [0, 2^64). When SEED is 0, the original seed (5489) is used."
  (setf (state rng)
	(gsll:make-random-number-generator type seed)))

;; TODO: How to clone a GSLL RNG?

(defmethod next ((rng gsll-rng) (limit fixnum) &optional pos)
  (do ((r (gsll:sample (state rng) :uniform-fixnum :upperbound limit)))
      ((not (and pos (zerop r))) r)))

(defmethod next ((rng gsll-rng) (limit float) &optional pos)
  (do ((r (gsll:sample (state rng) :flat :a 0.0 :b limit)))
      ((not (and pos (zerop r))) (float r limit))))


;; TODO: Add MT19337 from cl-randist.

