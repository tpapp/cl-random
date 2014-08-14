(in-package :cl-random)

;;;; Simple Multiplicative Congruential Generator.

(defclass simple-multiplicative-congruential (generator)
  ((default-seed :initform 1) ;Override the DEFAULT-SEED.
   (a :reader a
      :documentation "The multiplier of the sequence x(n+1) = A * x(n) mod M.")
   (m :reader m
      :documentation "The modulo of the sequence x(n+1) = A * x(n) mod M."))
  (:documentation "A multiplicative congruential generator generates the sequence x(n+1) =
  A * x(n) mod M and uses the seed as x(1). A simple multiplicative congruential generator
  is a multiplicative congruential generator with M a power of 2. This allows to implement
  the modulo operation as a bitwise and operation of M-1, which is also the maximum value
  of a random chunk."))

(defmethod initialize-instance :after ((self simple-multiplicative-congruential) &key 
				       &allow-other-keys)
  (with-slots (chunk-length min max m) self
    ;; M = 2^CHUNK-LENGTH, and MAX = M-1
    (let ((modulo (expt 2 chunk-length)))
      (setf m modulo
	    min 1
	    max (1- modulo)))))

(defmethod clone ((self simple-multiplicative-congruential))
  (make-instance (typeof self) :state (state self)))

(defmethod generate-state ((self simple-multiplicative-congruential) seed)
  seed)

(defmethod next-chunk ((self simple-multiplicative-congruential))
  (with-slots (state a max) self
    ;; y = a * x mod m = a * x & m-1 since m is a power of 2.
    (let ((next-state (logand (* a state) max)))
      (setf state next-state)
      next-state)))

(defmethod next-real ((self simple-multiplicative-congruential))
  (with-slots (m) self
    (float (/ (next-chunk self) m))))


;;;; Specific implementations

(defclass transputer (simple-multiplicative-congruential)
  ((a :initform 1664525)
   (chunk-length :initform 32))
  (:documentation "INMOS Transputer Development System generator. "))

(defclass randu (simple-multiplicative-congruential)
  ((a :initform 65539)
   (chunk-length :initform 31))
  (:documentation "The poor IBM randu generator. Park and Miller, Random Number
   Generators: Good ones are hard to find, Communications of the ACM, October 1988, Volume
   31, No 10, pp 1192-1201."))

(defclass borosh13 (simple-multiplicative-congruential)
  ((a :initform 1812433253)
   (chunk-length :initform 32))
  (:documentation "Donald E. Knuth's Borosh-Niederreiter, The Art of Computer Programming,
  Volume 2, Third Edition, Addison-Wesley, pp 106-108."))

(defclass waterman14 (simple-multiplicative-congruential)
  ((a :initform 1566083941)
   (chunk-length :initform 32))
  (:documentation "Donald E. Knuth's Waterman, The Art of Computer Programming, Volume 2,
  Third Edition, Addison-Wesley, pp 106-108."))
