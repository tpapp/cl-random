(in-package #:cl-random)


;;;; Random number generator (RNG) base class and functions.

;;; Parts of this code are copied and adapted from MT19937 (http://www.cliki.net/mt19937)
;;; (2014/08/07), which was itself adapted from CMUCL rand-mt19937.lisp -r1.11
;;; (2003/03/06).

;;; Usage:

;;; Create a new generator with MAKE-GENERATOR, optionally provide TYPE or SEED.  Call
;;; NEXT to get a new integer or real number.

;;; Adding a new generator:

;;; Each generator should inherit from GENERATOR and implement the generic functions
;;; NEXT-CHUNK, GENERATE-STATE, and COPY-STATE.



;;;; GENERATOR, base class for all generators.

(defclass generator () 
  ((state :initarg :state
	  :accessor state
	  :documentation "All information needed by the generator to create the next chunk
	  of random bits. This state is modified after each call to NEXT-CHUNK.")
   (min :initarg :min
	:documentation "The minimum value return by NEXT-CHUNK.")
   (max :initarg :max
	:documentation "The maximum value return by NEXT-CHUNK.")
   (chunk-length :reader chunk-length
		 :documentation "The length in bits of the integer returned by
		 NEXT-CHUNK.")
   (default-seed :reader default-seed
     :initform 0
     :documentation "The seed used by default, when the seed is NIL."))
  (:documentation "Base class for random number generators."))



;;;; Creating a new GENERATOR by cloning or seeding.

(defparameter *default-generator-type* 'builtin-generator)

(defun make-generator (&key (seed T) (type *default-generator-type*))
  "Make a random number generator object. SEED can be any of NIL, T, an other generator,
an integer, or any type of seed that a generator of type TYPE supports:
- NIL: the generator's STD-SEED is used;
- T: a random seed is used;
- a generator: a clone is returned;
- otherwise: SEED is used as depends on the generator."
  (if (typep seed 'generator)
      (make-instance (type-of seed) :state (copy-state seed))
      (make-instance type :seed seed)))



;;; Seeding a generator.

(defun generate-seed ()
  "Return a 64-bit random seed, based on current time."
  (logand (get-universal-time) #xffffffff))

(defmethod initialize-instance :after ((rng generator) &key (seed T) &allow-other-keys)
  ;; SEED = T => generate seed based on current time (default)
  ;; SEED = NIL => use default seed
  ;; Otherwise => use supplied seed
  (setf (state rng)
	(generate-state rng (cond ((null seed) (default-seed rng))
				  ((eq seed T) (generate-seed))
				  (T seed)))))

(defgeneric generate-state (rng seed)
  (:documentation "Return a state for a generator of RNG's type using seed.")
  (:method ((rng generator) seed)
	   seed))

;; TODO: Provide a mixin for COPY-STATE: COPY-INT-STATE and COPY-INT-ARRAY-STATE.

;; TODO: We may want to read/write a RNGs state.



;;;; Generating a new random number.

;; TODO: Use internal float?

(declaim (inline %next-single-float %next-double-float))
(declaim (ftype (function ((single-float (0f0)) generator)
                          (single-float 0f0))
                %next-single-float))

(declaim (ftype (function ((double-float (0d0)) generator)
                          (double-float 0d0))
                %next-double-float))

(defun %next-single-float (limit rng)
  "Handle the single or double float case of RANDOM. We generate a float in [0f0, 1f0) by
clobbering the mantissa of 1f0 with random bits (23 bits); this yields a number in [1f0,
2f0). Then 1f0 is subtracted."
  (let* ((random-mantissa-bits
	  (%next-integer (expt 2 23) rng))
	 (random-unit-float
	  (- (scale-float (float (+ (expt 2 23) random-mantissa-bits) 1f0) -23) 1f0)))
    (* limit random-unit-float)))

(defun %next-double-float (limit rng)
  "Handle the single or double float case of RANDOM. We generate a float in [0d0, 1d0) by
clobbering the mantissa of 1d0 with random bits (52 bits); this yields a number in [1d0,
2d0). Then 1d0 is subtracted."
  (let* ((random-mantissa-bits
	  (%next-integer (expt 2 52) rng))
	 (random-unit-double
	  (- (scale-float (float (+ (expt 2 52) random-mantissa-bits) 1d0) -52) 1d0)))
    (* limit random-unit-double)))

(defun %next-integer (limit rng)
  "Generates an integer greater than or equal to zero and less than LIMIT. Successive
chunks are concatenated without overlap to construct integers larger than a single
chunk. The return value has this property: If two integers are generated from the same RNG
with LIMIT equal to 2^m and 2^n, respectively, then bit k is the same in both integers for
0 <= k < min(m,n). Each call to %NEXT-INTEGER consumes at least one chunk; bits left over
from previous chunks are not re-used."
  (declare (type (integer 1) limit) (type generator rng))
  (do* ((nchunks (ceiling (integer-length (1- limit)) (chunk-length rng)) (1- nchunks))
	(new-bits 0 (next-chunk rng))
	(bits 0 (logior bits (ash new-bits shift)))
	(shift 0 (+ shift (chunk-length rng))))
       ((= 0 nchunks)
	(rem bits limit))))

(defun next (limit &optional (rng *random-state*))
  "Generates a uniformly distributed pseudo-random number greater than or equal to zero
and less than LIMIT. RNG, if supplied, is the random generator to use."
  (declare (inline %next-single-float %next-double-float))
  (cond ((random-state-p rng)
	 (random limit rng))
	((and (typep limit 'single-float) (> limit 0.0F0))
	 (%next-single-float limit rng))
	((and (typep limit 'double-float) (> limit 0.0D0))
	 (%next-double-float limit rng))
	((and (integerp limit) (> limit 0))
	 (%next-integer limit rng))
	(T
	 (error 'simple-type-error
		:expected-type '(or (integer 1) (float (0))) :datum limit
		:format-control "Argument is not a positive integer or a positive float: ~S"
		:format-arguments (list limit)))))
