(in-package :cl-random)

;;; This code was copied and adapted from MT19937 (http://www.cliki.net/mt19937)
;;; (2014/08/07), which was itself adapted from CMUCL rand-mt19937.lisp -r1.11
;;; (2003/03/06).

;;; CMUCL version by Douglas T. Crosher and Raymond Toy based on public domain code from
;;; Carnegie Mellon University.

;;; Modified for Maxima by Robert Dodier.
;;; (1) Construct floating point numbers using portable operations.
;;; (2) Construct large integers using all bits of each chunk.

;;; Support for the Mersenne Twister, MT19937, random number generator
;;; due to Matsumoto and Nishimura. This implementation has been
;;; placed in the public domain with permission from M. Matsumoto.
;;;
;;; Makoto Matsumoto and T. Nishimura, "Mersenne twister: A
;;; 623-dimensionally equidistributed uniform pseudorandom number
;;; generator.", ACM Transactions on Modeling and Computer Simulation,
;;; 1997, to appear.

;;; The state is stored in a (simple-array (unsigned-byte 32) (627)):
;;;
;;;  0-1:   Constant matrix A. [0, #x9908b0df]
;;;  2:     Index k.
;;;  3-626: State.

(defconstant +mt19937-default-seed+ 5489)
(defconstant +mt19937-chunk-length+ 32)
(defconstant +mt19937-state-length+ 627)
(defconstant +mt19937-n+ 624)
(defconstant +mt19937-m+ 397)
(defconstant +mt19937-upper-mask+ #x80000000)
(defconstant +mt19937-lower-mask+ #x7fffffff)
(defconstant +mt19937-b+ #x9D2C5680)
(defconstant +mt19937-c+ #xEFC60000)

(defclass mt19937 (generator)
  ((chunk-length :initform +mt19937-chunk-length+))
  (:documentation "A local implementation of the MT19937. Use it if you need a RNG
  suitable for scientific simulations but your CL implementation does not provide one, or
  when you want it to be cross-platform/-implementation. SBCL's RNG, for example, is not
  guaranteed to return the same results on different platforms."))



;;;; Clone a random state.

(defmethod clone ((rng mt19937))
  (let ((old-state (state rng))
	(new-state
	 (make-array +mt19937-state-length+ :element-type '(unsigned-byte 32))))
    (dotimes (i +mt19937-state-length+)
      (setf (aref new-state i) (aref old-state i)))
    (make-instance 'mt19937 :state new-state)))



;;;; Initialize a random state.

(defmethod initialize-instance :after ((rng mt19937) &key (seed NIL) &allow-other-keys)
  ;; SEED can be NIL, an integer, or a vector
  (declare (type (or null integer (array (unsigned-byte 32) (*))) seed))
  (setf (state rng) (etypecase seed
		      (null
		       (int-init-random-state +mt19937-default-seed+))
		      (integer
		       (int-init-random-state (ldb (byte 32 0) seed)))
		      ((array (unsigned-byte 32) (*))
		       (vec-init-random-state seed)))))

;; New initializer proposed by Takuji Nishimura and Makota Matsumoto.
;; (See http://www.math.keio.ac.jp/~matumoto/MT2002/emt19937ar.html)
;;
;; This corrects a deficiency in the original initializer wherein the
;; MSB of the seed was not well represented in the state.
;;
;; The initialization routine is described below.  Let s be the seed,
;; mt[] be the state vector.  Then the algorithm is
;;
;; mt[0] = s & 0xffffffffUL
;;
;; for (k = 1; k < N; k++) {
;;   mt[k] = 1812433253 * (mt[k-1] ^ (mt[k-1] >> 30)) + k
;;   mt[k] &= 0xffffffffUL
;; }
;;
;; The multiplier is from Knuth TAOCP Vol2, 3rd Ed., p. 106.
(defun int-init-mt19937-state (&optional (seed +mt19937-default-seed+) state)
  "Return a random state using SEED as seed. Overwrite STATE if supplied."
  (declare (type (integer 1 #xffffffff) seed))
  (let ((state (or state
		   (make-array +mt19937-state-length+ :element-type '(unsigned-byte 32)))))
    (declare (type (simple-array (unsigned-byte 32) (+mt19937-state-length+)) state))
    (setf (aref state 0) 0)
    (setf (aref state 1) #x9908b0df)
    (setf (aref state 2) +mt19937-n+)
    (setf (aref state 3) seed)
    (do ((k 1 (1+ k)))
        ((>= k 624))
      (declare (type (mod 625) k))
      (let ((prev (aref state (+ 3 (1- k)))))
        (setf (aref state (+ 3 k))
              (logand (+ (* 1812433253 (logxor prev (ash prev -30)))
                         k)
                      #xffffffff))))
    state))

;; Here is the algorithm, in C.  init_genrand is the initalizer above,
;; init_key is the seed vector of length key_length.
;;
;;     init_genrand(19650218UL);
;;     i=1; j=0;
;;     k = (N>key_length ? N : key_length);
;;     for (; k; k--) {
;;         mt[i] = (mt[i] ^ ((mt[i-1] ^ (mt[i-1] >> 30)) * 1664525UL))
;;           + init_key[j] + j; /* non linear */
;;         mt[i] &= 0xffffffffUL; /* for WORDSIZE > 32 machines */
;;         i++; j++;
;;         if (i>=N) {
;;           mt[0] = mt[N-1]; i=1;
;;         }
;;         if (j>=key_length) {
;;           j=0;
;;         }
;;     }
;;     for (k=N-1; k; k--) {
;;         mt[i] = (mt[i] ^ ((mt[i-1] ^ (mt[i-1] >> 30)) * 1566083941UL))
;;           - i; /* non linear */
;;         mt[i] &= 0xffffffffUL; /* for WORDSIZE > 32 machines */
;;         i++;
;;         if (i>=N) { mt[0] = mt[N-1]; i=1; }
;;     }
;;
;;     mt[0] = 0x80000000UL; /* MSB is 1; assuring non-zero initial array */
;;
(defun vec-init-mt19937-state (key &optional state)
  "Return a random state using vector KEY as seed. Overwrite STATE if supplied."
  (declare (type (array (unsigned-byte 32) (*)) key))
  (let ((key-len (length key))
        (state (int-init-mt19937-state 19650218 state))
        (i 1)
        (j 0))
    (loop for k from (max key-len +mt19937-n+) above 0 do
          (let ((prev (aref state (+ 3 (1- i)))))
            (setf (aref state (+ 3 i))
                  (ldb (byte 32 0)
                       (+ (aref key j) j
                          (logxor (aref state (+ 3 i))
                                  (ldb (byte 32 0)
                                       (* 1664525
                                          (logxor prev (ash prev -30))))))))
            (incf i)
            (incf j)
            (when (>= i +mt19937-n+)
              (setf (aref state 3)
                    (aref state (+ 3 (- +mt19937-n+ 1))))
              (setf i 1))
            (when (>= j key-len)
              (setf j 0))))
    (loop for k from (1- +mt19937-n+) above 0 do
          (let ((prev (aref state (+ 3 (1- i)))))
            (setf (aref state (+ 3 i))
                  (ldb (byte 32 0)
                       (- (logxor (aref state (+ 3 i))
                                  (* 1566083941
                                     (logxor prev (ash prev -30))))
                          i)))
            (incf i)
            (when (>= i +mt19937-n+)
              (setf (aref state 3)
                    (aref state (+ 3 (- +mt19937-n+ 1))))
              (setf i 1))))
    (setf (aref state 3) #x80000000)
    state))



;;;; Generate random chunk.

(defmethod next-chunk ((rng mt19937))
  (declare (optimize (speed 3) (safety 0)))
  (let* ((state (state rng))
         (k (aref state 2)))
    (declare (type (mod 628) k))
    (when (= k +mt19937-n+)
      (random-mt19937-update state)
      (setf k 0))
    (setf (aref state 2) (1+ k))
    (let ((y (aref state (+ 3 k))))
      (declare (type (unsigned-byte 32) y))
      (setf y (logxor y (ash y -11)))
      (setf y (logxor y (ash (logand y (ash +mt19937-b+ -7)) 7)))
      (setf y (logxor y (ash (logand y (ash +mt19937-c+ -15)) 15)))
      (setf y (logxor y (ash y -18)))
      y)))

(defun random-mt19937-update (state)
  (declare (type (simple-array (unsigned-byte 32) (+mt19937-state-length+)) state)
           (optimize (speed 3) (safety 0)))
  (let ((y 0))
    (declare (type (unsigned-byte 32) y))
    (do ((kk 3 (1+ kk)))
        ((>= kk (+ 3 (- +mt19937-n+ +mt19937-m+))))
      (declare (type (mod 628) kk))
      (setf y (logior (logand (aref state kk) +mt19937-upper-mask+)
                      (logand (aref state (1+ kk)) +mt19937-lower-mask+)))
      (setf (aref state kk) (logxor (aref state (+ kk +mt19937-m+))
                                    (ash y -1) (aref state (logand y 1)))))
    (do ((kk (+ (- +mt19937-n+ +mt19937-m+) 3) (1+ kk)))
        ((>= kk (+ (1- +mt19937-n+) 3)))
      (declare (type (mod 628) kk))
      (setf y (logior (logand (aref state kk) +mt19937-upper-mask+)
                      (logand (aref state (1+ kk)) +mt19937-lower-mask+)))
      (setf (aref state kk) (logxor (aref state (+ kk (- +mt19937-m+ +mt19937-n+)))
                                    (ash y -1) (aref state (logand y 1)))))
    (setf y (logior (logand (aref state (+ 3 (1- +mt19937-n+)))
                            +mt19937-upper-mask+)
                    (logand (aref state 3) +mt19937-lower-mask+)))
    (setf (aref state (+ 3 (1- +mt19937-n+)))
          (logxor (aref state (+ 3 (1- +mt19937-m+)))
                  (ash y -1) (aref state (logand y 1)))))
  (values))
