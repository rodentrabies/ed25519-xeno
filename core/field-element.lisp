(uiop:define-package :ed25519/core/field-element
    (:use :cl :ed25519/core/low-level)
  (:export
   ;; data
   #:field-element
   ;; operations
   #:fe
   #:fe-zero
   #:fe-one
   #:fe-copy
   #:fe-add
   #:fe-sub
   #:fe-neg
   #:fe-negative?
   #:fe-non-zero?
   #:fe-mul
   #:fe-square
   #:fe-double-square
   #:fe-invert
   ;; utils
   #:fe-from-bytes
   #:fe-to-bytes
   #:fe-cmove
   #:fe-pow22523
   ;; constants
   #:+d+
   #:+2d+
   #:+sqrt-minus-one+
   #:+a+))

(in-package :ed25519/core/field-element)

(declaim (optimize (speed 3) (safety 0)))



;;;------------------------------------------------------------------------------
;;; Data types

;; Constant `+fe-size+' is the size of int32 array, that is used
;; to represent field elements.
(defconstant +fe-size+ 10)

;; Type `field-element' represents an element of the field GF(2^255 - 19).
;; For an instance `fe', its slots `fe[0]', `fe[1]',  ..., `f[9]' an
;; integer fe[0] + 2^26*fe[1] + 2^51*fe[2] + 2^77*fe[3] + ... + 2^230*t[9].
(deftype field-element () `(simple-array int32 (,+fe-size+)))

;; Type `field-element-ext' represents intermediary field-element value which
;; is a result of some of the operations (i.e. multiplication).
(deftype field-element-ext () `(simple-array int64 (,+fe-size+)))

(defmacro fe (&rest es)
  "Create a new field element, initialized with elements `es' if there
   is exactly 10 of them."
  (declare (list es))
  (ecase (length es)
    (0  `(make-array +fe-size+ :element-type 'int32))
    (10 `(make-array +fe-size+ :element-type 'int32 :initial-contents ',es))))

(defmacro fe-ext (&rest es)
  "Create a new extended field element, initialized with elements `es' if there
   is exactly 10 of them."
  (declare (list es))
  (ecase (length es)
    (0  `(make-array +fe-size+ :element-type 'int64))
    (10 `(make-array +fe-size+ :element-type 'int64 :initial-contents ',es))))



;;;------------------------------------------------------------------------------
;;; Operations

(declaim (ftype (function () field-element) fe-zero))
(defun fe-zero ()
  "Create a new field element with value 0."
  (make-array +fe-size+ :element-type 'int32 :initial-element 0))

(declaim (ftype (function (field-element) field-element) fe-copy))
(defun fe-copy (a)
  "Copy field element."
  (copy-seq a))

(declaim (ftype (function () field-element) fe-one))
(defun fe-one ()
  "Create a new field element with value 1."
  (let ((zero (fe-zero)))
    (setf (aref zero 0) 1)
    zero))

(declaim (ftype (function (field-element field-element) field-element) fe-add))
(defun fe-add (a b)
  "Add two field elements."
  (loop
     :with result :of-type field-element := (fe)
     :for i :below +fe-size+
     :do (setf (aref result i) (+ (aref a i) (aref b i)))
     :finally (return result)))

(declaim (ftype (function (field-element field-element) field-element) fe-sub))
(defun fe-sub (a b)
  "Subtract two field elements."
  (loop
     :with result :of-type field-element := (fe)
     :for i :below +fe-size+
     :do (setf (aref result i) (- (aref a i) (aref b i)))
     :finally (return result)))

(declaim (ftype (function (field-element) field-element) fe-neg))
(defun fe-neg (a)
  "Negate field element."
  (loop
     :with result :of-type field-element := (fe)
     :for i :below +fe-size+
     :do (setf (aref result i) (- (aref a i)))
     :finally (return result)))

(declaim (ftype (function (field-element) (unsigned-byte 8)) fe-negative?))
(defun fe-negative? (a)
  "Return 1 if field element is negative, 0 otherwise."
  (logand (aref (fe-to-bytes a) 0) 1))

(declaim (ftype (function (field-element) int32) fe-non-zero?))
(defun fe-non-zero? (a)
  "Constant-time check whether field element is non-zero."
  (loop
     :with buf :of-type (bytes *) := (fe-to-bytes a)
     :with x :of-type (unsigned-byte 8) := 0
     :for b :below (length buf)
     :do (setf x (logior x (aref buf b)))
     :finally
       (progn
         (setf x (logior x (ash x -4)))
         (setf x (logior x (ash x -2)))
         (setf x (logior x (ash x -1)))
         (return (logand x 1)))))

(declaim (ftype (function (field-element-ext) field-element) fe-combine))
(defun fe-combine (h)
  "Combine 64-bit coefficients resulting from multiplication of field elements
   into 32-bit representation by narrowing the range of each coefficient."
  (loop
     :with c :of-type field-element-ext := (fe-ext)
     :for i :below (1- +fe-size+)
     :do (let* ((shift (if (evenp i) 26 25)))
           (setf (aref c i) (>> (+ (aref h i) (<< 1 (1- shift) 64)) shift 64))
           (incf (aref h (1+ i)) (aref c i))
           (decf (aref h i) (<< (aref c i) shift 64)))
     :finally (progn
                (setf (aref c 9) (>> (+ (aref h 9) (<< 1 24 64)) 25 64))
                (incf (aref h 0) (* (the int32 (aref c 9)) 19))
                (decf (aref h 9) (<< (aref c 9) 25 64))
                (setf (aref c 0) (>> (+ (aref h 0) (<< 1 25 64)) 26 64))
                (incf (aref h 1) (aref c 0))
                (decf (aref h 0) (<< (aref c 0) 26 64))))
  (loop
     :with result :of-type field-element := (fe)
     :for i :below +fe-size+
     :do (setf (aref result i) (the int32 (aref h i)))
     :finally (return result)))

(declaim (ftype (function (field-element field-element int32)) fe-cmove))
(defun fe-cmove (f g b)
  "Destructively perform assignment f = if b == 1 then g else f
   without revealing the value of b."
  (loop
     :with b :of-type int32 := (- b)
     :for i :below +fe-size+
     :for c := (logand b (logxor (aref f i) (aref g i)))
     :do (setf (aref f i) (logxor (aref f i) c))))

(declaim (ftype (function ((bytes *)) field-element) fe-from-bytes))
(defun fe-from-bytes (in)
  "Deserialize field element from byte array."
  (let ((fe-ext (fe-ext)))
    (setf (aref fe-ext 0) (loadint 4 in))
    (setf (aref fe-ext 1) (<< (loadint 3 (subseq in 4)) 6 64))
    (setf (aref fe-ext 2) (<< (loadint 3 (subseq in 7)) 5 64))
    (setf (aref fe-ext 3) (<< (loadint 3 (subseq in 10)) 3 64))
    (setf (aref fe-ext 4) (<< (loadint 3 (subseq in 13)) 2 64))
    (setf (aref fe-ext 5) (loadint 4 (subseq in 16)))
    (setf (aref fe-ext 6) (<< (loadint 3 (subseq in 20)) 7 64))
    (setf (aref fe-ext 7) (<< (loadint 3 (subseq in 23)) 5 64))
    (setf (aref fe-ext 8) (<< (loadint 3 (subseq in 26)) 4 64))
    (setf (aref fe-ext 9) (<< (logand (loadint 3 (subseq in 29)) 8388607) 2 64))
    (fe-combine fe-ext)))

(declaim (ftype (function (field-element) (bytes *)) fe-to-bytes))
(defun fe-to-bytes (h)
  "Serialize field element to byte array."
  (let ((h (fe-copy h)))
    (loop :with q :of-type int32
       := (>> (the int32 (+ (* 19 (aref h 9)) (<< 1 24 64))) 25 64)
       :for i :below +fe-size+
       :do (setf q (>> (+ q (aref h i)) (if (evenp i) 26 25) 32))
       :finally (incf (aref h 0) (* 19 q)))
    (loop :with carry :of-type field-element := (fe)
       :for i :below (1- +fe-size+)
       :do (let ((shift (if (evenp i) 26 25)))
             (setf (aref carry i) (>> (aref h i) shift 32))
             (incf (aref h (1+ i)) (aref carry i))
             (decf (aref h i) (<< (aref carry i) shift 32)))
       :finally (let ((i (1- +fe-size+))
                      (shift 25))
                  (setf (aref carry i) (>> (aref h i) shift 32))
                  (decf (aref h i) (<< (aref carry i) shift 32))))
    (loop
       :with buf :of-type (bytes *)
         := (make-array 32 :element-type '(unsigned-byte 8))
       :with shift :of-type (unsigned-byte 5) := 0
       :with i :of-type int32 := 0
       :for k :below 32
       :for bound :of-type int32 := (if (or (evenp i) (= i (1- +fe-size+))) 26 25)
       :if (> (+ shift 8) bound)
       :do (let* ((rshift (- bound shift))
                  (left (>> (aref h i) shift 32)))
             (if (= rshift 0)
                 (setf shift 0)
                 (let ((right (<< (aref h (1+ i)) rshift 32)))
                   (setf (aref buf k) (mod (logior left right) 256))
                   (setf shift (- 8 rshift))))
             (setf i (1+ i)))
       :else :if (= (+ shift 8) bound)
       :do (progn
             (setf (aref buf k) (mod (>> (aref h i) shift 32) 256))
             (setf i (1+ i))
             (setf shift 0))
       :else
       :do (let ((byte (mod (>> (aref h i) shift 32) 256)))
             (setf (aref buf k) byte)
             (setf shift (+ shift 8)))
       :finally (return buf))))

(declaim (ftype (function (field-element field-element) field-element) fe-mul))
(defun fe-mul (f g)
  "Compute h = f * g using schoolbook multiplication algorithm.
   TODO:
       1. doublings (`fc' values) and reductions (`gc' values) can
          be precomputed as in Adam Langley's Go implementation;
       2. deal with (mod (- i j) +fe-size+) expression warning."
  (loop
     :with h :of-type field-element-ext := (fe-ext)
     :for i :below +fe-size+
     :do (loop
            :with hi :of-type int64 := 0
            :for j :below +fe-size+
            :for k := (mod (- i j) +fe-size+)
            :for fc := (if (and (evenp i) (oddp j)) 2 1)
            :for gc := (if (minusp (- i j)) 19 1)
            :do (incf hi (the int64 (* fc (aref f j) gc (aref g k))))
            :finally (setf (aref h i) hi))
     :finally (return (fe-combine h))))

(declaim (ftype (function (field-element) field-element-ext) fe-square-ext))
(defun fe-square-ext (f)
  "Compute h = f * f, return result in extended field-element format.
   NOTE: same as `fe-mul' implementation, as the only difference in
         h1 = f*g and h2 = f*f is in the number of precomputations,
         which are currently not implemented.
   TODO: see TODO in `fe-mul'."
  (loop
     :with h :of-type field-element-ext := (fe-ext)
     :for i :below +fe-size+
     :do (loop
            :with hi :of-type int64 := 0
            :for j :below +fe-size+
            :for k := (mod (- i j) +fe-size+)
            :for fc1 := (if (and (evenp i) (oddp j)) 2 1)
            :for fc2 := (if (minusp (- i j)) 19 1)
            :do (incf hi (the int64 (* fc1 (aref f j) fc2 (aref f k))))
            :finally (setf (aref h i) hi))
     :finally (return h)))

(declaim (ftype (function (field-element) field-element) fe-square))
(defun fe-square (f)
  "Compute h = f*f."
  (fe-combine (fe-square-ext f)))

(declaim (ftype (function (field-element) field-element) fe-double-square))
(defun fe-double-square (f)
  "Compute h = 2*f*f."
  (loop
     :with square :of-type field-element-ext := (fe-square-ext f)
     :for i :below +fe-size+
     :do (incf (aref square i) (aref square i))
     :finally (return (fe-combine square))))

(declaim (ftype (function (field-element int32) field-element) fe-square-times))
(defun fe-square-times (f k)
  "Compute h = f^(2^k) by squaring f k times."
  (loop
     :with result :of-type field-element := f
     :for i :below k
     :do (setf result (fe-square result))
     :finally (return result)))

(declaim (ftype (function (field-element) (values field-element &optional field-element)) pow22501))
(defun fe-pow22501 (f)
  "Compute a pair of values (f^(2^250 - 3), f^11), a smart trick
   from `curve25519-dalek' implementation, which allows for simpler
   `invert' and `pow22523' implementations by computing two main
   intermediate results."
  (let* ((t0  (fe-square f))               ; 1
         (t1  (fe-square (fe-square t0)))  ; 3
         (t2  (fe-mul f t1))               ; 3,0
         (t3  (fe-mul t0 t2))              ; 3,1,0
         (t4  (fe-square t3))              ; 4,2,1
         (t5  (fe-mul t2 t4))              ; 4,3,2,1,0
         (t6  (fe-square-times t5 5))      ; 9,8,7,6,5
         (t7  (fe-mul t6 t5))              ; 9,8,7,6,5,4,3,2,1,0
         (t8  (fe-square-times t7 10))     ; 19..10
         (t9  (fe-mul t8 t7))              ; 19..0
         (t10 (fe-square-times t9 20))     ; 39..20
         (t11 (fe-mul t10 t9))             ; 39..0
         (t12 (fe-square-times t11 10))    ; 49..10
         (t13 (fe-mul t12 t7))             ; 49..0
         (t14 (fe-square-times t13 50))    ; 99..50
         (t15 (fe-mul t14 t13))            ; 99..0
         (t16 (fe-square-times t15 100))   ; 199..100
         (t17 (fe-mul t16 t15))            ; 199..0
         (t18 (fe-square-times t17 50))    ; 249..50
         (t19 (fe-mul t18 t13)))           ; 249..0
    (values t19 t3)))

(declaim (ftype (function (field-element) field-element) fe-invert))
(defun fe-invert (f)
  "Compute the inverse of field-element."
  (multiple-value-bind (t19 t3) (fe-pow22501 f)
    (fe-mul (fe-square-times t19 5) t3)))

(declaim (ftype (function (field-element) field-element) fe-pow22523))
(defun fe-pow22523 (f)
  "Compute h = f^(2^252 - 3), which is used for group element encoding."
  (let ((t19 (fe-pow22501 f)))
    (fe-mul (fe-square-times t19 2) f)))



;;;-----------------------------------------------------------------------------
;;; Constants

(unless (boundp '+d+)
  (defconstant +d+
    (fe -10913610 13857413 -15372611 6949391 114729 -8787816 -6275908 -3247719 -18696448 -12055116)
    "Constant field element d."))

(unless (boundp '+2d+)
  (defconstant +2d+
    (fe -21827239 -5839606 -30745221 13898782 229458 15978800 -12551817 -6495438 29715968 9444199)
    "Constant field element 2d."))

(unless (boundp '+sqrt-minus-one+)
  (defconstant +sqrt-minus-one+
    (fe -32595792 -7943725 9377950 3500415 12389472 -272473 -25146209 -2005654 326686 11406482)
    "Constant field element sqrt."))

(unless (boundp '+a+)
  (defconstant +a+
    (fe 486662 0 0 0 0 0 0 0 0 0)
    "Constant field element A."))
