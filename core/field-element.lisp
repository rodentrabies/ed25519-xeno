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
   #:fe-from-bytes
   #:fe-to-bytes))

(in-package :ed25519/core/field-element)

(declaim (optimize (speed 3) (safety 3)))



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


(declaim (ftype (function () field-element) fe))
(defun fe ()
  "Create a new empty field element."
  (make-array +fe-size+ :element-type 'int32))

(declaim (ftype (function () field-element-ext) fe-ext))
(defun fe-ext ()
  "Create a new empty extended field element."
  (make-array +fe-size+ :element-type 'int64))

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

(declaim (ftype (function (field-element-ext) field-element) fe-combine))
(defun fe-combine (h)
  "Combine 64-bit coefficients resulting from multiplication of field elements
   into 32-bit representation by narrowing the range of each coefficient."
  (print h)
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

(declaim (ftype (function (bytes) field-element) fe-from-bytes))
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

(declaim (ftype (function (field-element) bytes) fe-to-bytes))
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
       :with buf :of-type bytes := (make-array 32 :element-type '(unsigned-byte 8))
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
