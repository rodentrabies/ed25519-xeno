(uiop:define-package :ed25519/core/field-element
    (:use :cl :ed25519/core/raw)
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

(declaim (optimize speed (safety 0)))



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
  (let ((c (fe-ext)))
    (declare (type field-element-ext c))
    (setf (aref c 9) (>> (+ (aref c 9) (<< 1 24 64)) 25 64))
    (decf (aref h 9) (<< (aref c 9) 25 64))
    (incf (aref h 0) (the int64 (* (aref c 9) 19)))
    (loop
       :for i :below (1- +fe-size+)
       :do (let* ((in (if (evenp i) 25 24))
                  (out (1+ in)))
             (setf (aref c i) (>> (+ (aref h i) (<< 1 in 64)) out 64))
             (decf (aref h i) (<< (aref c i) out 64))
             (incf (aref h (1+ i)) (aref c i))))
    (loop
       :with result :of-type field-element := (fe)
       :for i :below +fe-size+
       :do (setf (aref result i) (the int32 (aref h i)))
       :finally (return result))))
