(uiop:define-package :ed25519/core/all
    (:nicknames :ed25519-core)
  (:use :cl)
  (:export
   ;; data
   #:field-element
   ;; operations
   #:fe
   #:fe-zero
   #:fe-one
   #:fe-add
   #:fe-sub))

(in-package :ed25519/core/all)



(defconstant +fe-size+ 10)

(deftype int32 () '(signed-byte 32))

(deftype field-element () `(simple-array int32 (,+fe-size+)))



(declaim (ftype (function () field-element) fe))
(defun fe ()
  "Create a new empty field element."
  (make-array +fe-size+ :element-type 'int32))

(declaim (ftype (function () field-element) fe-zero))
(defun fe-zero ()
  "Create a new 0 field element."
  (make-array +fe-size+ :element-type 'int32 :initial-element 0))

(declaim (ftype (function () field-element) fe-one))
(defun fe-one ()
  "Create a new 1 field element."
  (let ((zero (fe-zero)))
    (setf (aref zero 0) 1)
    zero))

(declaim (ftype (function (field-element field-element) field-element) fe-add))
(defun fe-add (a b)
  (loop
     :with result :of-type field-element := (fe)
     :for i :below +fe-size+
     :do (setf (aref result i) (+ (aref a i) (aref b i)))
     :finally (return result)))

(declaim (ftype (function (field-element field-element) field-element) fe-sub))
(defun fe-sub (a b)
  (loop
     :with result :of-type field-element := (fe)
     :for i :below +fe-size+
     :do (setf (aref result i) (- (aref a i) (aref b i)))
     :finally (return result)))
