(uiop:define-package :ed25519/core/scalar
    (:use :cl
          :ed25519/core/low-level
          :ed25519/core/field-element)
  (:export
   ;; data
   #:scalar
   ;; operations
   #:sc-mul-add
   ;; utils
   #:sc-from-bytes))

(in-package :ed25519/core/scalar)

(declaim (optimize (speed 3) (safety 3)))

;;; Scalars are integer values in Galois field
;;;   GF(2^252 + 27742317777372353535851937790883648493),
;;; represented by 32-byte arrays.

(defconstant +scalar-size+ 32)

(deftype scalar () `(bytes (,+scalar-size+)))

(declaim (ftype (function ((bytes (64))) scalar) sc-from-bytes))
(defun sc-from-bytes (bytes)
  (declare (ignore bytes))
  (let ((s (make-array +scalar-size+ :element-type '(unsigned-byte 8))))
    (declare (ignore s))
    (error "not implemented")))

(declaim (ftype (function (scalar scalar scalar) scalar) sc-mul-add))
(defun sc-mul-add (a b c)
  (declare (ignore a b c))
  (let ((s (make-array +scalar-size+ :element-type '(unsigned-byte 8))))
    (declare (ignore s))
    (error "not implemented")))
