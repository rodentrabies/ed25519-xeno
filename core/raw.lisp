(uiop:define-package :ed25519/core/raw
    (:use :cl)
  (:export
   ;; data
   #:int32
   #:int64
   #:bytes
   ;; operations
   #:<<
   #:>>))

(in-package :ed25519/core/raw)

(declaim (optimize speed (safety 0)))



(deftype int32 () '(signed-byte 32))

(deftype int64 () '(signed-byte 63))

(deftype bytes () '(simple-array (unsigned-byte 8) *))


(declaim (ftype (function (int32 (unsigned-byte 6)) int32) shl32))
(defun shl32 (n shift)
  (logand (ash n shift) (1- (ash 1 32))))

(declaim (ftype (function (int32 (unsigned-byte 6)) int32) shr32))
(defun shr32 (n shift)
  (logand (ash n (- shift)) (1- (ash 1 32))))

(declaim (ftype (function (int64 (unsigned-byte 6)) int64) shl64))
(defun shl64 (n shift)
  (logand (ash n shift) (1- (ash 1 62))))

(declaim (ftype (function (int64 (unsigned-byte 6)) int64) shr64))
(defun shr64 (n shift)
  (logand (ash n (- shift)) (1- (ash 1 62))))

(defmacro << (n shift width)
  (ecase width
    (32 `(shl32 ,n ,shift))
    (64 `(shl64 ,n ,shift))))

(defmacro >> (n shift width)
  (ecase width
    (32 `(shr32 ,n ,shift))
    (64 `(shr64 ,n ,shift))))
