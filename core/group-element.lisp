(uiop:define-package :ed25519/core/group-element
    (:use :cl :ed25519/core/field-element)
  (:export
   ;; data
   ;; operations
   ;; utils
   ))

(in-package :ed25519/core/group-element)

(declaim (optimize (speed 3) (safety 3)))
