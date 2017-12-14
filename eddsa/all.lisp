(uiop:define-package :ed25519/eddsa/all
    (:nicknames :ed25519)
  (:use :cl :ed25519/core/all)
  (:export
   ;; data types
   #:+public-key-size+
   #:+private-key-size+
   #:+signature-size+
   #:signature
   ;; interface
   #:generate-key
   #:sign
   #:verify))

(in-package :ed25519/eddsa/all)



(defconstant +public-key-size+  32)
(defconstant +private-key-size+ 64)
(defconstant +signature-size+   64)



(defun generate-key ())

(defun sign (privkey message)
  (declare (ignore privkey message)))

(defun verify (pubkey message signature)
  (declare (ignore pubkey message signature)))
