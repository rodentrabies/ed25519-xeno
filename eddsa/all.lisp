(uiop:define-package :ed25519/eddsa/all
    (:nicknames :ed25519)
  (:use :cl :ed25519/core/all)
  (:import-from :ironclad)
  (:export
   ;; data
   #:+secretkey-size+
   #:+publickey-size+
   #:+signature-size+
   ;; ;; types
   ;; #:secretkey
   ;; #:publickey
   ;; #:signature
   ;; interface
   #:generate-keys
   #:sign
   #:verify))

(in-package :ed25519/eddsa/all)


(defconstant +secretkey-size+ 64)
(defconstant +publickey-size+ 32)
(defconstant +signature-size+ 64)

(deftype secretkey () `(simple-array (unsigned-byte 8) (,+secretkey-size+)))
(deftype publickey () `(simple-array (unsigned-byte 8) (,+publickey-size+)))
(deftype signature () `(simple-array (unsigned-byte 8) (,+signature-size+)))

(deftype message () '(simple-array (unsigned-byte 8) *))

(declaim (ftype (function (fixnum) (simple-array (unsigned-byte 8) *)) random-bytes))
(defun random-bytes (len)
  "Read and pack `len' random bytes from `/dev/urandom' to simple array."
  (let ((buffer (make-array len :element-type '(unsigned-byte 8))))
    (with-open-file (urandom "/dev/urandom" :element-type '(unsigned-byte 8))
      (read-sequence buffer urandom))
    buffer))

(declaim (ftype (function () (values secretkey &optional publickey)) generate-keys))
(defun generate-keys ()
  "Generate random secret key `secret' and compute public key as SHA512(`secret')*G'."
  (let* ((secret (random-bytes +secretkey-size+))
         (hash (ironclad:make-digest 'ironclad:sha512))
         (digest (ironclad:produce-digest (ironclad:update-digest hash secret))))
    (setf (aref digest 0) (logand (aref digest 0) 248))
    (setf (aref digest 31) (logior (logand (aref digest 31) 127) 64))
    (values secretkey (ge-to-bytes (ge-scalar-mul-base digest)))))

(defun sign (secretkey message)
  (declare (ignore secretkey message)))

(defun verify (publickey message signature)
  (declare (ignore publickey message signature)))
