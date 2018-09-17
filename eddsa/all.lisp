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

(defun sha512 (&rest arrays)
  (ironclad:produce-digest
   (ironclad:update-digest
    (ironclad:make-digest 'ironclad:sha512)
    (apply #'concatenate '(vector (unsigned-byte 8)) arrays))))

(declaim (ftype (function (fixnum) (simple-array (unsigned-byte 8) *))
                random-bytes))
(defun random-bytes (len)
  "Read and pack `len' random bytes from `/dev/urandom' to simple array."
  (let ((buffer (make-array len :element-type '(unsigned-byte 8))))
    (with-open-file (urandom "/dev/urandom" :element-type '(unsigned-byte 8))
      (read-sequence buffer urandom))
    buffer))

(declaim (ftype (function () (values secretkey &optional publickey))
                generate-keys))
(defun generate-keys ()
  "Generate random secret key `secret' and compute public key
   as SHA512(`secret')*G'."
  (let* ((secret (random-bytes +secretkey-size+))
         (digest (sha512 secret)))
    (setf (aref digest 0) (logand (aref digest 0) 248))
    (setf (aref digest 31) (logior (logand (aref digest 31) 127) 64))
    (values secret (ge-to-bytes-extended (ge-scalar-mul-base digest)))))

(declaim (ftype (function (secretkey message) signature) sign))
(defun sign (secretkey message)
  "Sign a given `message' with a given `secretkey'."
  (let* ((key-digest (sha512 (subseq secretkey 0 32)))
         (expanded-key (subseq key-digest 0 32))
         (message-digest (sha512 (subseq key-digest 32) message))
         (message-sc (sc-from-bytes message-digest))
         (r (ge-to-bytes-extended (ge-scalar-mul-base message-sc)))
         (hram-digest (sha512 r (subseq secretkey 32) message))
         (hram-sc (sc-from-bytes hram-digest)))
    (setf (aref expanded-key 0) (logand (aref expanded-key 0) 248))
    (setf (aref expanded-key 31) (logand (aref expanded-key 31) 63))
    (setf (aref expanded-key 31) (logior (aref expanded-key 31) 64))
    (let ((s (sc-mul-add hram-sc expanded-key message-sc)))
      (concatenate '(vector (unsigned-byte 8)) r s))))

(declaim (ftype (function (publickey message signature) boolean) verify))
(defun verify (publickey message signature)
  "Verify a `signature' for a given `message' with respect to `publickey'."
  (let ((a (ge-from-bytes publickey)))
    (when (and a (not (zerop (logand (aref signature 63) 224))))
      (setf (extended-ge-x a) (fe-neg (extended-ge-x a)))
      (setf (extended-ge-tau a) (fe-neg (extended-ge-tau a)))
      (let* ((e-bytes (subseq signature 0 32))
             (hram-digest (sha512 e-bytes publickey message))
             (hram-sc (sc-from-bytes hram-digest))
             (r (ge-double-scalar-mul-vartime hram-sc a (subseq signature 32)))
             (r-bytes (ge-to-bytes-projective r)))
        (and (= (length e-bytes) (length r-bytes))
             (zerop
              (reduce  ; constant time comparison of byte arrays 
               (lambda (v a.b) (logior v (logxor (car a.b) (cdr a.b))))
               (map 'list #'cons e-bytes r-bytes)
               :initial-value 0)))))))
