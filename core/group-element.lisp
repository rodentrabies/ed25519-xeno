(uiop:define-package :ed25519/core/group-element
    (:use :cl
          :ed25519/core/field-element
          :ed25519/core/low-level)
  (:export
   ;; data
   #:projective-group-element
   #:extended-group-element
   #:completed-group-element
   #:precomputed-group-element
   #:cached-group-element
   ;; operations
   ;; utils
   ))

(in-package :ed25519/core/group-element)

(declaim (optimize (speed 3) (safety 3)))

;;; Group consists of the points of elliptic curve
;;;   -x^2 + y^2 = 1 - (121665/121666) * x^2 * y^2

(defstruct (projective-group-element
             (:constructor projective-group-element)
             (:conc-name projective-ge-))
  "Projective group element - triple (X, Y, Z)
   where curve point (x, y) = (X/Z, Y/Z)."
  (x (fe-zero) :type field-element)
  (y (fe-zero) :type field-element)
  (z (fe-zero) :type field-element))

(defstruct (extended-group-element
             (:constructor extended-group-element)
             (:conc-name extended-ge-))
  "Extended group element - quadruple (X, Y, Z, T)
   where curve point (x, y) = (X/Z, Y/Z) and XY = ZT."
  (x   (fe-zero) :type field-element)
  (y   (fe-zero) :type field-element)
  (z   (fe-zero) :type field-element)
  (tau (fe-zero) :type field-element))

(defstruct (completed-group-element
             (:constructor completed-group-element)
             (:conc-name completed-ge-))
  "Completed group element - pair of tuples ((X, Y), (Z, T))
   where curve point (x, y) = (X/Z, Y/T)."
  (x   (fe-zero) :type field-element)
  (y   (fe-zero) :type field-element)
  (z   (fe-zero) :type field-element)
  (tau (fe-zero) :type field-element))

(defstruct (precomputed-group-element
             (:constructor precomputed-group-element)
             (:conc-name precomputed-ge-))
  "Precomputed group element - triple (y+x, y-x, 2dxy)."
  (yplusx  (fe-zero) :type field-element)
  (yminusx (fe-zero) :type field-element)
  (xy2d    (fe-zero) :type field-element))

(defstruct (cached-group-element
             (:constructor cached-group-element)
             (:conc-name cached-ge-))
  "Cached group element - quadruple (Y+X, Y-X, Z, 2dT)."
  (yplusx  (fe-zero) :type field-element)
  (yminusx (fe-zero) :type field-element)
  (z       (fe-zero) :type field-element)
  (t2d     (fe-zero) :type field-element))



;;; Zero-value constructors
(declaim (ftype (function () projective-group-element) ge-zero-projective))
(defun ge-zero-projective ()
  (projective-group-element :x (fe-zero) :y (fe-one) :z (fe-one)))

(declaim (ftype (function () extended-group-element) ge-zero-extended))
(defun ge-zero-extended ()
  (extended-group-element :x (fe-zero) :y (fe-one) :z (fe-one) :tau (fe-zero)))

(declaim (ftype (function () precomputed-group-element) ge-zero-precomputed))
(defun ge-zero-precomputed ()
  (precomputed-group-element :yplusx (fe-one) :yminusx (fe-one) :xy2d (fe-zero)))




;;; Doubling operations
(declaim (ftype (function (extended-group-element) completed-group-element) ge-double-extended))
(defun ge-double-extended (p)
  (ge-double-projective (ge-extended-to-projective p)))

(declaim (ftype (function (projective-group-element) completed-group-element) ge-double-projective))
(defun ge-double-projective (p)
  (let* ((x (fe-square (projective-ge-x p)))
         (z (fe-square (projective-ge-y p)))
         (tau (fe-double-square (projective-ge-z p)))
         (y (fe-add (projective-ge-x p) (projective-ge-y p)))
         (t0 (fe-square y))
         (y (fe-add z x))
         (z (fe-sub z x))
         (x (fe-sub t0 y))
         (tau (fe-sub tau z)))
    (completed-group-element :x x :y y :z z :tau tau)))



;;; Cross-representation conversions
(declaim (ftype (function (extended-group-element) cached-group-element) ge-extended-to-cached))
(defun ge-extended-to-cached (p)
  (let* ((yplusx (fe-add (extended-ge-y p) (extended-ge-x p)))
         (yminusx (fe-sub (extended-ge-y p) (extended-ge-x p)))
         (z (fe-copy (extended-ge-z p)))
         (t2d (fe-mul (extended-ge-tau p) +2d+)))
    (cached-group-element :yplusx yplusx :yminusx yminusx :z z :t2d t2d)))

(declaim (ftype (function (extended-group-element) projective-group-element) ge-extended-to-projective))
(defun ge-extended-to-projective (p)
  (projective-group-element
   :x (fe-copy (extended-ge-x p))
   :y (fe-copy (extended-ge-y p))
   :z (fe-copy (extended-ge-z p))))

(declaim (ftype (function (completed-group-element) projective-group-element) ge-completed-to-projective))
(defun ge-completed-to-projective (p)
  (let ((x (fe-mul (completed-ge-x p) (completed-ge-tau p)))
        (y (fe-mul (completed-ge-y p) (completed-ge-z p)))
        (z (fe-mul (completed-ge-z p) (completed-ge-tau p))))
    (projective-group-element :x x :y y :z z)))

(declaim (ftype (function (completed-group-element) extended-group-element) ge-completed-to-extended))
(defun ge-completed-to-extended (p)
  (let ((x (fe-mul (completed-ge-x p) (completed-ge-tau p)))
        (y (fe-mul (completed-ge-y p) (completed-ge-z p)))
        (z (fe-mul (completed-ge-z p) (completed-ge-tau p)))
        (tau (fe-mul (completed-ge-x p) (completed-ge-y p))))
    (extended-group-element :x x :y y :z z :tau tau)))



;;; Conversions to bytes
(declaim (ftype (function (projective-group-element) bytes) ge-to-bytes-projective))
(defun ge-to-bytes-projective (p)
  (let* ((recip (fe-invert (projective-ge-z p)))
         (x (fe-mul (projective-ge-x p) recip))
         (y (fe-mul (projective-ge-y p) recip))
         (buffer (fe-to-bytes y)))
    (setf (aref buffer 31) (logxor (aref buffer 31) (ash (fe-negative? x) 7)))
    buffer))

(declaim (ftype (function (extended-group-element) bytes) ge-to-bytes-extended))
(defun ge-to-bytes-extended (p)
  (let* ((recip (fe-invert (extended-ge-z p)))
         (x (fe-mul (extended-ge-x p) recip))
         (y (fe-mul (extended-ge-y p) recip))
         (buffer (fe-to-bytes y)))
    (setf (aref buffer 31) (logxor (aref buffer 31) (ash (fe-negative? x) 7)))
    buffer))




;;; Low-level operations
(declaim (ftype (function (extended-group-element cached-group-element) completed-group-element) ge-add))
(defun ge-add (p q)
  (let* ((x (fe-add (extended-ge-y p) (extended-ge-x p)))
         (y (fe-sub (extended-ge-y p) (extended-ge-x p)))
         (z (fe-mul x (cached-ge-yplusx q)))
         (y (fe-mul y (cached-ge-yminusx q)))
         (tau (fe-mul (cached-ge-t2d q) (extended-ge-tau p)))
         (x (fe-mul (extended-ge-z p) (cached-ge-z q)))
         (t0 (fe-add x x)))
    (completed-group-element
     :x (fe-sub z y)
     :y (fe-add z y)
     :z (fe-add t0 tau)
     :tau (fe-sub t0 tau))))
