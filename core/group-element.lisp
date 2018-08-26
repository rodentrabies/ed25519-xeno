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



(declaim (ftype (function () projective-group-element) ge-zero-projective))
(defun ge-zero-projective ()
  (projective-group-element :x (fe-zero) :y (fe-one) :z (fe-one)))

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

(declaim (ftype (function (projective-group-element) bytes) ge-to-bytes-projective))
(defun ge-to-bytes-projective (p)
  (let* ((recip (fe-invert (projective-ge-z p)))
         (x (fe-mul (projective-ge-x p) recip))
         (y (fe-mul (projective-ge-y p) recip))
         (buf (fe-to-bytes y)))
    (setf (aref buf 31) (logxor (aref buf 31) (<< (fe-negative? x) 7 ))))
  )

(declaim (ftype (function () extended-group-element) ge-zero-extended))
(defun ge-zero-extended ()
  (extended-group-element :x (fe-zero) :y (fe-one) :z (fe-one) :tau (fe-zero)))

(declaim (ftype (function (extended-group-element) completed-group-element) ge-double-extended))
(defun ge-double-extended ()
  )

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
