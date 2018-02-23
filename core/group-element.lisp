(uiop:define-package :ed25519/core/group-element
    (:use :cl :ed25519/core/field-element)
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
  (x (fe-zero) :type field-element)
  (y (fe-zero) :type field-element)
  (z (fe-zero) :type field-element)
  (t (fe-zero) :type field-element))

(defstruct (completed-group-element
             (:constructor completed-group-element)
             (:conc-name completed-ge-))
  "Completed group element - pair of tuples ((X, Y), (Z, T))
   where curve point (x, y) = (X/Z, Y/T)."
  (x (fe-zero) :type field-element)
  (y (fe-zero) :type field-element)
  (z (fe-zero) :type field-element)
  (t (fe-zero) :type field-element))

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
