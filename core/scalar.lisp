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

;;; Scalars are integer values represented by 32-byte arrays in Galois
;;; field GF(l) where l = 2^252 + 27742317777372353535851937790883648493.

(defconstant +scalar-size+ 32)

(deftype scalar () `(bytes (,+scalar-size+)))

(declaim (ftype (function ((bytes (64))) scalar) sc-from-bytes))
(defun sc-from-bytes (bytes)
  "Compute r = b mod l."
  (macrolet ((%>> (n shift) `(>> ,n ,shift 64))
             (%<< (n shift) `(<< ,n ,shift 64)))
    (let ((+p+ 2097151)
          (+c+
           (make-array
            6 :element-type 'int32
            :initial-contents '(666643 470296 654183 997805 136657 683901)))
          (c (make-array 17 :element-type 'int64))
          (s (make-array 24 :element-type 'int64))
          (r (make-array +scalar-size+ :element-type '(unsigned-byte 8))))
      (declare (type (simple-array int32) +c+))
      (setf (aref s  0) (logand +p+      (loadint 3         bytes)))
      (setf (aref s  1) (logand +p+ (%>> (loadint 4 (subseq bytes  2)) 5)))
      (setf (aref s  2) (logand +p+ (%>> (loadint 3 (subseq bytes  5)) 2)))
      (setf (aref s  3) (logand +p+ (%>> (loadint 4 (subseq bytes  7)) 7)))
      (setf (aref s  4) (logand +p+ (%>> (loadint 4 (subseq bytes 10)) 4)))
      (setf (aref s  5) (logand +p+ (%>> (loadint 3 (subseq bytes 13)) 1)))
      (setf (aref s  6) (logand +p+ (%>> (loadint 4 (subseq bytes 15)) 6)))
      (setf (aref s  7) (logand +p+ (%>> (loadint 3 (subseq bytes 18)) 3)))
      (setf (aref s  8) (logand +p+      (loadint 3 (subseq bytes 21))))
      (setf (aref s  9) (logand +p+ (%>> (loadint 4 (subseq bytes 23)) 5)))
      (setf (aref s 10) (logand +p+ (%>> (loadint 3 (subseq bytes 26)) 2)))
      (setf (aref s 11) (logand +p+ (%>> (loadint 4 (subseq bytes 28)) 7)))
      (setf (aref s 12) (logand +p+ (%>> (loadint 4 (subseq bytes 31)) 4)))
      (setf (aref s 13) (logand +p+ (%>> (loadint 3 (subseq bytes 34)) 1)))
      (setf (aref s 14) (logand +p+ (%>> (loadint 4 (subseq bytes 36)) 6)))
      (setf (aref s 15) (logand +p+ (%>> (loadint 3 (subseq bytes 39)) 3)))
      (setf (aref s 16) (logand +p+      (loadint 3 (subseq bytes 42))))
      (setf (aref s 17) (logand +p+ (%>> (loadint 4 (subseq bytes 44)) 5)))
      (setf (aref s 18) (logand +p+ (%>> (loadint 3 (subseq bytes 47)) 2)))
      (setf (aref s 19) (logand +p+ (%>> (loadint 4 (subseq bytes 49)) 7)))
      (setf (aref s 20) (logand +p+ (%>> (loadint 4 (subseq bytes 52)) 4)))
      (setf (aref s 21) (logand +p+ (%>> (loadint 3 (subseq bytes 55)) 1)))
      (setf (aref s 22) (logand +p+ (%>> (loadint 4 (subseq bytes 57)) 6)))
      (setf (aref s 23)             (%>> (loadint 4 (subseq bytes 60)) 3))
      (loop
         :for k :from 23 :downto 18
         :do (loop
                :for i :from 0 :to 5
                :for c := (+ i k -12) 
                :do (case i
                      ((3 5) (decf (aref s c)
                                   (* (the int32 (aref s k))
                                      (the int32 (aref +c+ i)))))
                      (t (incf (aref s c)
                               (* (the int32 (aref s k))
                                  (the int32 (aref +c+ i))))))
                :finally (setf (aref s k) 0)))
      (loop
         :for i :from 6 :to 16 :by 2
         :do (progn
               (setf (aref c i) (%>> (+ (aref s i) (%<< 1 20)) 21))
               (incf (aref s (1+ i)) (aref c i))
               (decf (aref s i) (%<< (aref c i) 21))))
      (loop
         :for i :from 7 :to 15 :by 2
         :do (progn
               (setf (aref c i) (%>> (+ (aref s i) (%<< 1 20)) 21))
               (incf (aref s (1+ i)) (aref c i))
               (decf (aref s i) (%<< (aref c i) 21))))
      (loop
         :for k :from 17 :downto 12
         :do (loop
                :for i :from 0 :to 5
                :for c := (+ i k -12) 
                :do (case i
                      ((3 5) (decf (aref s c)
                                   (* (the int32 (aref s k))
                                      (the int32 (aref +c+ i)))))
                      (t (incf (aref s c)
                               (* (the int32 (aref s k))
                                  (the int32 (aref +c+ i))))))
                :finally (setf (aref s k) 0)))
      (loop
         :for i :from 0 :to 10 :by 2
         :do (progn
               (setf (aref c i) (%>> (+ (aref s i) (%<< 1 20)) 21))
               (incf (aref s (1+ i)) (aref c i))
               (decf (aref s i) (%<< (aref c i) 21))))
      (loop
         :for i :from 1 :to 11 :by 2
         :do (progn
               (setf (aref c i) (%>> (+ (aref s i) (%<< 1 20)) 21))
               (incf (aref s (1+ i)) (aref c i))
               (decf (aref s i) (%<< (aref c i) 21))))
      (loop
         :for i :from 0 :to 5
         :do (case i
               ((3 5) (decf (aref s i)
                            (* (the int32 (aref s 12))
                               (the int32 (aref +c+ i)))))
               (t (incf (aref s i)
                        (* (the int32 (aref s 12))
                           (the int32 (aref +c+ i))))))
         :finally (setf (aref s 12) 0))
      (loop
         :for i :from 0 :to 11
         :do (progn
               (setf (aref c i) (%>> (aref s i) 21))
               (incf (aref s (1+ i)) (aref c i))
               (decf (aref s i) (%<< (aref c i) 21))))
      (loop
         :for i :from 0 :to 5
         :do (case i
               ((3 5) (decf (aref s i)
                            (* (the int32 (aref s 12))
                               (the int32 (aref +c+ i)))))
               (t (incf (aref s i)
                        (* (the int32 (aref s 12))
                           (the int32 (aref +c+ i))))))
         :finally (setf (aref s 12) 0))
      (loop
         :for i :from 0 :to 10
         :do (progn
               (setf (aref c i) (%>> (aref s i) 21))
               (incf (aref s (1+ i)) (aref c i))
               (decf (aref s i) (%<< (aref c i) 21))))
      (setf (aref r 0 )         (%>> (aref s 0)   0))
      (setf (aref r 1 )         (%>> (aref s 0)   8))
      (setf (aref r 2 ) (logior (%>> (aref s 0)  16) (%<< (aref s 1) 5)))
      (setf (aref r 3 )         (%>> (aref s 1)   3))
      (setf (aref r 4 )         (%>> (aref s 1)  11))
      (setf (aref r 5 ) (logior (%>> (aref s 1)  19) (%<< (aref s 2) 2)))
      (setf (aref r 6 )         (%>> (aref s 2)   6))
      (setf (aref r 7 ) (logior (%>> (aref s 2)  14) (%<< (aref s 3) 7)))
      (setf (aref r 8 )         (%>> (aref s 3)   1))
      (setf (aref r 9 )         (%>> (aref s 3)   9))
      (setf (aref r 10) (logior (%>> (aref s 3)  17) (%<< (aref s 4) 4)))
      (setf (aref r 11)         (%>> (aref s 4)   4))
      (setf (aref r 12)         (%>> (aref s 4)  12))
      (setf (aref r 13) (logior (%>> (aref s 4)  20) (%<< (aref s 5) 1)))
      (setf (aref r 14)         (%>> (aref s 5)   7))
      (setf (aref r 15) (logior (%>> (aref s 5)  15) (%<< (aref s 6) 6)))
      (setf (aref r 16)         (%>> (aref s 6)   2))
      (setf (aref r 17)         (%>> (aref s 6)  10))
      (setf (aref r 18) (logior (%>> (aref s 6)  18) (%<< (aref s 7) 3)))
      (setf (aref r 19)         (%>> (aref s 7)   5))
      (setf (aref r 20)         (%>> (aref s 7)  13))
      (setf (aref r 21)         (%>> (aref s 8)   0))
      (setf (aref r 22)         (%>> (aref s 8)   8))
      (setf (aref r 23) (logior (%>> (aref s 8)  16) (%<< (aref s 9) 5)))
      (setf (aref r 24)         (%>> (aref s 9)   3))
      (setf (aref r 25)         (%>> (aref s 9)  11))
      (setf (aref r 26) (logior (%>> (aref s 9)  19) (%<< (aref s 10) 2)))
      (setf (aref r 27)         (%>> (aref s 10)  6))
      (setf (aref r 28) (logior (%>> (aref s 10) 14) (%<< (aref s 11) 7)))
      (setf (aref r 29)         (%>> (aref s 11)  1))
      (setf (aref r 30)         (%>> (aref s 11)  9))
      (setf (aref r 31)         (%>> (aref s 11) 17))
      r)))

(declaim (ftype (function (scalar scalar scalar) scalar) sc-mul-add))
(defun sc-mul-add (a b c)
  (declare (ignore a b c))
  (let ((s (make-array +scalar-size+ :element-type '(unsigned-byte 8))))
    (declare (ignore s))
    (error "not implemented")))
