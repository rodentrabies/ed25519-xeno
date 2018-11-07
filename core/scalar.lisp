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

;; Local shift macro versions for 64-bit integers.
(defmacro >>> (n shift) `(>> ,n ,shift 64))
(defmacro <<< (n shift) `(<< ,n ,shift 64))

(defconstant +pmask+ 2097151)

(unless (boundp '+clist+)
  (defconstant +clist+ '(666643 470296 654183 997805 136657 683901)))

(declaim (ftype (function ((bytes (64))) scalar) sc-from-bytes))
(defun sc-from-bytes (sbytes)
  "Compute r = s mod l."
  (let ((s     (make-array 24 :element-type 'int64))
        (r     (make-array +scalar-size+ :element-type '(unsigned-byte 8)))
        (carry (make-array 17 :element-type 'int64)))
    ;; load bytes
    (setf (aref s  0) (logand +pmask+      (loadint 3         sbytes)))
    (setf (aref s  1) (logand +pmask+ (>>> (loadint 4 (subseq sbytes  2)) 5)))
    (setf (aref s  2) (logand +pmask+ (>>> (loadint 3 (subseq sbytes  5)) 2)))
    (setf (aref s  3) (logand +pmask+ (>>> (loadint 4 (subseq sbytes  7)) 7)))
    (setf (aref s  4) (logand +pmask+ (>>> (loadint 4 (subseq sbytes 10)) 4)))
    (setf (aref s  5) (logand +pmask+ (>>> (loadint 3 (subseq sbytes 13)) 1)))
    (setf (aref s  6) (logand +pmask+ (>>> (loadint 4 (subseq sbytes 15)) 6)))
    (setf (aref s  7) (logand +pmask+ (>>> (loadint 3 (subseq sbytes 18)) 3)))
    (setf (aref s  8) (logand +pmask+      (loadint 3 (subseq sbytes 21))))
    (setf (aref s  9) (logand +pmask+ (>>> (loadint 4 (subseq sbytes 23)) 5)))
    (setf (aref s 10) (logand +pmask+ (>>> (loadint 3 (subseq sbytes 26)) 2)))
    (setf (aref s 11) (logand +pmask+ (>>> (loadint 4 (subseq sbytes 28)) 7)))
    (setf (aref s 12) (logand +pmask+ (>>> (loadint 4 (subseq sbytes 31)) 4)))
    (setf (aref s 13) (logand +pmask+ (>>> (loadint 3 (subseq sbytes 34)) 1)))
    (setf (aref s 14) (logand +pmask+ (>>> (loadint 4 (subseq sbytes 36)) 6)))
    (setf (aref s 15) (logand +pmask+ (>>> (loadint 3 (subseq sbytes 39)) 3)))
    (setf (aref s 16) (logand +pmask+      (loadint 3 (subseq sbytes 42))))
    (setf (aref s 17) (logand +pmask+ (>>> (loadint 4 (subseq sbytes 44)) 5)))
    (setf (aref s 18) (logand +pmask+ (>>> (loadint 3 (subseq sbytes 47)) 2)))
    (setf (aref s 19) (logand +pmask+ (>>> (loadint 4 (subseq sbytes 49)) 7)))
    (setf (aref s 20) (logand +pmask+ (>>> (loadint 4 (subseq sbytes 52)) 4)))
    (setf (aref s 21) (logand +pmask+ (>>> (loadint 3 (subseq sbytes 55)) 1)))
    (setf (aref s 22) (logand +pmask+ (>>> (loadint 4 (subseq sbytes 57)) 6)))
    (setf (aref s 23)                 (>>> (loadint 4 (subseq sbytes 60)) 3))
    ;; reduce modulo l and encode
    (sc-reduce s carry)
    (sc-encode s r)
    r))

(declaim (ftype (function (scalar scalar scalar) scalar) sc-mul-add))
(defun sc-mul-add (abytes bbytes cbytes)
  "Compute s = (ab + c) mod l."
  (let ((a     (make-array 12 :element-type 'int64))
        (b     (make-array 12 :element-type 'int64))
        (c     (make-array 12 :element-type 'int64))
        (s     (make-array 24 :element-type 'int64))
        (r     (make-array +scalar-size+ :element-type '(unsigned-byte 8)))
        (carry (make-array 23 :element-type 'int64)))
    ;; decode scalars from byte arrays
    (setf (aref a  0) (logand +pmask+      (loadint 3         abytes)))
    (setf (aref a  1) (logand +pmask+ (>>> (loadint 4 (subseq abytes  2)) 5)))
    (setf (aref a  2) (logand +pmask+ (>>> (loadint 3 (subseq abytes  5)) 2)))
    (setf (aref a  3) (logand +pmask+ (>>> (loadint 4 (subseq abytes  7)) 7)))
    (setf (aref a  4) (logand +pmask+ (>>> (loadint 4 (subseq abytes 10)) 4)))
    (setf (aref a  5) (logand +pmask+ (>>> (loadint 3 (subseq abytes 13)) 1)))
    (setf (aref a  6) (logand +pmask+ (>>> (loadint 4 (subseq abytes 15)) 6)))
    (setf (aref a  7) (logand +pmask+ (>>> (loadint 3 (subseq abytes 18)) 3)))
    (setf (aref a  8) (logand +pmask+      (loadint 3 (subseq abytes 21))))
    (setf (aref a  9) (logand +pmask+ (>>> (loadint 4 (subseq abytes 23)) 5)))
    (setf (aref a 10) (logand +pmask+ (>>> (loadint 3 (subseq abytes 26)) 2)))
    (setf (aref a 11)                 (>>> (loadint 4 (subseq abytes 28)) 7))
    (setf (aref b  0) (logand +pmask+      (loadint 3         bbytes)))
    (setf (aref b  1) (logand +pmask+ (>>> (loadint 4 (subseq bbytes  2)) 5)))
    (setf (aref b  2) (logand +pmask+ (>>> (loadint 3 (subseq bbytes  5)) 2)))
    (setf (aref b  3) (logand +pmask+ (>>> (loadint 4 (subseq bbytes  7)) 7)))
    (setf (aref b  4) (logand +pmask+ (>>> (loadint 4 (subseq bbytes 10)) 4)))
    (setf (aref b  5) (logand +pmask+ (>>> (loadint 3 (subseq bbytes 13)) 1)))
    (setf (aref b  6) (logand +pmask+ (>>> (loadint 4 (subseq bbytes 15)) 6)))
    (setf (aref b  7) (logand +pmask+ (>>> (loadint 3 (subseq bbytes 18)) 3)))
    (setf (aref b  8) (logand +pmask+      (loadint 3 (subseq bbytes 21))))
    (setf (aref b  9) (logand +pmask+ (>>> (loadint 4 (subseq bbytes 23)) 5)))
    (setf (aref b 10) (logand +pmask+ (>>> (loadint 3 (subseq bbytes 26)) 2)))
    (setf (aref b 11)                 (>>> (loadint 4 (subseq bbytes 28)) 7))
    (setf (aref c  0) (logand +pmask+      (loadint 3         cbytes)))
    (setf (aref c  1) (logand +pmask+ (>>> (loadint 4 (subseq cbytes  2)) 5)))
    (setf (aref c  2) (logand +pmask+ (>>> (loadint 3 (subseq cbytes  5)) 2)))
    (setf (aref c  3) (logand +pmask+ (>>> (loadint 4 (subseq cbytes  7)) 7)))
    (setf (aref c  4) (logand +pmask+ (>>> (loadint 4 (subseq cbytes 10)) 4)))
    (setf (aref c  5) (logand +pmask+ (>>> (loadint 3 (subseq cbytes 13)) 1)))
    (setf (aref c  6) (logand +pmask+ (>>> (loadint 4 (subseq cbytes 15)) 6)))
    (setf (aref c  7) (logand +pmask+ (>>> (loadint 3 (subseq cbytes 18)) 3)))
    (setf (aref c  8) (logand +pmask+      (loadint 3 (subseq cbytes 21))))
    (setf (aref c  9) (logand +pmask+ (>>> (loadint 4 (subseq cbytes 23)) 5)))
    (setf (aref c 10) (logand +pmask+ (>>> (loadint 3 (subseq cbytes 26)) 2)))
    (setf (aref c 11)                 (>>> (loadint 4 (subseq cbytes 28)) 7))
    ;; perform multiplication/addition
    (setf (aref s 0) (+ (aref c 0) (* (aref a 0) (aref b 0))))
    (setf (aref s 1) (+ (aref c 1) (* (aref a 0) (aref b 1))
                        (* (aref a 1) (aref b 0))))
    (setf (aref s 2) (+ (aref c 2) (* (aref a 0) (aref b 2))
                        (* (aref a 1) (aref b 1)) (* (aref a 2) (aref b 0))))
    (setf (aref s 3) (+ (aref c 3) (* (aref a 0) (aref b 3))
                        (* (aref a 1) (aref b 2)) (* (aref a 2) (aref b 1))
                        (* (aref a 3) (aref b 0))))
    (setf (aref s 4) (+ (aref c 4) (* (aref a 0) (aref b 4))
                        (* (aref a 1) (aref b 3)) (* (aref a 2) (aref b 2))
                        (* (aref a 3) (aref b 1)) (* (aref a 4) (aref b 0))))
    (setf (aref s 5) (+ (aref c 5) (* (aref a 0) (aref b 5))
                        (* (aref a 1) (aref b 4)) (* (aref a 2) (aref b 3))
                        (* (aref a 3) (aref b 2)) (* (aref a 4) (aref b 1))
                        (* (aref a 5) (aref b 0))))
    (setf (aref s 6) (+ (aref c 6) (* (aref a 0) (aref b 6))
                        (* (aref a 1) (aref b 5)) (* (aref a 2) (aref b 4))
                        (* (aref a 3) (aref b 3)) (* (aref a 4) (aref b 2))
                        (* (aref a 5) (aref b 1)) (* (aref a 6) (aref b 0))))
    (setf (aref s 7) (+ (aref c 7) (* (aref a 0) (aref b 7))
                        (* (aref a 1) (aref b 6)) (* (aref a 2) (aref b 5))
                        (* (aref a 3) (aref b 4)) (* (aref a 4) (aref b 3))
                        (* (aref a 5) (aref b 2)) (* (aref a 6) (aref b 1))
                        (* (aref a 7) (aref b 0))))
    (setf (aref s 8) (+ (aref c 8) (* (aref a 0) (aref b 8))
                        (* (aref a 1) (aref b 7)) (* (aref a 2) (aref b 6))
                        (* (aref a 3) (aref b 5)) (* (aref a 4) (aref b 4))
                        (* (aref a 5) (aref b 3)) (* (aref a 6) (aref b 2))
                        (* (aref a 7) (aref b 1)) (* (aref a 8) (aref b 0))))
    (setf (aref s 9) (+ (aref c 9) (* (aref a 0) (aref b 9))
                        (* (aref a 1) (aref b 8)) (* (aref a 2) (aref b 7))
                        (* (aref a 3) (aref b 6)) (* (aref a 4) (aref b 5))
                        (* (aref a 5) (aref b 4)) (* (aref a 6) (aref b 3))
                        (* (aref a 7) (aref b 2)) (* (aref a 8) (aref b 1))
                        (* (aref a 9) (aref b 0))))
    (setf (aref s 10) (+ (aref c 10) (* (aref a 0) (aref b 10))
                         (* (aref a 1) (aref b 9)) (* (aref a 2) (aref b 8))
                         (* (aref a 3) (aref b 7)) (* (aref a 4) (aref b 6))
                         (* (aref a 5) (aref b 5)) (* (aref a 6) (aref b 4))
                         (* (aref a 7) (aref b 3)) (* (aref a 8) (aref b 2))
                         (* (aref a 9) (aref b 1)) (* (aref a 10) (aref b 0))))
    (setf (aref s 11) (+ (aref c 11) (* (aref a 0) (aref b 11))
                         (* (aref a 1) (aref b 10)) (* (aref a 2) (aref b 9))
                         (* (aref a 3) (aref b 8)) (* (aref a 4) (aref b 7))
                         (* (aref a 5) (aref b 6)) (* (aref a 6) (aref b 5))
                         (* (aref a 7) (aref b 4)) (* (aref a 8) (aref b 3))
                         (* (aref a 9) (aref b 2)) (* (aref a 10) (aref b 1))
                         (* (aref a 11) (aref b 0))))
    (setf (aref s 12) (+ (* (aref a 1) (aref b 11)) (* (aref a 2) (aref b 10))
                         (* (aref a 3) (aref b 9)) (* (aref a 4) (aref b 8))
                         (* (aref a 5) (aref b 7)) (* (aref a 6) (aref b 6))
                         (* (aref a 7) (aref b 5)) (* (aref a 8) (aref b 4))
                         (* (aref a 9) (aref b 3)) (* (aref a 10) (aref b 2))
                         (* (aref a 11) (aref b 1))))
    (setf (aref s 13) (+ (* (aref a 2) (aref b 11)) (* (aref a 3) (aref b 10))
                         (* (aref a 4) (aref b 9)) (* (aref a 5) (aref b 8))
                         (* (aref a 6) (aref b 7)) (* (aref a 7) (aref b 6))
                         (* (aref a 8) (aref b 5)) (* (aref a 9) (aref b 4))
                         (* (aref a 10) (aref b 3)) (* (aref a 11) (aref b 2))))
    (setf (aref s 14) (+ (* (aref a 3) (aref b 11)) (* (aref a 4) (aref b 10))
                         (* (aref a 5) (aref b 9)) (* (aref a 6) (aref b 8))
                         (* (aref a 7) (aref b 7)) (* (aref a 8) (aref b 6))
                         (* (aref a 9) (aref b 5)) (* (aref a 10) (aref b 4))
                         (* (aref a 11) (aref b 3))))
    (setf (aref s 15) (+ (* (aref a 4) (aref b 11)) (* (aref a 5) (aref b 10))
                         (* (aref a 6) (aref b 9)) (* (aref a 7) (aref b 8))
                         (* (aref a 8) (aref b 7)) (* (aref a 9) (aref b 6))
                         (* (aref a 10) (aref b 5)) (* (aref a 11) (aref b 4))))
    (setf (aref s 16) (+ (* (aref a 5) (aref b 11)) (* (aref a 6) (aref b 10))
                         (* (aref a 7) (aref b 9)) (* (aref a 8) (aref b 8))
                         (* (aref a 9) (aref b 7)) (* (aref a 10) (aref b 6))
                         (* (aref a 11) (aref b 5))))
    (setf (aref s 17) (+ (* (aref a 6) (aref b 11)) (* (aref a 7) (aref b 10))
                         (* (aref a 8) (aref b 9)) (* (aref a 9) (aref b 8))
                         (* (aref a 10) (aref b 7)) (* (aref a 11) (aref b 6))))
    (setf (aref s 18) (+ (* (aref a 7) (aref b 11)) (* (aref a 8) (aref b 10))
                         (* (aref a 9) (aref b 9)) (* (aref a 10) (aref b 8))
                         (* (aref a 11) (aref b 7))))
    (setf (aref s 19) (+ (* (aref a 8) (aref b 11)) (* (aref a 9) (aref b 10))
                         (* (aref a 10) (aref b 9)) (* (aref a 11) (aref b 8))))
    (setf (aref s 20) (+ (* (aref a 9) (aref b 11)) (* (aref a 10) (aref b 10))
                         (* (aref a 11) (aref b 9))))
    (setf (aref s 21) (+ (* (aref a 10) (aref b 11))
                         (* (aref a 11) (aref b 10))))
    (setf (aref s 22) (+ (* (aref a 11) (aref b 11))))
    (setf (aref s 23) 0)
    ;; reduce modulo l and encode
    (loop
       :for i :from 0 :to 22 :by 2
       :do (progn
             (setf (aref carry i) (>>> (+ (aref s i) (<<< 1 20)) 21))
             (incf (aref s (1+ i)) (aref carry i))
             (decf (aref s i) (<<< (aref carry i) 21))))
    (loop
       :for i :from 1 :to 21 :by 2
       :do (progn
             (setf (aref carry i) (>>> (+ (aref s i) (<<< 1 20)) 21))
             (incf (aref s (1+ i)) (aref carry i))
             (decf (aref s i) (<<< (aref carry i) 21))))
    (sc-reduce s carry)
    (sc-encode s r)
    r))

(declaim (ftype (function ((simple-array int64) (simple-array int64)) (values))
                sc-reduce))
(defun sc-reduce (s carry)
  "Compute reduction of s modulo l."
  (let ((coeffs (make-array 6 :element-type 'int32 :initial-contents +clist+)))
    (loop
       :for k :from 23 :downto 18
       :do (loop
              :for i :from 0 :to 5
              :for c := (+ i k -12)
              :do (case i
                    ((3 5) (decf (aref s c)
                                 (* (the int32 (aref s k))
                                    (the int32 (aref coeffs i)))))
                    (t (incf (aref s c)
                             (* (the int32 (aref s k))
                                (the int32 (aref coeffs i))))))
              :finally (setf (aref s k) 0)))
    (loop
       :for i :from 6 :to 16 :by 2
       :do (progn
             (setf (aref carry i) (>>> (+ (aref s i) (<<< 1 20)) 21))
             (incf (aref s (1+ i)) (aref carry i))
             (decf (aref s i) (<<< (aref carry i) 21))))
    (loop
       :for i :from 7 :to 15 :by 2
       :do (progn
             (setf (aref carry i) (>>> (+ (aref s i) (<<< 1 20)) 21))
             (incf (aref s (1+ i)) (aref carry i))
             (decf (aref s i) (<<< (aref carry i) 21))))
    (loop
       :for k :from 17 :downto 12
       :do (loop
              :for i :from 0 :to 5
              :for c := (+ i k -12)
              :do (case i
                    ((3 5) (decf (aref s c)
                                 (* (the int32 (aref s k))
                                    (the int32 (aref coeffs i)))))
                    (t (incf (aref s c)
                             (* (the int32 (aref s k))
                                (the int32 (aref coeffs i))))))
              :finally (setf (aref s k) 0)))
    (loop
       :for i :from 0 :to 10 :by 2
       :do (progn
             (setf (aref carry i) (>>> (+ (aref s i) (<<< 1 20)) 21))
             (incf (aref s (1+ i)) (aref carry i))
             (decf (aref s i) (<<< (aref carry i) 21))))
    (loop
       :for i :from 1 :to 11 :by 2
       :do (progn
             (setf (aref carry i) (>>> (+ (aref s i) (<<< 1 20)) 21))
             (incf (aref s (1+ i)) (aref carry i))
             (decf (aref s i) (<<< (aref carry i) 21))))
    (loop
       :for i :from 0 :to 5
       :do (case i
             ((3 5) (decf (aref s i)
                          (* (the int32 (aref s 12))
                             (the int32 (aref coeffs i)))))
             (t (incf (aref s i)
                      (* (the int32 (aref s 12))
                         (the int32 (aref coeffs i))))))
       :finally (setf (aref s 12) 0))
    (loop
       :for i :from 0 :to 11
       :do (progn
             (setf (aref carry i) (>>> (aref s i) 21))
             (incf (aref s (1+ i)) (aref carry i))
             (decf (aref s i) (<<< (aref carry i) 21))))
    (loop
       :for i :from 0 :to 5
       :do (case i
             ((3 5) (decf (aref s i)
                          (* (the int32 (aref s 12))
                             (the int32 (aref coeffs i)))))
             (t (incf (aref s i)
                      (* (the int32 (aref s 12))
                         (the int32 (aref coeffs i))))))
       :finally (setf (aref s 12) 0))
    (loop
       :for i :from 0 :to 10
       :do (progn
             (setf (aref carry i) (>>> (aref s i) 21))
             (incf (aref s (1+ i)) (aref carry i))
             (decf (aref s i) (<<< (aref carry i) 21))))
    (values)))


(declaim (ftype (function ((simple-array int64) scalar) (values)) sc-encode))
(defun sc-encode (s r)
  "Encode expanded representation of scalar into a 32-byte array."
  (setf (aref r 0 )         (>>> (aref s 0)   0))
  (setf (aref r 1 )         (>>> (aref s 0)   8))
  (setf (aref r 2 ) (logior (>>> (aref s 0)  16) (<<< (aref s 1) 5)))
  (setf (aref r 3 )         (>>> (aref s 1)   3))
  (setf (aref r 4 )         (>>> (aref s 1)  11))
  (setf (aref r 5 ) (logior (>>> (aref s 1)  19) (<<< (aref s 2) 2)))
  (setf (aref r 6 )         (>>> (aref s 2)   6))
  (setf (aref r 7 ) (logior (>>> (aref s 2)  14) (<<< (aref s 3) 7)))
  (setf (aref r 8 )         (>>> (aref s 3)   1))
  (setf (aref r 9 )         (>>> (aref s 3)   9))
  (setf (aref r 10) (logior (>>> (aref s 3)  17) (<<< (aref s 4) 4)))
  (setf (aref r 11)         (>>> (aref s 4)   4))
  (setf (aref r 12)         (>>> (aref s 4)  12))
  (setf (aref r 13) (logior (>>> (aref s 4)  20) (<<< (aref s 5) 1)))
  (setf (aref r 14)         (>>> (aref s 5)   7))
  (setf (aref r 15) (logior (>>> (aref s 5)  15) (<<< (aref s 6) 6)))
  (setf (aref r 16)         (>>> (aref s 6)   2))
  (setf (aref r 17)         (>>> (aref s 6)  10))
  (setf (aref r 18) (logior (>>> (aref s 6)  18) (<<< (aref s 7) 3)))
  (setf (aref r 19)         (>>> (aref s 7)   5))
  (setf (aref r 20)         (>>> (aref s 7)  13))
  (setf (aref r 21)         (>>> (aref s 8)   0))
  (setf (aref r 22)         (>>> (aref s 8)   8))
  (setf (aref r 23) (logior (>>> (aref s 8)  16) (<<< (aref s 9) 5)))
  (setf (aref r 24)         (>>> (aref s 9)   3))
  (setf (aref r 25)         (>>> (aref s 9)  11))
  (setf (aref r 26) (logior (>>> (aref s 9)  19) (<<< (aref s 10) 2)))
  (setf (aref r 27)         (>>> (aref s 10)  6))
  (setf (aref r 28) (logior (>>> (aref s 10) 14) (<<< (aref s 11) 7)))
  (setf (aref r 29)         (>>> (aref s 11)  1))
  (setf (aref r 30)         (>>> (aref s 11)  9))
  (setf (aref r 31)         (>>> (aref s 11) 17))
  (values))
