(in-package #:cl-irregsexp)

(deftype byte-vector (&optional (len '*))
  `(vector (unsigned-byte 8) ,len))
(deftype simple-byte-vector (&optional (len '*))
  `(simple-array (unsigned-byte 8) (,len)))


(defun make-byte-vector (len)
  (declare (optimize speed))
  (declare (type (unsigned-byte *) len))
  (make-array len :element-type '(unsigned-byte 8)))

(declaim (inline make-byte-vector))
(declaim (ftype (function ((unsigned-byte *)) simple-byte-vector) make-byte-vector)) 