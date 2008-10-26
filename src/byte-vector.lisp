(in-package #:cl-irregsexp)

(deftype byte-vector (&optional (len '*))
  "Vector of unsigned byte 8, often a UTF-8 encoded string"
  `(vector (unsigned-byte 8) ,len))
(deftype simple-byte-vector (&optional (len '*))
  `(simple-array (unsigned-byte 8) (,len)))


(declaim (ftype (function ((unsigned-byte *)) simple-byte-vector) make-byte-vector))
(defun-speedy make-byte-vector (len)
  "Return a simple byte-vector of length LEN"
  (declare (type (unsigned-byte *) len))
  (make-array len :element-type '(unsigned-byte 8)))
