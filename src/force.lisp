(in-package #:cl-irregsexp)

(declaim-defun-consistent-ftype byte-vector-to-simple-byte-vector 
				((and byte-vector 
				      #-sbcl ; type inference gets confused 
				      (not simple-byte-vector))) simple-byte-vector)

(defun-consistent byte-vector-to-simple-byte-vector (val)
  (declare (type (and byte-vector 
		      #-sbcl ; type inference gets confused
		      (not simple-byte-vector)
		      ) val))
  (let ((ret (make-byte-vector (length val))))
    (replace ret val)
    ret))



(declaim-defun-consistent-ftype force-string (t) simple-string)
(defun-consistent force-string (val)
  "Return a representation of VAL as a string, doing the work at compile-time if possible."
  (let ((str
	 (the string
	   (typecase val
	     (null "")
	     (symbol (symbol-name val))
	     (string val)
	     (simple-byte-vector (utf8-decode val))
	     (byte-vector 
		(utf8-decode (byte-vector-to-simple-byte-vector 
			      val)))
	     (t  (with-standard-io-syntax (princ-to-string val)))))))
    (etypecase str
      (simple-string str)
      (string 
       (locally 
	 (declare (type (and string (not simple-string)) str))
	 (replace (make-string (length str)) str))))))




(declaim-defun-consistent-ftype force-byte-vector (t) byte-vector)
(defun-consistent force-byte-vector (val)
  "Return a representation of VAL as a UTF-8 byte-vector, doing the work at compile-time if possible."
  (typecase val
    (null #.(make-byte-vector 0))
    (simple-string (utf8-encode val))
    (string (utf8-encode val))
    (character (utf8-encode (string val)))
    (byte-vector val)
    (sequence (map 'byte-vector 'identity val))
    (t (utf8-encode (force-string val)))))



(declaim-defun-consistent-ftype force-simple-byte-vector  (t) simple-byte-vector)
(defun-consistent force-simple-byte-vector (val)
  "Return a representation of VAL as a UTF-8 simple byte-vector, doing the work at compile-time if possible."
  (let ((val (force-byte-vector val)))
    (etypecase val
      (simple-byte-vector val)
      (byte-vector 
       (locally
         (declare (type (and byte-vector (not simple-byte-vector)) val))
	 (byte-vector-to-simple-byte-vector val))))))



(defun-consistent byte-vector-to-string (vec)
  "UTF-8 decode VEC to a string"
  (utf8-decode (force-simple-byte-vector vec)))

(declaim-defun-consistent-ftype force-simple-string (t) simple-string)
(defun-consistent force-simple-string (val)
  "Return a representation of VAL as a string, doing the work at compile-time if possible."
  (declare (optimize speed (safety 0)))
  (let ((val (force-string val)))
    (etypecase val
      (simple-string val)
      (string
       (replace (make-string (length val)) val)))))

(defun-speedy force-sequence (val)
  (typecase val
    (sequence val)
    (character (string val))
    (number (vector val))
    (t (list val))))


(defun-consistent force-simple-vector (v)
  (let ((seq (force-sequence v)))
    (etypecase seq
      (simple-vector seq)
      (vector
       (make-array (length seq) :element-type (array-element-type seq) :initial-contents seq))
      (sequence
       (make-array (length seq) :initial-contents seq)))))


(defun-speedy force-list (val)
  (map 'list 'identity (force-sequence val)))
