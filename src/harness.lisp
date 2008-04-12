(in-package #:cl-irregsexp)

(defmacro defun-speedy (name lambda-list &body body)
  `(progn
     (defun ,name ,lambda-list
       (declare (optimize speed))
       ,@body)
     (declaim (inline ,name))))

(defvar *fail* (lambda()))
(declaim (type (function () ()) *fail*))

(defun-speedy fail ()
  (funcall *fail*))

(defvar *target* "")
(defvar *pos* 0)
(declaim (type unsigned-byte *pos*))

(defun-speedy len-available ()
  (- (length *target*) *pos*))
(declaim (ftype (function () unsigned-byte) len-available))

(defun-speedy peek (&optional (len (len-available)))
  (declare (type unsigned-byte len))
  (when (> len (len-available))
    (fail))
  (subseq *target* *pos* (+ *pos* len)))

(defun-speedy peek-one ()
  (elt (peek 1) 0))

(defun-speedy eat (&optional (len 1))
  (prog1
      (peek len)
    (incf *pos* len)))

(defmacro with-match ( (target &key (on-failure ''match-failed) ) &body body)
  (with-unique-names (match-block)
    `(block ,match-block
       (let ((*pos* 0)
	     (*target* ,target)
	     (*fail* (lambda() (return-from ,match-block ,on-failure))))
	 ,@body))))

  