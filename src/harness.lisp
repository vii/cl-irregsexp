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
(declaim (type fixnum *pos*))


(defun-speedy len-available ()
  (- (length *target*) *pos*))
(declaim (ftype (function () fixnum) len-available))

(defun-speedy peek (&optional (len (len-available)))
  (declare (type fixnum len))
  (when (> len (len-available))
    (fail))
  (subseq *target* *pos* (+ *pos* len)))

(defun-speedy peek-one ()
  (when (> 1 (len-available))
    (fail))
  (elt *target* *pos*))

(defun-speedy eat (&optional (len 1))
  (declare (type fixnum len))
  (prog1
      (peek len)
    (eat-unchecked len)))

(defun-speedy eat-unchecked (&optional (len 1))
  (declare (type fixnum len))
  (incf *pos* len))

(defun-speedy force-simple-vector (seq)
  (etypecase seq
    (simple-vector seq)
    (vector
     (make-array (length seq) :element-type (array-element-type seq) :initial-contents seq))
    (sequence
     (make-array (length seq) :initial-contents seq))))

(defun-speedy force-simple-string (seq)
  (etypecase seq
    (simple-string seq)
    (sequence
     (replace (make-string (length seq)) seq))))

(defmacro with-match ( (target &key (on-failure ''match-failed) ) &body body)
  (with-unique-names (match-block)
    (once-only (target)
      `(locally
	   (declare (optimize debug))
	 (block ,match-block
	   (let ((*pos* 0)
	       (*fail* (lambda() (return-from ,match-block ,on-failure))))
	     (declare (type fixnum *pos*))
	     (typecase ,target
	       (string
		(let ((*target* (force-simple-string ,target)))
		  (declare (type simple-string *target*))
		  ,@body))
	       (t
		(let ((*target* (force-simple-vector ,target)))
		  (declare (type simple-vector *target*))
		  ,@body)))))))))
