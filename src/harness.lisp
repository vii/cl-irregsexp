(in-package #:cl-irregsexp)

(defvar *fail* (lambda()))
					; (declaim (type (function () ()) *fail*))

(deftype small-positive-integer ()
  `(integer 0 ,(floor most-positive-fixnum 3)))

(defun-speedy fail ()
  (funcall *fail*))

(defvar *target* "")
(defvar *pos* 0)
(declaim (type small-positive-integer *pos*))

(defun-speedy len-available ()
  (- (length *target*) *pos*))

(defun-speedy check-len-available (len)
  (declare (type small-positive-integer len *pos*))
  (declare (optimize speed (safety 0)))
  (when (> (the small-positive-integer (+ *pos* len)) (the small-positive-integer (length *target*)))
    (fail))
  (values))

(defun-speedy peek (&optional (len (len-available)))
  (declare (type small-positive-integer len *pos*))
  (check-len-available len)
  (subseq *target* *pos* (+ *pos* len)))

(defun-speedy eat (&optional (len 1))
  (declare (type small-positive-integer len))
  (prog1
      (peek len)
    (eat-unchecked len)))

(defun-speedy eat-unchecked (&optional (len 1))
  (declare (type small-positive-integer len *pos*))
  (incf *pos* len)
  (values))

(defun-speedy elt-target (i)
  (declare (optimize speed (safety 0)))
  (declare (type small-positive-integer i))
  (elt *target* i))

(defun-speedy peek-one-unchecked (&optional (i 0))
  (declare (optimize speed (safety 0)))
  (declare (type small-positive-integer i *pos*))
  (elt-target (+ *pos* i)))

(defun-speedy peek-one (&optional (i 0))
  (check-len-available (1+ i))
  (peek-one-unchecked i))

(with-define-specialized-match-functions
  (defmacro force-to-target-sequence (v)
    `(force ,v)))


(with-define-specialized-match-functions
  (defmacro force-to-target-sequence (v)
    `(force ,v))
  
  (defun force-to-target-element-type (c)
    (let ((s (force-to-target-sequence c)))
      (assert (= 1 (length s)))
      (elt s 0)))

  (defun-speedy dynamic-literal (v)
    (let ((value (force-to-target-sequence v)))
      (check-len-available (length value))
      (loop for i of-type small-positive-integer below (length value)
	    unless (eql (peek-one-unchecked i) (elt value i))
	    do (fail))
      (eat-unchecked (length value))
      (values)))

  (defun generate-constant-literal (value)
    (let ((i -1))
      `(progn
	 (check-len-available ,(length value))
	 ,@(map 'list (lambda(c)
			(let ((possible (map 'list 'identity (force-sequence c))))
			  `(case (peek-one-unchecked ,(incf i))
			     (,possible)
			     (t (fail))))) value)
	 (eat-unchecked ,(length value))
	 (values))))

  (defmacro literal (v)
    (if (constantp v)
	(generate-constant-literal (force-to-target-sequence v))
	`(dynamic-literal ,v))))

(defun-consistent to-int (val)
  (etypecase val
    (fixnum val)
    (character (char-code val))))

(defmacro with-match-env ((type target) &body body)
  (check-type type symbol)
  `(with-specialized-match-functions (,type)
     (let ((*target* (force-to-target-sequence ,target)))
       (declare (type ,type *target*))
       (declare (optimize speed))	 
       ,@body)))

(defmacro with-match ( (target &key on-failure) &body body)
  (with-unique-names (bv s v)
    (once-only (target)
      `(flet ((,bv () ;; use separate flets so poor SBCL does not struggle so much with large matches
		(with-match-env (simple-byte-vector ,target)
		  ,@body))
	      (,s ()
		(with-match-env (simple-string ,target)
		  ,@body))
	      (,v ()
		(with-match-env (simple-vector ,target)
		  ,@body)))
	 (declare (inline ,bv ,s ,v))
	 (let ((*pos* 0)
	       (*fail* (lambda() ,on-failure (values))))
	   (declare (type small-positive-integer *pos*))
	   
	   (typecase ,target
	     (byte-vector (,bv))
	     (string (,s))
	     (t (,v))))))))
