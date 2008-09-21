(in-package #:cl-irregsexp)

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



(defun-speedy to-int (val)
  (etypecase val
    (integer val)
    (character (char-code val))))

(defun-speedy to-int-target-elt (p)
  (to-int (elt *target* p)))

(defmacro force-to-target-sequence (v)
  `(force-sequence ,v))

(defun-speedy literal-slow (v)
  (let ((value (force-to-target-sequence v)))
    (when (> (length value) (len-available))
      (fail))
    (loop for i below (length value)
	  unless (= (to-int-target-elt (+ *pos* i)) (to-int (elt value i)))
	  do (fail))
    (eat-unchecked (length value))))

(defmacro literal (v)
  `(literal-slow ,v))

(defun specialised-func-symbol (func type)
  (intern (concatenate 'string (symbol-name func) "-" (symbol-name type)) #.(package-name *package*)))

(defmacro literal-simple-byte-vector (v)
  (cond ((constantp v)
	 (let ((bv (force-byte-vector v)))
	   `(progn
	      (when (> ,(length bv) (len-available))
		(fail))
	      (unless (and ,@(loop for i below (length bv) collect
				   `(= ,(aref bv i) (to-int-target-elt (+ *pos* ,i)))))
		(fail))
	      (eat-unchecked ,(length bv)))))
	(t `(literal-slow ,v))))

(defmacro to-int-target-elt-simple-byte-vector (i)
  `(aref (the simple-byte-vector *target*) (the fixnum ,i)))


(defmacro literal-simple-string (v)
  (cond ((constantp v)
	 (let ((bv (force-simple-string v)))
	   `(progn
	      (when (> ,(length bv) (len-available))
		(fail))
	      (unless (and ,@(loop for i below (length bv) collect
				   `(= ,(char-code (aref bv i)) (to-int-target-elt (+ *pos* ,i)))))
		(fail))
	      (eat-unchecked ,(length bv)))))
	(t `(literal-slow ,v))))

(defmacro to-int-target-elt-simple-string (i)
  `(char-code (schar *target* (the fixnum ,i))))



(defmacro literal-simple-vector (v)
  (cond ((constantp v)
	 (let ((bv (force-simple-vector v)))
	   `(progn
	      (when (> ,(length bv) (len-available))
		(fail))
	      (unless (and ,@(loop for i below (length bv) collect
				   `(= ,(to-int (aref bv i)) (to-int-target-elt (+ *pos* ,i)))))
		(fail))
	      (eat-unchecked ,(length bv)))))
	(t `(literal-slow ,v))))

(defmacro to-int-target-elt-simple-vector (i)
  `(to-int (svref *target* (the fixnum ,i))))


(defmacro with-match-env ( (type target) &body body)
  (check-type type symbol)
  `(macrolet ((force-to-target-sequence (v)
		`(,',(specialised-func-symbol 'force type) ,v)))
     (macrolet ((to-int-target-elt (v)
		  `(,',(specialised-func-symbol 'to-int-target-elt type) ,v)))
       (macrolet ((literal (v)
		    `(,',(specialised-func-symbol 'literal type) ,v)))
	 (let ((*target* (force-to-target-sequence ,target)))
	   (declare (type ,type *target*))
	   ,@body)))))

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
	       (byte-vector
		(with-match-env (simple-byte-vector ,target)
		  ,@body))
	       (string
		(with-match-env (simple-string ,target)
		  ,@body))
	       (t
		(with-match-env (simple-vector ,target)
		  ,@body)))))))))
