(in-package #:cl-irregsexp)

(deftype integer-match-index ()
  `(integer 0 ,(floor most-positive-fixnum 3)))

(defmacro fail ()
  (error "fail called outside a with-fail"))

(defmacro with-fail (body &body fail-actions)
  (assert fail-actions)
  `(flet ((fail () ,@fail-actions (error "Fail must not return: ~A (in ~A)" ',fail-actions ',body)))
     (declare (ftype (function nil nil) fail))
     ,body))

(defmacro with-save-restore-pos (&body body)
  (with-unique-names (saved-pos)
    `(let ((,saved-pos pos))
       (flet ((restore-pos () (setf pos ,saved-pos)))
	 ,@body))))

(defmacro with-match-block (&body body)
  (with-unique-names (match-block restart)
    `(block ,match-block
       (tagbody 
	  ,restart
	  (return-from ,match-block
	    (macrolet ((match-block-restart ()
			 `(locally (declare (optimize speed (safety 0)))
			    (go ,',restart)))
		       (return-from-match-block (&optional value)
			 (once-only (value)
			   `(locally (declare (optimize speed (safety 0)))
			      (return-from  ,',match-block ,value)))))
	      (locally ,@body)))))))

(with-define-specialized-match-functions
  (defmacro force-to-target-sequence (v)
    `(force ,v))

  (defmacro len-available ()
    `(- (length target) pos))

  (defmacro check-len-available (len)
    (once-only (len)
      `(locally
	   (declare (type integer-match-index ,len pos) (optimize speed (safety 0)))
	 (when (> (+ pos ,len) (length target))
	   (fail))
	 (values))))
 
  (defmacro peek (&optional (len '(len-available)))
    (once-only (len)
      `(locally    
	   (declare (type integer-match-index ,len pos))
	 (check-len-available ,len)
	 (subseq target pos (+ pos ,len)))))

  (defmacro eat (&optional (len 1))
    (once-only (len)
      `(prog1
	   (peek ,len)
	 (eat-unchecked ,len))))

  (defmacro eat-unchecked (&optional (len 1))
    `(locally
	 (declare (optimize speed (safety 0)) (type integer-match-index pos))
       (incf pos ,len) (values)))

  (defmacro elt-target (i)
    (once-only (i)
      `(locally
	   (declare (optimize speed (safety 0)) (type integer-match-index ,i))
	 (elt target ,i))))

  (defmacro peek-one-unchecked (&optional (i 0))
    `(elt-target (+ pos ,i)))

  (defmacro peek-one (&optional (i 0))
    (once-only (i)
    `(progn
       (check-len-available (1+ ,i))
       (peek-one-unchecked ,i))))

  (defun-consistent force-to-target-element-type (c)
    (let ((s (force-to-target-sequence c)))
      (assert (= 1 (length s)))
      (elt s 0)))

  (defmacro dynamic-literal (v)
    `(let ((value (force-to-target-sequence ,v)))
       (check-len-available (length value))
       (loop for i of-type integer-match-index below (length value)
	     unless (eql (peek-one-unchecked i) (elt value i))
	     do (fail))
       (eat-unchecked (length value))
       (values))))

(defun-consistent to-int (val)
  (etypecase val
    (fixnum val)
    (character (char-code val))))

(defmacro with-match-env ((type target) &body body)
  (check-type type symbol)
  `(with-specialized-match-functions (,type)
     (let ((target (force-to-target-sequence ,target)) (pos 0))
       (declare (type ,type target) (type integer-match-index pos) (optimize speed))
       ,(output-code (simplify-seq body)))))

(defmacro with-match ( (target &key (on-failure '(error 'match-failed))) &body body)
  (with-unique-names (bv s)
    (once-only (target)
      `(with-fail
	   (flet ((,bv () ;; use separate flets so poor SBCL does not struggle so much with large matches
		    (with-match-env (simple-byte-vector ,target)
		      ,@body))
		  (,s ()
		    (with-match-env (simple-string ,target)
		    ,@body)))
	     (etypecase ,target
	       (byte-vector (,bv))
	       (string (,s))))
	 ,on-failure))))
