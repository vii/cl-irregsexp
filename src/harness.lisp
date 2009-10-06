(in-package #:cl-irregsexp)

(deftype integer-match-index ()
  `(integer 0 ,(floor most-positive-fixnum 3)))

(defun fail ()
  (error "fail called outside a with-fail"))

(defun certainly-not-returning (statements)
  (loop for statement in statements
	thereis (and (listp statement)
		     (case (first statement)
		       ((error return-from unsafe-return-from) t)))))

(defmacro with-fail (body &body fail-actions)
  (assert fail-actions)
  `(flet ((fail () ,@fail-actions 
		,@(unless (certainly-not-returning fail-actions)
			 `((error "Fail must not return: ~A (in ~A)" ',fail-actions ',body)))))
     (declare (ignorable #'fail) (dynamic-extent #'fail) (inline fail))
     ,body))

(defmacro with-save-restore-pos (&body body)
  (with-unique-names (saved-pos)
    `(let ((,saved-pos pos))
       (declare (dynamic-extent ,saved-pos)
		(type integer-match-index ,saved-pos)
		#+ccl (ignorable ,saved-pos) ; ClozureCL 1.3 will
					     ; sometimes issue a
					     ; spurious warning about
					     ; unused lexical variable
					     ; ,saved-pos
		)
       (flet ((restore-pos () (setf pos ,saved-pos)))
	 (declare (dynamic-extent #'restore-pos) (inline restore-pos))
	 ,@body))))

(defmacro with-match-block (&body body)
  (with-unique-names (match-block restart)
    `(block ,match-block
       (tagbody 
	  ,restart
	  (return-from ,match-block
	    (macrolet ((match-block-restart ()
			 `(locally (declare #.*optimize-unsafe*)
			    (go ,',restart)))
		       (return-from-match-block (&optional value)
			 (once-only (value)
			   `(unsafe-return-from  ,',match-block ,value))))
	      (locally ,@body)))))))

(with-define-specialized-match-functions
  (defmacro force-to-target-sequence (v)
    `(force ,v))

  (defmacro len-available ()
    `(- (length target) pos))

  (defmacro check-len-available (len)
    (once-only (len)
      `(locally
	   (declare (type integer-match-index ,len pos) #.*optimize-unsafe*)
	 (when (> (+ pos ,len) (length target))
	   (fail))
	 (values))))
 
  (defmacro peek (&optional (len '(len-available)))
    (once-only (len)
      `(locally    
	   (declare (type integer-match-index ,len pos))
	 (check-len-available ,len)
	 (target-subseq pos (+ pos ,len)))))

  (defmacro eat (&optional (len 1))
    (once-only (len)
      `(prog1
	   (peek ,len)
	 (eat-unchecked ,len))))

  (defmacro eat-unchecked (&optional (len 1))
    `(locally
	 (declare #.*optimize-unsafe* (type integer-match-index pos))
       (incf pos ,len) (values)))

  (defmacro elt-target (i)
    (once-only (i)
      `(locally
	   (declare #.*optimize-unsafe* (type integer-match-index ,i))
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

  (defmacro target-subseq (start end)
    (once-only (start end)
     `(locally
	  (declare #.*optimize-unsafe* (type integer-match-index ,start ,end))
	(cond ((and (zerop ,start) (= (length target) ,end)) target)
	      (t
	       (subseq target ,start ,end))))))

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
       (declare (type ,type target) (type integer-match-index pos) (dynamic-extent pos) (optimize speed))
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
	     (declare (dynamic-extent #',bv #',s))
	     (etypecase ,target
	       (null (setf ,target "") (,s))
	       (byte-vector (,bv))
	       (string (,s))))
	 ,on-failure))))


