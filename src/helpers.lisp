(in-package #:cl-irregsexp)

(defun-speedy to-int (val)
  (etypecase val
    (integer val)
    (character (char-code val))))

(defun-speedy force-sequence (val)
  (etypecase val
    (sequence val)
    (character (string val))
    (number (vector val))))

(defun-speedy literal (v)
  (let ((value (force-sequence v)))
    (let ((target (peek (length value))))
      (loop for i from (1- (length value)) downto 0 
	    unless (= (to-int (elt target i)) (to-int (elt value i)))
	    do (fail)))
    (eat (length value))
    value))

(defmacro try-match (&body body)
  (with-unique-names (saved-pos try-match-block)
    `(let ((,saved-pos *pos*))
       (block ,try-match-block
	 (let ((*fail* 
		(lambda() 
		  (setf *pos* ,saved-pos)
		  (return-from ,try-match-block 'match-failed))))
	   ,@body)))))

(defmacro match-any (&rest options)
  (when options
    (with-unique-names (saved-pos match-any-block)
      `(let ((,saved-pos *pos*))
	 (block ,match-any-block
	   ,@(loop for opt in (butlast options)
		   collect
		   (with-unique-names (match-any-fail-block)
		     `(block ,match-any-fail-block
			(let ((*fail* (lambda () (setf *pos* ,saved-pos)
					 (return-from ,match-any-fail-block))))
			  (return-from ,match-any-block ,opt)))))
	   ,@(last options))))))

(defmacro match-all (&rest options)
  (with-unique-names (saved-pos)
    `(let ((,saved-pos *pos*))
       ,@(loop for opt in options collect
	       `(progn (setf *pos* ,saved-pos)
		       ,opt)))))

(defmacro match-until-internal (&rest matches)
  (cond 
    ((or (equalp '((match-end)) matches) (equalp '((progn (match-end))) matches))
     `(match-remaining))
    (t
     (with-unique-names (start end)
       `(let ((,start *pos*) (,end *pos*))
	  (loop while 
		(eq 'match-until-continue 
		    (match-any 
		     ,@matches
		     (progn (eat 1) (incf ,end) 'match-until-continue))))
	  (values (subseq *target* ,start ,end) ,start ,end))))))

(defmacro match-until-and-eat (&rest matches)
  `(prog1 (match-until-internal ,@matches)))

(defun-speedy match-remaining ()
  (eat (len-available)))

(defmacro match-until (&rest matches)
  (with-unique-names (ret start end)
    `(multiple-value-bind (,ret ,start ,end)
	 (match-until-internal ,@matches)
       (declare (ignore ,start))
       (setf *pos* ,end)
       ,ret)))

(defmacro match-not (&rest matches)
  (with-unique-names (match-not-block)
    `(block ,match-not-block 
       (let ((*fail* (lambda() (return-from ,match-not-block))))
	 ,@matches)
       (fail))))


(defun-speedy match-element-range (start-inclusive end-inclusive)
  (let ((s (to-int start-inclusive)) (e (to-int end-inclusive)) (v (to-int (peek-one))))
    (unless (>= e v s)
      (fail))
    (eat 1)))

(defmacro match-multiple ( (&optional min (optional-extra t)) &rest matches)
  (with-unique-names (start)
    `(let ((,start *pos*))
       ,(when min
	      `(loop for i below ,min
		     do ,@matches))
       ,(case optional-extra
	      ((nil))
	      ((t) `(loop for p = *pos* do (try-match ,@matches) until (= p *pos*)))
	      (otherwise `(loop for i below ,optional-extra 
				for p = *pos* do (try-match ,@matches) until (= p *pos*))))
       (subseq *target* ,start *pos*))))


(defmacro match-one-or-more (&rest matches)
  `(match-multiple (1) ,@matches))

(defmacro match-zero-or-more (&rest matches)
  `(match-multiple () ,@matches))

(defun-speedy match-end ()
  (unless (zerop (len-available))
    (fail)))

(defun-speedy match-one-whitespace ()
  (match-any (literal #\Space) (literal #\Tab) (literal #\Linefeed) (literal #\Return) (literal #\Page)))

(defun match-integer (&optional (base 10))
  (let ((sign (if (= (to-int #\-) (to-int (peek-one))) (progn (eat 1) -1) 1)) (val 0) success)
    (declare (type (member 1 -1) sign))
    (declare (type (integer 2 36) base))
    (declare (type unsigned-byte val))
    (flet ((range (a c b &optional (offset 0))
	     (when (<= (to-int a) (to-int c) (to-int b))
	       (+ offset (- (to-int c) (to-int a))))))
      (loop for digit = 
	    (let ((c (to-int (peek-one))))
	      (or (range #\0 c #\9) (range #\A c #\Z 10) (range #\a c #\z 10)))
	    while (and digit (> base digit))
	    do
	    (setf success t)
	    (setf val (+ digit (* val base) ))
	    (eat 1)
	    until (zerop (len-available)))
      (unless success
	(fail))
      (* val sign))))
