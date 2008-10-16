(in-package #:cl-irregsexp)


(defmacro try-match (&body body)
  (with-unique-names (saved-pos try-match-block)
    `(let ((,saved-pos *pos*))
       (block ,try-match-block
	 (let ((*fail* 
		(lambda() 
		  (setf *pos* ,saved-pos)
		  (return-from ,try-match-block 'match-failed))))
	   ,@body)))))

(with-define-specialized-match-functions
  (defgeneric fl-prefix (first rest))
  (defmethod fl-prefix (first rest)
    (cond ((macro-function first)
	   (let ((a (macroexpand `(,first ,@rest))))
	     (fl-prefix (first a) (rest a))))
	  (t
	   (values nil `(,first ,@rest)))))
  
  (defmethod fl-prefix ((first (eql 'literal)) rest)
    (assert (not (second rest)))
    (cond ((constantp (first rest))
	   (values (force-to-target-sequence (first rest)) nil))
	  (t (call-next-method))))

  (defmacro match-any (&rest rest)
    (multiple-value-bind (prefix options)
	(fl-prefix 'match-any rest)
      (cond (prefix
	     (assert (not options))
	     (generate-constant-literal prefix))
	    (options
	     (assert (not prefix))
	     (with-unique-names (saved-pos match-any-block)
	       `(let ((,saved-pos *pos*))
		  (block ,match-any-block
		    ,@(loop for opt in (butlast rest)
			    collect
			    (with-unique-names (match-any-fail-block)
			      `(block ,match-any-fail-block
				 (let ((*fail* (lambda () (setf *pos* ,saved-pos)
						       (return-from ,match-any-fail-block))))
				   (return-from ,match-any-block ,opt)))))
		    ,@(last options))))))))

  (defmethod fl-prefix ((first (eql 'match-any)) rest)
    (block outer ; because of the type-specialization we cannot return-from fl-prefix
      (let (prefix)
	(loop for m in rest
	      for once = t then nil
	      do (multiple-value-bind (p s)
		     (isolate-fixed-length-prefix (list m))
		   (when once
		     (setf prefix p))
		   (setf prefix (merge-match-any-prefix p prefix))
		   (when (or s (not prefix))
		     (return-from outer (call-next-method)))))
	(values prefix nil))))

  (defun isolate-fixed-length-prefix (matches)
    (let ((forms (flatten-progn-forms matches)) after prefix)
      (loop for f in forms
	    do
	    (cond ( (or after (not (listp f))) (appendf after (list f)))
		  (t (multiple-value-bind (p s)
			 (fl-prefix (first f) (rest f))
		       (appendf prefix (map 'list 'identity p))
		       (when s
			 (appendf after (list s)))))))
      (values prefix after)))

  (defmacro match-until-internal (&rest matches)
    (cond 
      ((or (equalp '((match-end)) matches) (equalp '((progn (match-end))) matches))
       `(values *pos* (length *target*)))
      (t
       (multiple-value-bind 
	     (bm-spec other-matches)
	   (isolate-fixed-length-prefix matches)
	 (with-unique-names (start end restart)
	   `(let ((,start *pos*) (,end *pos*))
	      (tagbody
		 ,restart
		 ,(when bm-spec
			`(setf ,end
			       ,(generate-bm-matcher bm-spec)))
		 ,(when other-matches
			`(match-any 
			  (progn ,@other-matches)
			  (progn (eat 1) (incf ,end) (go ,restart)))))
	      (values ,start ,end)))))))

  (defmacro match-until-and-eat (&rest matches)
    (with-unique-names (start end)
      `(multiple-value-bind (,start ,end) 
	   (match-until-internal ,@matches)
	 (subseq *target* ,start ,end))))


  (defmacro match-until (&rest matches)
    (with-unique-names (start end)
      `(multiple-value-bind (,start ,end)
	   (match-until-internal ,@matches)
	 (declare (ignore ,start))
	 (setf *pos* ,end)
	 (subseq *target* ,start ,end)))))

(defmacro match-all (&rest options)
  (with-unique-names (saved-pos)
    `(let ((,saved-pos *pos*))
       ,@(loop for opt in options collect
	       `(progn (setf *pos* ,saved-pos)
		       ,opt)))))

(defun flatten-progn-forms (matches) 
  (when (eq (first matches) 'progn)
    (setf matches (rest matches)))
  (loop for i in matches
	append 
	(cond
	  ((not i) nil)
	  ((and (listp i) (eq 'progn (first i)))
	   (flatten-progn-forms i))
	  (t
	   (list i)))))


(defun merge-match-any-prefix (a b)
  (when (= (length a) (length b))
    (let (different)
      (map 'nil 
	   (lambda(x y)
	     (flet ((subset-p (superset set)
		      (every (lambda(e) (some (lambda(d) (eql d e)) (force-sequence superset))) (force-sequence set))))
	       (unless (and (subset-p x y) (subset-p y x))
		 (when different
		   (return-from merge-match-any-prefix))
		 (setf different t))))
	   a b))
    (map 'list (lambda (x y)
		 (let (r)
		   (flet ((add (c) (pushnew c r :test 'equal)))
		     (map nil #'add (force-sequence x))
		     (map nil #'add (force-sequence y)))
		   r)) a b)))

(defun-speedy match-remaining ()
  (eat (len-available)))


(defmacro match-not (&rest matches)
  (with-unique-names (match-not-block)
    `(block ,match-not-block 
       (let ((*fail* (lambda() (return-from ,match-not-block))))
	 ,@matches)
       (fail))))

(defun-speedy match-element-range (start-inclusive end-inclusive)
  (let ((s (to-int start-inclusive)) (e (to-int end-inclusive)) (v (to-int (peek-one))))
    (declare (type fixnum s e v))
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

(defmacro match-one-whitespace ()
  `(match-any (literal #\Space) (literal #\Tab) (literal #\Linefeed) (literal #\Return) (literal #\Page)))

(defmacro match-integer (&optional (base 10))
  `(progn
     (let ((sign (if (eql (force-to-target-element-type #\-) (peek-one)) 
		     (progn 
		       (eat-unchecked 1)
		       -1) 
		     1)))
       (* sign (match-unsigned-integer ,base)))))


(defun-speedy match-unsigned-integer (base)
  (let ((val 0) 
	success)
    (declare (type (integer 2 36) base))
    (declare (type unsigned-byte val))
    (labels ((range (a c b &optional (offset 0))
	       (declare (type small-positive-integer offset))
	       (when (<= (to-int a) c (to-int b))
		 (+ offset (- c (to-int a))))))
      (declare (inline range))
      (loop for digit = 
	    (let ((c (to-int (peek-one))))
	      (or (range #\0 c #\9) (range #\A c #\Z 10) (range #\a c #\z 10)))
	    while (and digit (> base digit))
	    do
	    (locally 
		(declare (type (integer 0 35) digit))
	      (setf success t)
	      (setf val (+ digit (* val base)))
	      (eat-unchecked 1))
	    until (zerop (len-available)))
      (unless success
	(fail))
      val)))
