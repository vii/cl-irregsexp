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
    (defmacro match-until-internal (&rest matches)
      (output-match-until-code (simplify-seq matches)))

  (defmacro match-until-and-eat (&rest matches)
    `(subseq *target* *pos* (match-until-internal ,@matches))))

(defmacro match-all (&rest options)
  (with-unique-names (saved-pos)
    `(let ((,saved-pos *pos*))
       ,@(loop for opt in options collect
	       `(progn (setf *pos* ,saved-pos)
		       ,opt)))))

(defsimplifier match-until (&rest matches)
  `(setf *pos* (match-until-internal ,@matches)))

(defsimplifier match-any (&body body)
  (let ((choices 
	 (loop for form in body
	       for choice = (simplify form)
	       if (choice-p choice)
	       append (choice-list choice)
	       else
	       collect choice)))
    (cond ((not choices))
	  ((not (rest choices))
	   (first choices))
	  (t
	   (make-choice :list choices)))))

(defsimplifier literal (value)
  (cond ((constantp value)
	 (make-const :value (force-to-target-sequence value)))
	(t `(dynamic-literal ,value))))


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


(defsimplifier match-multiple ((&optional min (optional-extra t)) &rest matches)
  (with-unique-names (i p)
    `(progn
       ,(cond ((not min))
	      ((constantp min)
	       `(progn
		  ,@(loop for i below min append matches)))
	      (t
	       `(dotimes ((,i ,min))
		  ,@matches)))
       ,(case optional-extra
	      ((nil))
	      ((t) `(loop for ,p = *pos* do (try-match ,@matches) until (= ,p *pos*)))
	      (otherwise `(loop for ,i below ,optional-extra 
				for ,p = *pos* do (try-match ,@matches) until (= ,p *pos*)))))))


(defsimplifier match-one-or-more (&rest matches)
  `(match-multiple (1) ,@matches))

(defsimplifier match-zero-or-more (&rest matches)
  `(match-multiple () ,@matches))

(defun-speedy match-end ()
  (unless (zerop (len-available))
    (fail)))

(defsimplifier match-one-whitespace ()
  `(match-any (literal #\Space) (literal #\Tab) (literal #\Linefeed) (literal #\Return) (literal #\Page)))

(with-define-specialized-match-functions
  (defun-speedy match-integer (&optional (base 10))
    (let ((sign (if (eql (force-to-target-element-type #\-) (peek-one)) 
		    (progn 
		      (eat-unchecked 1)
		      -1) 
		    1)))
      (* sign (match-unsigned-integer base)))))


(defun-speedy match-unsigned-integer (&optional (base 10))
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
