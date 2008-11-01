(in-package #:cl-irregsexp)

(defmacro try-match (&body body)
  (with-unique-names (try-match-block)
    `(with-save-restore-pos
       (block ,try-match-block
	 (with-fail
	     (progn
	       ,@body)
	   (restore-pos)
	   (return-from ,try-match-block 'match-failed))))))


(with-define-specialized-match-functions
  (defmacro match-until-internal (&rest matches)
    (output-match-until-code (simplify-seq matches)))
  
  (defmacro match-until-and-eat (&rest matches)
    `(subseq target pos (match-until-internal ,@matches))))

(defmacro match-all (&rest options)
  `(with-save-restore-pos
       ,@(loop for opt in options collect
	       `(progn (restore-pos)
		       ,opt))))

(defsimplifier match-until (&rest matches)
  `(setf pos (match-until-internal ,@matches)))

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

(defsimplifier match-element-range (start-inclusive end-inclusive)
  (assert (constantp start-inclusive))
  (assert (constantp end-inclusive))
  `(match-any ,@(loop for i from (to-int start-inclusive) upto (to-int end-inclusive) collect
		      `(literal ,(code-char i)))))

(with-define-specialized-match-functions
  (defmacro match-remaining ()
    `(eat (len-available))))

(defmacro match-not (&rest matches)
  `(with-match-block
       (with-fail
	   (progn
	     ,@matches)
	 (return-from-match-block))
       (fail)))




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
	      ((t) `(loop for ,p = pos do (try-match ,@matches) until (= ,p pos)))
	      (otherwise `(loop for ,i below ,optional-extra 
				for ,p = pos do (try-match ,@matches) until (= ,p pos)))))))


(defsimplifier match-one-or-more (&rest matches)
  `(match-multiple (1) ,@matches))

(defsimplifier match-zero-or-more (&rest matches)
  `(match-multiple () ,@matches))


(defsimplifier match-one-whitespace ()
  `(match-any (literal #\Space) (literal #\Tab) (literal #\Linefeed) (literal #\Return) (literal #\Page)))

(defun generate-digit-matcher (base var)
  (declare (type (integer 2 36) base))
  (let ((ranges (sort (remove-if-not (lambda(x) (> base (second x))) `( (,(to-int #\0)  0) (,(to-int #\A)  10) (,(to-int #\a)  10)))
		      '>
		      :key 'first)))
    `(let ((i (to-int ,var)))
       (cond ,@(loop for r in ranges
		     collect `((>= i ,(+ (first r) (- base (second r)))) nil) 
		     collect `((>= i ,(first r)) (- i ,(- (first r) (second r)))))))))

(defun generate-unsigned-matcher (base largest)
  (declare (type (integer 2 36) base))
  (let ((val-type `(integer 0 ,(or largest '*))))
    `(let ((val 0) (start pos))
       (loop for digit of-type (or null (integer 0 ,(1- base))) = ,(generate-digit-matcher base '(peek-one))
	     while digit
	     do 
	     (locally
		 (declare (type (integer 0 ,(1- base)) digit) (optimize speed))
	       (setf val
		     (let ((old-val val))
			 ,(when largest 
				`(declare ,@(when (<= largest most-positive-fixnum) (list `(optimize (safety 0))))
					  (type (integer 0 ,(- (floor largest base) (1- base))) old-val)))
		       (the ,val-type (+ (* old-val ,base) digit))))
	       (eat-unchecked 1))
	     until (zerop (len-available)))
       (when (= pos start)
	 (fail))
       (the ,val-type val))))

(defun generate-integer-matcher (base least largest)
  (declare (type (integer 2 36) base))
  (cond ((or (not least) (minusp least))
    `(if (eql (force-to-target-element-type #\-) (peek-one)) 
	 (progn 
	   (eat-unchecked 1)
	   (* -1 ,(generate-unsigned-matcher base (when least (- least)))))
	 ,(generate-unsigned-matcher base largest)))
	(t
	 (generate-unsigned-matcher base largest))))

(defsimplifier match-end ()
  (make-match-end))


(with-define-specialized-match-functions
  (defmacro match-integer (&optional (base 10))
    (generate-integer-matcher base nil nil))

  (defmacro match-unsigned-integer (&optional (base 10))
    (generate-unsigned-matcher base nil))

  (defmacro match-fixnum (&optional (base 10))
    ;; most-negative-fixnum is unfortunately usually 1+ most-positive-fixnum which is annoying but most people don't care
    (generate-integer-matcher base (- most-positive-fixnum) most-positive-fixnum))

  (defmacro match-positive-fixnum (&optional (base 10))
    (generate-unsigned-matcher base most-positive-fixnum)))
