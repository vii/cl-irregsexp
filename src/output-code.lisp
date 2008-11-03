(in-package #:cl-irregsexp)

(defmethod output-code ((match-end match-end))
  (declare (ignore match-end))
  `(unless (zerop (len-available))
     (fail)))

(defmethod output-match-until-code ((match-end match-end))
  (declare (ignore match-end))
  `(setf pos (length target)))

(defmethod output-code ((choice choice))
  (let ((decider (choice-to-fast-decider choice)))
    (if decider (output-code decider)
	(choice-output-code choice))))

(defun choice-output-code (choice)
  (let ((options (choice-list choice)))
    (cond ((not (rest options))
	   (output-code (first options)))
	  (t
	   (with-unique-names (choice-block)
	     `(with-save-restore-pos
		(block ,choice-block
		,@(loop for o in (butlast options) collect
			`(with-match-block
			   (with-fail
			       (return-from ,choice-block ,(output-code o))
			     (return-from-match-block)))
			collect
			`(restore-pos))
		,(output-code (first (last options))))))))))

(defun choice-output-match-until-code (choice)
  (with-unique-names (end)
    `(with-match-block
       (let ((,end pos))
	 (with-fail
	     (progn
	       ,(choice-output-code choice)
	       ,end)
	   (setf pos ,end)
	   (eat 1)
	   (match-block-restart))))))

(defmethod output-match-until-code ((choice choice))
  (let ((decider (choice-to-fast-decider choice)))
    (if decider 
	(output-match-until-code decider)
	(choice-output-match-until-code choice))))

(defun output-finish-match-code (decider)
  (flet ((code (&optional p)
	   `(progn
	      (eat-unchecked ,(decider-len decider))
	      ,(when p (path-after-code p)))))
    (cond 
      ((decider-differing-point decider)
       (let ((i (decider-differing-point decider))
	     (codes (make-hash-table :test 'equal)))
	 (loop for p in (decider-paths decider) do (push p (gethash (code p) codes)))
	 `(case (peek-one-unchecked ,i)
	    ,@(loop for k being the hash-keys of codes using (hash-value v)
		    collect `(,(loop for p in v collect (elt (path-prefix p) i))
			       ,k))
	    (t (fail)))))
      ((not (rest (decider-paths decider)))
       (code (first (decider-paths decider))))
      (t (code)))))


(defmethod output-code ((decider decider))
  `(cond ((> ,(decider-len decider) (len-available))
	  ,(decider-end decider))
	 (t
	  ,@(loop for i below (decider-len decider)
		  unless (eql (decider-differing-point decider) i)
		  collect
		  `(case (peek-one-unchecked ,i)
		     (,(decider-possible decider i))
		     (t (fail))))
	  ,(output-finish-match-code decider))))

(defun decider-skip-table (decider)
  (let ((table (make-hash-table)) (i 0))
    (labels ((add (c)
	       (typecase c
		 (sequence (map nil #'add c))
		 (t
		  (push i (gethash c table))))))
      (mapc (lambda(p)
	      (map nil (lambda(c)
			 (add c)
			 (incf i)) (path-prefix p))) (decider-paths decider)))
    table))

(defmethod output-match-until-code ((decider decider))
  (with-unique-names (end)
    `(with-match-block
	 (let ((,end pos))
	   (cond ((> ,(decider-len decider) (len-available))
		  ,(decider-end decider))
		 (t
		  ,@(let ((table (decider-skip-table decider)))
			 (flet ((skip-list-cases (i possible)
				   (loop for k being the hash-keys of table using (hash-value v)
					 for m = (remove-if-not 'plusp (mapcar (lambda(x) (- i x)) v))
					 when (and m (not (member k possible :test 'eql)))
					 collect
					 `(,k (eat-unchecked ,(apply 'min m))
					      (match-block-restart)))))
			    (loop for i downfrom (1- (decider-len decider)) to 0
				  unless (eql (decider-differing-point decider) i)
				  collect
				  (let ((possible (decider-possible decider i)))
				    `(case (peek-one-unchecked ,i)
				       (,possible)
				       ,@(skip-list-cases i possible)
				       (t (eat-unchecked ,(1+ i)) (match-block-restart)))))))
		   (with-fail
		       ,(output-finish-match-code decider)
		     (setf pos (1+ ,end)) 
		     (match-block-restart))))
	   ,end))))
