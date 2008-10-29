(in-package #:cl-irregsexp)

(defmacro match-replace-helper ( string &rest match-replacements)
  (with-unique-names (before replacement-text after)
    `(let (,replacement-text)
       (if-match-bind (,before (or 
				,@(loop for (match replacement) in match-replacements
					collect `(progn ,match '(setf ,replacement-text (force-to-target-sequence ,replacement))))
				(last))
			       ,after)
		      ,string
		      (values ,before ,replacement-text ,after)
		      (values nil nil nil)))))

(defun-speedy concat-byte-vector (seqs)
  (let ((len (loop for s in seqs summing (length (the byte-vector s)))))
    (let ((ret (make-byte-vector len)) (i 0))
      (loop for a in seqs do (let ((s (force-simple-byte-vector a))) (replace ret s :start1 i) (incf i (length s))))
      ret)))

(defun-speedy concat-string (seqs)
  (apply 'concatenate 'string seqs))


(defun-speedy concat (string seqs)
  (etypecase string
    (string (concat-string seqs))
    (byte-vector (concat-byte-vector seqs))))

(defmacro match-replace-one (string &rest match-replacements)
  "As match-replace-all but at most one replacement is made"
  (once-only (string)
    `(concat ,string (multiple-value-list (match-replace-helper ,string ,@match-replacements)))))

(defmacro match-replace-all (string &rest match-replacements)
  "For each (match replacment) in MATCH-REPLACEMENTS replace each value of match with the value of replacement in STRING"
  (with-unique-names (s b r a f)
    `(let ((,s ,string))
       (flet ((,f () ; move out of the iter:iter so we can use macrolet without warnings
		(match-replace-helper ,s ,@match-replacements)))
	 (declare (inline ,f))
	 (concat ,s 
		 (iter:iter 
		   (multiple-value-bind (,b ,r ,a)
		       (,f)
		     (unless (zerop (length ,b)) (iter:collect ,b))
		     (unless (zerop (length ,r)) (iter:collect ,r))
		     (iter:until (zerop (length ,a)))
		     (setf ,s ,a))))))))
