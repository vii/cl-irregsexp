(in-package #:cl-irregsexp)

(defmacro match-replace-helper ( string &rest match-replacements)
  (with-unique-names (before replacement-text after)
    `(let (,replacement-text)
       (if-match-bind (,before (or 
				,@(loop for (match replacement) in match-replacements
					collect `(progn ,@(if (listp match) match (list match)) '(setf ,replacement-text ,replacement)))
				(last))
			       ,after)
		      ,string
		      (values ,before ,replacement-text ,after)
		      (values nil nil nil)))))

(defun-speedy concat (string seqs)
  (apply 'concatenate `(vector ,(if (arrayp string) (array-element-type string) t)) seqs))

(defmacro match-replace-one (string match-replacements)
  (once-only (string)
    `(concat ,string (multiple-value-list (match-replace-helper ,string ,@match-replacements)))))

(defmacro match-replace-all (string &rest match-replacements)
  (with-unique-names (s b r a)
    `(let ((,s ,string))
       (concat ,s
	       (iter:iter 
		 (multiple-value-bind (,b ,r ,a)
		     (match-replace-helper ,s ,@match-replacements)
		   (unless (zerop (length ,b)) (iter:collect ,b))
		   (unless (zerop (length ,r)) (iter:collect ,r))
		   (iter:until (zerop (length ,a)))
		   (setf ,s ,a)))))))


	  
