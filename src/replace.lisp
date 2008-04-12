(in-package #:cl-irregsexp)

(defmacro match-replace-helper (match-replacements string)
  (with-unique-names (before replacement-text after)
    `(let (,replacement-text)
       (if-match-bind (,before (or 
				,@(loop for (match replacement) in match-replacements
					collect `(progn ,match '(setf ,replacement-text ,replacement)))
				(last))
			       ,after)
		      ,string
		      (values ,before ,replacement-text ,after)
		      (values nil nil nil)))))

(defun-speedy concat (string seqs)
  (apply 'concatenate `(vector ,(array-element-type string)) seqs))

(defmacro match-replace-one (match-replacements string)
  (once-only (string)
    `(concat ,string (multiple-value-list (match-replace-helper ,match-replacements ,string)))))

(defmacro match-replace-all (match-replacements string)
  (with-unique-names (s b r a)
    `(let ((,s ,string))
       (concat ,s
	       (iter:iter 
		 (multiple-value-bind (,b ,r ,a)
		     (match-replace-helper ,match-replacements ,s)
		   (unless (zerop (length ,b)) (iter:collect ,b))
		   (unless (zerop (length ,r)) (iter:collect ,r))
		   (iter:until (zerop (length ,a)))
		   (setf ,s ,a)))))))


	  
