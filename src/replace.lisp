(in-package #:cl-irregsexp)

(defun replace-concatenate-sequences (prototype total-length sequences)
  (declare (optimize speed) (type array sequences) 
	   (type array prototype) 
	   (type fixnum total-length))
  (cond ((= 1 (length sequences))
	 (aref sequences 0))
	(t
	 (let ((result 
		(make-array 
		 total-length 
		 :element-type (array-element-type prototype))) 
	       (i 0))
	   (declare (type fixnum i))
	   (macrolet ((gen (&rest types)
			`(etypecase prototype
			     ,@(loop for type in types collect
				     `(,type 
				       (loop for s across sequences
					     do 
					     (let ((s (,(concat-sym 'force-simple- type) (the ,type s))))
					       (declare (type ,(concat-sym 'simple- type) s result) 
							#.*optimize-unsafe*)
					       (replace result s :start1 i)
					       (incf i (length s)))))))))
	     (gen byte-vector string))
	   (values
	    result
	    (floor (length result) 2))))))

(defmacro match-replace-helper (string &key op after result match-replacements)
  (with-unique-names (start before results-len add)
    `(let ((,start 0) (,before 0) (,results-len 0))
       (declare (type integer-match-index ,start ,before)
		(type fixnum ,results-len)
	       (dynamic-extent ,start ,before ,results-len))
	 (flet ((,add (x) 
		  (let ((len (length x))) 
		    (unless (zerop len) 
		      (incf ,results-len len) 
		      (vector-push-extend x ,result)))))
	   (declare (inline ,add) (dynamic-extent #',add))
       (match-bind 
	  (,op '(setf ,start pos)
	       t
	       (or ,@(loop for (match replacement) in match-replacements
			   collect 
			   `(progn
			      ,match
			      '(,add (target-subseq ,start (match-until-start)))
			      '(,add (force-to-target-sequence ,replacement))))
		     (progn (last) '(,add (target-subseq ,start pos))))
	       ,@(when after `('(,add ,after))))
	  ,string)
       (replace-concatenate-sequences
	,string
	,results-len
	,result)))))
	

(defmacro match-replace-one (string &body match-replacements)
  "As match-replace-all but at most one replacement is made"
  (with-unique-names (result)
    (once-only (string)
      `(let ((,result (make-array 3 :fill-pointer 0 :adjustable nil)) )
	 (declare (dynamic-extent ,result))
	 (match-replace-helper ,string :result ,result :op progn :after (match-remaining) :match-replacements ,match-replacements)))))

(defmacro match-replace-all (string &body match-replacements)
  "For each (match replacment) in MATCH-REPLACEMENTS replace each value of match with the value of replacement in STRING"
  (with-unique-names (result)
    (once-only (string)
      `(let ((,result (make-array (length ,string) :fill-pointer 0 :adjustable t)))
	 (declare (dynamic-extent ,result))
	 (match-replace-helper ,string :result ,result :op * :match-replacements ,match-replacements)))))

