(in-package #:cl-irregsexp)

(defun make-occurence-table (bm-spec)
  (let ((table (make-hash-table)) (i 0))
    (labels ((add (c)
	     (typecase c
	       (sequence (map nil #'add c))
	       (t
		(push (to-int i) (gethash (to-int c) table))))))
      (map nil (lambda(c)
		 (add c)
		 (incf i)) bm-spec))
    table))


(defun generate-bm-matcher (bm-spec)
  (with-unique-names (end offset c restart)
    `(let ((,offset *pos*))
      (declare (type fixnum ,offset))
	 (let ((,end (- (length *target*) ,(length bm-spec))))
	   (declare (type fixnum ,end))
	   (locally 
	       (declare (optimize
			 speed
			 (safety 0)
			 (space 0)
			 (debug 0)))
	     (tagbody
		,restart
		(when (> ,offset ,end)
		  (fail))
		,@(loop for i downfrom (1- (length bm-spec)) to 0
			collect
			(flet ((skip-lookup (i element)
				 `(case ,element
				    ,@(loop for k being the hash-keys of (make-occurence-table bm-spec) using (hash-value v)
					    for m = (remove-if-not 'plusp (mapcar (lambda(x) (- i x)) v))
					    when m
					    collect `(,k ,(apply 'min m)))
				    (t ,(1+ i)))))
			`(let ((,c (to-int-target-elt (+ ,offset ,i))))
			   (declare (type fixnum ,c))
;;			   (format t "~D ~D have ~A want ~A ~A ~D~&" ,offset (+ ,offset ,i) (code-char ,c) (code-char ,(to-int (elt bm-spec i))) (= ,c ,(to-int (elt bm-spec i))) ,(skip-lookup i c))
			   (unless (= ,c ,(to-int (elt bm-spec i)))
			     (incf ,offset ,(skip-lookup i c))
			     (go ,restart)))))))
	 (setf *pos* (+ ,offset ,(length bm-spec)))
	 ,offset))))

(defmacro bm-literal (bm-spec)
  (generate-bm-matcher bm-spec))