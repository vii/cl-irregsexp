(in-package #:cl-irregsexp)

(defun simplifier-run (simplifier form)
  (funcall (simplifier-lambda simplifier) form))

(defun output-simplified (form)
  (output-code (simplify form)))

(defmacro defsimplifier (name lambda-list &body body)
  `(with-define-specialized-match-functions
     (defsimplifier-go ,name ,lambda-list ,@body)))

(defmacro defsimplifier-go (name lambda-list &body body)
  (with-unique-names (form whole unused)
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute) 
	 (setf (get ',name 'simplifier)
	       (make-simplifier :name ',name
				:lambda
				(lambda(,form)
				  (destructuring-bind
					,lambda-list
				      (rest ,form)
				    ,@body)))))
       ,(when (eq (symbol-package name) (symbol-package 'simplifier))
	      `(defmacro ,name (&whole ,whole &rest ,unused)
		 (declare (ignore ,unused))
		 (with-unique-names (saved-pos)
		   `(let ((,saved-pos pos))
		      ,(output-simplified ,whole)
		      (subseq target ,saved-pos pos)))))
       ',name)))

(defun simplify (form)
  (cond ((and (listp form) (symbolp (first form)))
	 (let ((simplifier (get (first form) 'simplifier)))
	   (cond 
	     (simplifier
	      (simplify (simplifier-run simplifier form)))
	     (t form))))
	(t form)))

(defun simplify-seq (body)
  (when body
    (reduce 'merge-simplify (rest body) :initial-value (simplify (first body)))))

(defsimplifier-go progn (&body body)
  (simplify-seq body))


(defgeneric merge-simplify (before after))
(defmethod merge-simplify (before after)
  (make-seq :list (list before after)))
(defmethod merge-simplify ((before const) (after const))
  (make-const :value (concatenate 'list (const-value before) (const-value after))))

(defmethod merge-simplify ((b seq) (a seq))
  (make-seq :list (append (seq-list b) (seq-list a))))
(defmethod merge-simplify (a (b seq))
  (make-seq :list (list* a (seq-list b))))
(defmethod merge-simplify ((a seq) b)
  (make-seq :list (append (seq-list a) (list b))))

(defmethod merge-simplify ((a choice) b)
  (make-choice :list 
	       (loop for c in (choice-list a)
		     collect (merge-simplify c b))))

(defgeneric output-code (matcher))
(defmethod output-code (matcher)
  matcher)
(defmethod output-code ((matcher seq))
  `(progn ,@(mapcar 'output-code (seq-list matcher))))
(defmethod output-code ((matcher const))
  (output-code (make-choice :list (list matcher))))

(defgeneric output-match-until-code (matcher))
(defmethod output-match-until-code (matcher)
  (output-match-until-code (make-choice :list (list matcher))))






(defun path-after-code (path)
  (output-code (simplify-seq (path-after path))))

(defun path-split (path len)
  (assert (>= (length (path-prefix path)) len))
  (let ((before (subseq (path-prefix path) 0 len))
	(after 
	 (when (/= (length (path-prefix path)) len)
	   (subseq (path-prefix path) len))))
    (when after
      (push (make-const :value after) (path-after path)))

    (setf (path-prefix path) before)
    path))



(defun decider-len (decider)
  (loop for p in (decider-paths decider) maximizing (length (path-prefix p))))

(defun decider-possible (decider i)
  (reduce (lambda(&optional x y) (union x y :test 'eql)) 
	  (map 'list (lambda(p) (force-list (elt (path-prefix p) i))) (decider-paths decider))))

(defun maybe-build-fast-decider (paths end)
  ;; if all the path prefices are the same except for at one point
  (let (point (len (loop for p in paths
			 minimizing (length (path-prefix p)))))
    (labels ((set-eq (a b)
	       (not (set-exclusive-or (force-list a) (force-list b) :test 'eql)))
	     (differing-point (a b)
	       (let ((i 0))
		 (map 'nil 
		      (lambda(x y)
			(unless (or (>= i len) (set-eq x y))
			  (cond 
			    ((and point (not (eql point i)))
			     (setf len (max point i))
			     (setf point (min point i)))
			    (t
			     (setf point i))))
			(incf i))
		      a b))))
      (let ((prefices (map 'list 'path-prefix paths)))
	(mapc (lambda(p)(differing-point p (first prefices))) (rest prefices))
	(when point
	  (let (already)
	    (loop for p in prefices
		  do (map nil 
			  (lambda(c)
			    (when (member c already :test 'eql)
			      (return-from maybe-build-fast-decider))
			    (push c already))
			  (force-sequence (elt p point))))))

	(unless (zerop len)
	  (loop for p in paths
		do (path-split p len))
	  (make-decider :paths paths :differing-point point :end end))))))

(defgeneric split-choice-prefix (c))
(defmethod split-choice-prefix (c)
  (values c nil))
(defmethod split-choice-prefix ((c seq))
  (values (first (seq-list c)) (rest (seq-list c))))

(defun choice-to-fast-decider (choice)
  (let (paths (end '(fail)))
    (loop for c in (choice-list choice)
	  do (multiple-value-bind (first rest)
		 (split-choice-prefix c)
	       (cond 
		 ((match-end-p first)
		  (setf end rest))
		 ((const-p first)
		  (push (make-path :prefix (const-value first) :after rest) paths))
		 (t
		  (push (make-path :after c) paths)))))
    (setf paths (nreverse paths))
    (maybe-build-fast-decider paths end)))
