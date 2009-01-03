(in-package #:cl-irregsexp)

(defun concat-sym (&rest args)
  (apply 'concat-sym-from-sym-package (first args) args))

(defun concat-sym-from-sym-package (sym &rest args)
  (apply 'concat-sym-package (symbol-package sym) args))

(defun concat-sym-package (package &rest args)
  (intern (apply 'concatenate 'string (mapcar 'symbol-name args)) 
	  package))

(defmacro read-only-load-time-value (form)
  `(load-time-value ,form t))

(defun load-time-constantp (form &optional env)
  (ignore-errors
    (or (constantp form env) 
	(let ((expansion (macroexpand form env)))
	  (and (listp expansion) (eq 'load-time-value (first expansion)))))))

(defmacro defun-speedy (name lambda-list &body body)
  `(progn
     (declaim (inline ,name))
     (defun ,name ,lambda-list
       (declare (optimize speed))
       ,@body)))

(defmacro defun-careful (name lambda-list &body body)
  `(progn
     (defun ,name ,lambda-list
       (declare (optimize debug safety (speed 0)))
       ,@body)))

(defun defun-consistent-names (name)
  (list (concat-sym name '-consistent-internal) (concat-sym name '-consistent-internal-careful)))

(defmacro defun-consistent (name lambda-list &body body)
  (with-unique-names (env)
    (let* ((names (defun-consistent-names name))
	   (fast (first names))
	   (careful (second names)))
      `(progn
	 (defun-speedy ,fast ,lambda-list
	   ,@body)
	 (defun-careful ,careful ,lambda-list
	   ,@body)
	 #+cmucl (declaim (extensions:constant-function ,fast ,careful))
	 (defmacro ,name (,@lambda-list &environment ,env)
	   (if (and ,@(mapcar (lambda(l) `(load-time-constantp ,l ,env)) lambda-list))
	       `(read-only-load-time-value (,',careful ,,@lambda-list))
	       `(,',fast ,,@lambda-list)))))))

(defmacro declaim-defun-consistent-ftype (name input-type output-type)
  `(progn
     ,@(loop for n in (defun-consistent-names name) collect
	     `(declaim (ftype (function ,input-type ,output-type) ,n)))))

(define-modify-macro appendf (&rest lists) append)

(declaim (inline cdr-assoc))
(defun cdr-assoc (alist key &key (test 'eql))
  (cdr (assoc key alist :test test)))
(define-setf-expander cdr-assoc (place key &key (test ''eql) &environment env)
  (multiple-value-bind (dummies vals newval setter getter)
      (get-setf-expansion place env)
    (with-unique-names (store key-val test-val alist found)
      (values 
       (append dummies (list key-val test-val))
       (append vals (list key test))
       `(,store ,@newval)
       `(let (,@(mapcar 'list dummies vals)
	      (,alist ,getter))
	  (let ((,found (assoc ,key-val ,alist :test ,test-val)))
	    (cond (,found 
		   (setf (cdr ,found) ,store))
		  (t
		   (setf ,(first newval) (acons ,key ,store ,alist))
		   ,setter))
	    ,store))
       `(cdr-assoc ,getter)))))
