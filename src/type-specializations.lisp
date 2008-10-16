(in-package #:cl-irregsexp)

(defvar *type-specific-match-functions* '(force))
(defvar *specialized-types* '(simple-byte-vector simple-string simple-vector))

(defun specialized-func-symbol (func type)
  (intern (concatenate 'string (symbol-name func) "-" (symbol-name type)) (symbol-package func)))

(defmacro with-specialized-match-functions ((type) &body body)
  `(macrolet ,(loop for f in *type-specific-match-functions* collect
		    `(,f (&rest args)
			 `(,',(specialized-func-symbol f type) ,@args)))
     (declare (type ,type *target*))
     ,@body))
		
(defmacro with-define-specialized-match-functions (&body match-functions)
  (let ((names 
	 (loop for f in match-functions
	       do (assert (member (first f) '(defmethod defgeneric defmacro defun defun-speedy)))
	       collect (second f))))
    (loop for n in names do (pushnew n *type-specific-match-functions*))
    `(progn
       ,@(loop for type in *specialized-types* collect
	     `(with-specialized-match-functions (,type)
		,@(loop for f in match-functions
			collect (destructuring-bind (def name &rest rest)
				    f		
				  `(,def ,(specialized-func-symbol name type)
				       ,@rest))))))))

