(in-package #:cl-irregsexp)

(defvar *type-specific-match-functions* '(force))
(defvar *specialized-types* '(simple-byte-vector simple-string))

(defun specialized-func-symbol (func type)
  (intern (concatenate 'string (symbol-name func) "-" (symbol-name type)) (symbol-package func)))

(defun all-specialized-func-symbols (func)
  (loop for type in *specialized-types* collect (specialized-func-symbol func type)))

(defmacro with-specialized-match-functions ((type) &body body)
  (loop for f in *type-specific-match-functions*
	do (setf body (subst (specialized-func-symbol f type) f body))) ; unfortunately cannot just use macrolet any more because of defsimplify
  `(progn
     (macrolet (,@(loop for f in *type-specific-match-functions* 
			collect `(,f (&rest args) `(,',(specialized-func-symbol f type) ,@args))))
       ,@body)))


(defmacro with-define-specialized-match-functions (&body match-functions)
  (let ((names 
	 (loop for f in match-functions
	       do (assert (member (first f) '(defmethod defgeneric defmacro defun defun-speedy defun-consistent defsimplifier-go)))
	       collect (second f))))
    `(progn
       (eval-when (:compile-toplevel :load-toplevel)
	 (loop for n in ',names do (pushnew n *type-specific-match-functions*)))

       ,@(loop for type in *specialized-types* collect
	     `(with-specialized-match-functions (,type)
		,@(loop for f in match-functions
			collect (destructuring-bind (def name &rest rest)
				    f		
				  `(,def ,(specialized-func-symbol name type)
				       ,@rest))))))))




