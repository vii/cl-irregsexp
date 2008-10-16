(in-package #:cl-irregsexp)

(defmacro read-only-load-time-value (form)
  `(load-time-value ,form t))

(defun load-time-constantp (form &optional env)
  (ignore-errors
    (or (constantp form env) 
	(let ((expansion (macroexpand form env)))
	  (and (listp expansion) (eq 'load-time-value (first expansion)))))))

(defmacro defun-consistent (name lambda-list &body body)
  (with-unique-names (env)
    (let ((real-function (intern (concatenate 'string (symbol-name name) (symbol-name '-consistent-internal)))))
      `(progn
	 (defun ,real-function ,lambda-list
	   ,@body)
	 (declaim (inline ,real-function))
	 #+cmucl (declaim (extensions:constant-function ,real-function))
	 (defmacro ,name (,@lambda-list &environment ,env)
	   (if (and ,@(mapcar (lambda(l) `(load-time-constantp ,l ,env)) lambda-list))
	       `(read-only-load-time-value (,',real-function ,,@lambda-list))
	       `(,',real-function ,,@lambda-list)))))))

(defmacro defun-speedy (name lambda-list &body body)
  `(progn
     (defun ,name ,lambda-list
       (declare (optimize speed))
       ,@body)
     (declaim (inline ,name))
     ',name))

(define-modify-macro appendf (&rest lists) append)
