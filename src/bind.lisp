(in-package #:cl-irregsexp)

(defvar *match-bind-operator-abbreviations*
  '(
    (macrolet . error)
    (quote . error)

    (+ . match-one-or-more)
    (* . match-zero-or-more)
    (progn . progn)
    (:? . try-match)
    (not . match-not)
    (and . match-all)
    (or . match-any)))


(defvar *match-bind-function-abbreviations*
  '(
    (- . match-element-range)
    (integer . match-integer)
    (unsigned-byte . match-unsigned-integer)
    (fixnum . match-fixnum)
    (float . match-float)
    (space . match-one-whitespace)
    (last . match-end)
    (char . eat)
    (string . eat)))

(defvar *match-bind-macros* nil)

(defun operator-abbreviation (form)
  (cdr (assoc (first form) *match-bind-operator-abbreviations*)))

(defun function-abbreviation (form)
  (cdr (assoc (first form) *match-bind-function-abbreviations*)))

(defun bind-macro-expander (form)
  (cdr (assoc (first form) *match-bind-macros*)))

(define-condition match-failed 
    (error)
  ((match-form :initarg :matching) 
   (target-string :initarg :string))
  (:documentation "Raised when the bindings in a match-bind do not match the target string"))

(defun set-match-bind-macro (name function)
  (setf (cdr-assoc *match-bind-macros* name)
	function))

(defun match-bind-macro-body (lambda-list body)
  (with-unique-names (args)
    `(lambda (&rest ,args)
       (destructuring-bind ,lambda-list
	   ,args
	 ,@body))))

(defmacro def-match-bind-macro (name lambda-list &body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (set-match-bind-macro ,name
			   ,(match-bind-macro-body lambda-list body))))

(defun generate-match-bind (bindings &optional env)
  (let (vars forms)
    (labels ((recurse (bindings)
	       (multiple-value-bind (f v)
		   (generate-match-bind (copy-list bindings) env)
		 (setf vars (append vars v))
		 f))
	     (add (form)
	       (push form forms)))
      (when (operator-abbreviation bindings)
	(setf bindings (list bindings)))
      (loop while bindings do
	    (let ((form (pop bindings)))
	      (cond ((not form))
		    ((eql t form)
		     (add
		      `(match-until-and-eat 
			(progn ,@(or (recurse (list (pop bindings))) (list `(match-end)))))))
		    ((symbolp form)
		     (push form vars)
		     (add `(setf ,form 
				 (match-until-and-eat 
				  (progn ,@(or (recurse (list (pop bindings))) (list `(match-end))))))))
		    ((listp form)
		     (cond 
		       ((eq 'quote (first form))
			(add (second form)))
		       ((eq 'macrolet (first form))
			(destructuring-bind (bindings &body body)
			    (rest form)
			  (let ((*match-bind-macros* (copy-list *match-bind-macros*)))
			    (loop for binding in bindings do
				  (destructuring-bind
					(name lambda-list &body body)
				      binding
				    (set-match-bind-macro name (eval (match-bind-macro-body lambda-list body)))))
			    (add `(progn ,@(recurse body))))))
		       ((bind-macro-expander form)
			(add `(progn ,@(recurse (apply (bind-macro-expander form) (rest form))))))
		       ((operator-abbreviation form)
			(add `(,(operator-abbreviation form) ,@(recurse (rest form)))))
		       ((function-abbreviation form)
			(add `(,(function-abbreviation form) ,@(rest form))))
		       (t (check-type (first form) symbol)
			  (push (list (first form) (third form)) vars)
			  (add `(setf ,(first form) ,@(recurse (list (second form))))))))
		    ((constantp form env)
		     (add `(literal ,form)))
		    (t (error "Untranslatable form ~A" form))))))
    (values (reverse forms) vars)))

(defmacro match-bind-internal (args-for-with-match bindings &body body &environment env)
  (multiple-value-bind (forms vars)
      (generate-match-bind bindings env)
    `(let ,vars
       (with-match ,args-for-with-match
	 ,@forms)
       (values)
       ,@body)))

(defmacro match-bind (bindings string &body body)
  "Try to parse STRING according to BINDINGS. If successful, execute BODY. Otherwise raise match-failed"
  (once-only (string)
    `(match-bind-internal (,string :on-failure (error 'match-failed :string ,string :matching ',bindings))
	 ,bindings
       ,@body)))

(defmacro if-match-bind (bindings string &optional (then t) (else nil))
  "Perform a match bind, but return ELSE on failure instead of raising match-failed"
  (with-unique-names (if-match-block)
    `(block ,if-match-block
       (match-bind-internal (,string :on-failure (return-from ,if-match-block ,else))
	   ,bindings
	 ,then))))

