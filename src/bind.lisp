(in-package #:cl-irregsexp)

(defconstant +match-bind-operator-abbreviations+
  '(
    (+ . match-one-or-more)
    (* . match-zero-or-more)
    (progn . progn)
    (:? . try-match)
    (not . match-not)
    (and . match-all)
    (or . match-any)))


(defconstant +match-bind-function-abbreviations+
  '(
    (- . match-element-range)
    (integer . match-integer)
    (unsigned-byte . match-unsigned-integer)
    (fixnum . match-fixnum)
    (space . match-one-whitespace)
    (last . match-end)
    (char . eat)
    (string . eat)))

(defun operator-abbreviation (form)
  (cdr (assoc (first form) +match-bind-operator-abbreviations+)))

(defun function-abbreviation (form)
  (cdr (assoc (first form) +match-bind-function-abbreviations+)))

(define-condition match-failed 
    (error)
  ((match-form :initarg :matching) 
   (target-string :initarg :string))
  (:documentation "Raised when the bindings in a match-bind do not match the target string"))

(defun generate-match-bind (bindings &optional env)
  (let (vars forms)
    (labels ((recurse (bindings)
	       (multiple-value-bind (f v)
		   (generate-match-bind (copy-list bindings) env)
		 (setf vars (append vars v))
		 f)))
      (loop while bindings do
	(let ((form (pop bindings)))
	  (cond ((not form))
		      ((symbolp form)
		       (push form vars)
		       (push `(setf ,form 
				    (match-until-and-eat 
				     (progn ,@(or (recurse (list (pop bindings))) (list `(match-end)))))) forms))
		     ((listp form)
		      (cond ((eq 'quote (first form))
			     (push (second form) forms))
			    ((operator-abbreviation form)
			     (push `(,(operator-abbreviation form) ,@(recurse (rest form))) forms))
			    ((function-abbreviation form)
			     (push `(,(function-abbreviation form) ,@(rest form)) forms))
			    (t (check-type (first form) symbol)
			       (push (list (first form) (third form)) vars)
			       (push `(setf ,(first form) ,@(recurse (list (second form)))) forms))))
			((constantp form env)
			 (push `(literal ,form) forms))
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

