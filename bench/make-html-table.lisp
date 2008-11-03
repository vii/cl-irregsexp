(in-package :cl-user)

(require 'cl-irregsexp)

(defun slurp-file (name)
  (with-open-file (s name)
    (let ((buf (make-string (file-length s))))
      (read-sequence buf s)
      buf)))

(let ((table (make-hash-table :test 'equalp)))
  (loop for file in (directory "*.table") do
	(cl-irregsexp:match-bind ((* name (+ (space) ) (pos (integer)) (+ (space)) (time (float)) (* (space))
				     '(push time (gethash name table))) (last))
	    (slurp-file file)))
  (let ((max-time (apply 'max (loop for k being the hash-keys of table using (hash-value v) append v))))
    (loop for k in (sort (loop for k being the hash-keys of table collect k) '> :key (lambda(k)(apply 'max (gethash k table)))) do
	  (loop for name = k then "-"
		for time in (gethash k table) do
		(format t "<tr><td>~A</td><td><span class=bar style=\"width: ~$%\">.</span>~$</td></tr>~&"
			name (/ (* 60 time) max-time) time)))))
	  