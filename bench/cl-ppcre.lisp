#.(macrolet ((r (x) x `,x)) 
    (r
     (let ((*debug-io* (make-string-output-stream)) (*standard-output* (make-string-output-stream)))
       (asdf:operate 'asdf:load-op :cl-ppcre))))

(defun slurp-file (name)
  (with-open-file (s name)
    (let ((buf (make-string (file-length s))))
      (read-sequence buf s)
      buf)))

(defun find-it (buf)
  (declare (optimize speed))
  (cl-ppcre:scan "indecipherable|undecipherable" buf))

(compile 'find-it)
(gc)

(let ((buf (slurp-file "test-data")))
  (let ((len (find-it buf)))
    (let ((start (get-internal-real-time)))
      (loop repeat 1000 do
	    (assert (= len (find-it buf))))
      (let ((end (get-internal-real-time)))
	(format t "~A ~$~&" len (/ (- end start) internal-time-units-per-second))))))

(quit)
