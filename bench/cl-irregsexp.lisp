#.(macrolet ((r (x) x `,x)) 
    (r
     (let ((*debug-io* (make-string-output-stream)) (*standard-output* (make-string-output-stream)))
       (asdf:operate 'asdf:load-op :cl-irregsexp))))

(defun slurp-file (name)
  (with-open-file (s name :element-type '(unsigned-byte 8))
    (let ((buf (cl-irregsexp.bytestrings:make-byte-vector (file-length s))))
      (read-sequence buf s)
      buf)))

(defun find-it (buf)
  (declare (optimize speed))
  (cl-irregsexp:match-bind (before (or "indecipherable" "undecipherable"))
      buf
    (length before)))

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
