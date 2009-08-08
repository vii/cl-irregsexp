(in-package #:cl-irregsexp)

(declaim (inline utf8-char-length))
(declaim (ftype (function (character) (integer 1 4)) utf8-char-length))
(defun utf8-char-length (char)
  (declare (type character char))
  (let ((code (char-code char)))
    (cond ((> #x80 code) 1)
	  ((> #x800 code) 2)
	  ((> #x10000 code) 3)
	  (t 4))))

(defun utf8-encode-really (string)
  (declare (optimize speed))
  (declare (type simple-string string))
  (let ((dest-len
	 (loop for c across string summing (utf8-char-length c) of-type fixnum)))
    (let ((vec (make-byte-vector dest-len)))
      (let ((i 0))
	(declare (type fixnum i))
	(flet ((out (val)
		 (setf (aref vec i) val)
		 (incf i)))
	  (loop for c across string
		do
	      (let ((code (logand (char-code c) (1- #x200000))))
		(cond ((> #x80 code) (out code))
		      ((> #x800 code)
		       (out (logior #xc0 (ash code -6)))
		       (out (logior #x80 (logand code #x3f))))
		      ((> #x10000 code)
		       (out (logior #xe0 (ash code -12)))
		       (out (logior #x80 (logand (ash code -6) #x3f)))
		       (out (logior #x80 (logand code #x3f))))
		      (t
		       (out (logior #xf0 (ash code -18)))
		       (out (logior #x80 (logand (ash code -12) #x3f)))
		       (out (logior #x80 (logand (ash code -6) #x3f)))
		       (out (logior #x80 (logand code #x3f)))))))))
      vec)))

'big-characters-in-strings

#.(progn
    (when (eql #x100 (ignore-errors (char-code (code-char #x100))))
      (pushnew 'big-characters-in-strings *features*)
      `(pushnew 'big-characters-in-strings *features*)))

(declaim-defun-consistent-ftype utf8-decode (simple-byte-vector) string)
(declaim-defun-consistent-ftype utf8-encode (simple-string) simple-byte-vector)
		       
#+cl-irregsexp::big-characters-in-strings      
(defun-consistent utf8-encode (str)
  (declare (type simple-string str))
  (declare (optimize speed))
  (let ((vec (make-byte-vector (length str))))
    (loop for i fixnum from 0 for s across str do
	  (let ((c (char-code s)))
	    (when (<= #x80 c)
	      (setf vec (utf8-encode-really str))
	      (return))
	    (setf (aref vec i) c)))
    vec))

(declaim (ftype (function (simple-byte-vector) string) utf8-decode-really))
(defun utf8-decode-really (vec)
					; decode a byte-vector in UTF8 to a string 
					; any bad characters or characters trying to hide in overly long sequences are encoded as char (code-char #xfffd)
  (declare (type simple-byte-vector vec))
  (let ((str (make-string (length vec))))
    (block decode
      (let ((i 0) (j 0) (len (length vec)))
	(declare (type fixnum i j len))
	(labels
	    ((invalid ()
	       (code-char #xfffd))
	     (done? ()
	       (when (>= i len)
		 (setf str (subseq str 0 j))
		 (return-from decode)))
	     (inc ()
	       (incf i)
	       (done?))
	     (eat ()
	       (let ((c (aref vec i)) (val 0))
		 (declare (type fixnum val))
		 (flet ((start (x)
			    (setf val (logand c (lognot x))))
			  (next ()
			    (inc)
			    (setf val (logior (ash val 6) (logand #x3f (aref vec i)))))
			(safe (s)
			  (if s s (invalid))))
		   (declare (inline start next safe) (ignorable #'safe))
		   (macrolet ((limit (min max)
				(let* ((code-char
					(if (loop for c from min below (min max char-code-limit)
						  thereis (not (code-char c)))
					    `(safe (code-char (the char-code val)))
					    `(code-char (the char-code val))))
				       (body
					`(if (>= val ,min)
					    ,code-char
					    (invalid))))
				  (if (> max char-code-limit)
				      `(if (> char-code-limit val)
					   ,body
					   (invalid))
				      body))))
		    (cond ((> #x80 c)
			   (code-char c))
			  ((> #xc0 c) (invalid))
			  ((> #xe0 c) (start #xc0) (next) (limit #x80 #x800))
			  ((> #xf0 c)
			   (start #xe0) (next)(next) (limit #x800 #x10000))
			  (t
			   (start #xf0) (next)(next)(next) (limit #x10000 #x200000))
			  ))))))
	  (done?)
	  (loop 
		do
		(setf (schar str j) (eat))
		(incf j)
		(inc)))))
    str))



#+cl-irregsexp::big-characters-in-strings      
(defun-consistent utf8-decode (vec)
  (declare (type simple-byte-vector vec))
  (values (utf8-decode-really vec)))


#-cl-irregsexp::big-characters-in-strings      
(defun-consistent utf8-decode (vec)
  (map 'string 'code-char vec))

#-cl-irregsexp::big-characters-in-strings      
(defun-consistent utf8-encode (string)
  (map 'byte-vector 'char-code string))



