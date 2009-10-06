(in-package #:cl-irregsexp)

(5am:def-suite utf8 :in :cl-irregsexp)
(5am:in-suite utf8)

#+(and sbcl cl-irregsexp::big-characters-in-strings)
(5am:test string-to-octets 
  (loop for code in '(0 #x7f #x80 #x81 #x400 #x7ff #x800 #x801 #x1000 #xffff #x10000 #x10001 #x10002 #x10000) do
	(let ((string (string (code-char code))))
	  (let ((u8e (utf8-encode string))
		(sto (sb-ext:string-to-octets string :external-format :utf-8)))
	    (5am:is (equalp u8e sto))
	    
	    (let ((u8d (utf8-decode sto))
		  (ots (sb-ext:octets-to-string u8e :external-format :utf-8)))
	      (5am:is (equalp u8d string))
	      (5am:is (equalp ots string)))))))

(defun make-bad-utf8-sequence (code &optional (length 
					       (cond ((> #x80 code) 1)
						     ((> #x800 code) 2)
						     ((> #x10000 code) 3)
						     (t 4))))
  (let ((vec (make-byte-vector length)) (i 0))
	(flet ((out (val)
		 (setf (aref vec i) val)
		 (incf i)))
	  (ecase length
	    (1 (out code))
	    (2
	     (out (logior #xc0 (ash code -6)))
	     (out (logior #x80 (logand code #x3f))))
	    (3
	     (out (logior #xe0 (ash code -12)))
	     (out (logior #x80 (logand (ash code -6) #x3f)))
	     (out (logior #x80 (logand code #x3f))))
	    (4
	     (out (logior #xf0 (ash code -18)))
	     (out (logior #x80 (logand (ash code -12) #x3f)))
	     (out (logior #x80 (logand (ash code -6) #x3f)))
	     (out (logior #x80 (logand code #x3f))))))
	vec))

#+cl-irregsexp::big-characters-in-strings
(5am:test invalid-sequences
  (let ((good-seqs '( (1 #x7f) (2 #x400) (3 #x1000) (4 #x89889))))
    (loop for (len code) in good-seqs do
	  (5am:is (string= (utf8-decode (make-bad-utf8-sequence code len)) (string (code-char code))))))

  (let ((bad-seqs 
	 '( (2 0) (2 #x40) (2 #x7f)
	   (3 #x400) (3 #x7ff)
	   (4 #x800)))
	(invalid-decode (string (code-char #xfffd))))
    (loop for (min-len code) in bad-seqs do
	  (loop for len from min-len upto 4 do
		(let ((dec (utf8-decode (make-bad-utf8-sequence code len))))
		  (5am:is (string= dec invalid-decode)))))
    (5am:is (string= (utf8-decode (force-byte-vector '(#x80))) invalid-decode))))

#+cl-irregsexp::big-characters-in-strings
(5am:test encode-all-character-points
  (loop for n below char-code-limit
	for c = (code-char n)
	when c do 
	(let ((encoded (utf8-encode (string c))))
	  ;; 5am:is is too slow
	  #+sbcl (assert (string= (sb-ext:octets-to-string encoded :external-format :utf-8) (string c)))
	  (assert (string= (utf8-decode encoded) (string c))))))

#+cl-irregsexp::big-characters-in-strings
(5am:test decode-all-character-points
  (loop for n below (min char-code-limit #x200000)
	for c = (code-char n)
	when c do 
	(let ((encoded (make-bad-utf8-sequence n)))
	;; 5am:is is too slow
	  (assert (string= (utf8-decode encoded) (string c))))))

