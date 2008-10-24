(in-package #:cl-irregsexp)

;(5am:def-suite benchmark :in :cl-irregsexp)
;(5am:in-suite benchmark)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:operate 'asdf:load-op 'cl-ppcre))

(defmacro find-irregsexp (needle haystack)
  (with-unique-names (before)
    `(if-match-bind (,before ,needle) ,haystack)))

(defmacro find-ppcre (needle haystack)
  `(cl-ppcre:scan ,needle ,haystack))

(defmacro find-cl (needle haystack)
  `(search ,needle ,haystack))

(defmacro with-report-time (name &body body)
  (with-unique-names (start end)
    `(let ((,start (get-internal-run-time)))
       (locally ,@body)
       (let ((,end (get-internal-run-time)))
	 (format t "~A: ~$ seconds / " ,name (float (/ (- ,end ,start) internal-time-units-per-second)))))))

(defmacro benchmark-find (needle haystack &optional (reps 1000))
  (with-unique-names (present)
    (once-only (haystack)
      (check-type needle string)
      (flet ((report (impl)
	       (with-unique-names (res)
		 `(with-report-time ',impl 
		    (dotimes (i ,reps)
		      (let ((,res (,impl ,needle ,haystack)))
			(assert (eq (not ,res) (not ,present)) (,res ,present))))))))
	`(let ((,present 
		(find-cl ,needle ,haystack)))
	   ,(report 'find-irregsexp)
	   ,(report 'find-ppcre)
	   ,(report 'find-cl))))))
	   
;; Initial results
;;; (benchmark-find "this" (make-string 1000 :initial-element #\a))
; FIND-IRREGSEXP: 1.32 seconds
; FIND-PPCRE: 0.00 seconds
; FIND-CL: 0.04 seconds

;; After BM implementation
; FIND-IRREGSEXP: 0.04 seconds
; FIND-PPCRE: 0.01 seconds
; FIND-CL: 0.04 seconds

(defun match-http-method-irregsexp (line)
  (declare (type simple-byte-vector line))
  (declare (optimize speed))
  (match-bind (method " ") line method))

(defun match-http-method-hand (line)
  (declare (type simple-byte-vector line))
  (declare (optimize speed))
  (let ((i 0))
    (declare (fixnum i))

    (flet ((nonws ()
	     (let ((j i))
	       (loop until (= 32 (elt line i))
		     do (incf i))
	       (make-array `(,(- i j)) :element-type '(unsigned-byte 8) :displaced-to line :displaced-index-offset j))))
      (nonws))))


(defun match-http-line-irregsexp (line)
  (declare (type simple-byte-vector line))
  (declare (optimize speed))
  (match-bind (method (+ (space)) url (+ (space))
		      (:? "HTTP/" (version-major (unsigned-byte) 1) "." (version-minor (unsigned-byte) 0))) line (values method url version-major version-minor)))

(defun match-http-line-hand (line)
  (declare (type simple-byte-vector line))
  (declare (optimize speed))
  (let ((i 0) method url (version-major 1) (version-minor 0))
    (declare (fixnum i))

    (flet ((ws ()
	     (incf i)
	     (loop while (= 32 (elt line i))
		   do (incf i)))
	   (nonws ()
	     (let ((j i))
	       (loop until (= 32 (elt line i))
		     do (incf i))
	       (make-array `(,(- i j)) :element-type '(unsigned-byte 8) :displaced-to line :displaced-index-offset j))))
      (setf method (nonws))
      (ws)
      (setf url (nonws))
      (ws)
      (incf i 5)
      (setf version-major (- (elt line i) (char-code #\0)))
      (incf i 2)
      (setf version-minor (- (elt line i) (char-code #\0))))
    (values method url version-major version-minor)))

    

;; MATCH-HTTP-METHOD-IRREGSEXP: 2.25 seconds / MATCH-HTTP-METHOD-HAND: 0.34 seconds / MATCH-HTTP-LINE-IRREGSEXP: 5.83 seconds / MATCH-HTTP-LINE-HAND: 0.60 seconds / 
;; MATCH-HTTP-METHOD-IRREGSEXP: 0.08 seconds / MATCH-HTTP-METHOD-HAND: 0.32 seconds / MATCH-HTTP-LINE-IRREGSEXP: 0.38 seconds / MATCH-HTTP-LINE-HAND: 0.60 seconds / 


(defmacro benchmark-http-line (&key (line "POST /?this HTTP/1.1") (reps 1000))
  (let* ((line-bv (force-byte-vector line))
	(first-val (match-http-line-hand line-bv)))
    (flet ((report (impl)
	     (with-unique-names (res)
	       `(with-report-time ',impl 
		  (dotimes (i ,reps)
		    (let ((,res (,impl ,line-bv)))
			(assert (= (length ,res) (length ,first-val)) (,res))))))))
	   `(progn
	      ,(report 'match-http-method-irregsexp)
	      ,(report 'match-http-method-hand)
	      ,(report 'match-http-line-irregsexp)
	      ,(report 'match-http-line-hand)))))
