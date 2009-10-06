(in-package #:cl-irregsexp)

(5am:def-suite find :in :cl-irregsexp)
(5am:in-suite find)

(5am:test find-present
  (5am:for-all ((len (5am:gen-integer :max 1000000 :min 0)) (suffix (5am:gen-integer :max 1000 :min 0)))
    (5am:is (= (length (match-bind (before "xaaaaaaaaaaaa")
			   (concatenate 'string (make-string len :initial-element #\a)
					"xaaaaaaaaaaaa"
					(make-string suffix :initial-element #\a))
			 before))
	       len))))

(5am:test find-absent
  (5am:for-all ((prefix (5am:gen-integer :max 1000000 :min 0)))
    (let ((present (if-match-bind (before "xaaaaaaaaaaa")
		       (concatenate 'string (make-string prefix :initial-element #\a)
				    "xaaaaaaaaaa"))))
      (5am:is (not present)))))

(5am:test find-random
  (macrolet ((f (needle)
	       `(5am:for-all ((haystack (5am:gen-string :length (5am:gen-integer :max 100 :min 0))))
		  (let ((present (search ,needle haystack)))
		    (5am:is (equalp present
				    (if-match-bind (before ,needle)
						   haystack)))))))
    (f "qqqq")
    (f "0")
    (f "X")
    (f "rn")))
  
