(in-package #:cl-irregsexp)

(5am:def-suite replace :in :cl-irregsexp)
(5am:in-suite replace)

(5am:test xml-escape
  (flet ((f (value)
	   (match-replace-all value
			      (#\< "&lt;")
			      (#\> "&gt;")
			      (#\& "&amp;")
			      (#\' "&#39;"))))
    (5am:is (equal (f "") ""))
    (5am:is (equal (f "this") "this"))
    (5am:is (equal (f "<<this") "&lt;&lt;this"))
    (5am:is (equal (f "<<this>>") "&lt;&lt;this&gt;&gt;"))))

