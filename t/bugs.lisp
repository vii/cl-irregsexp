(in-package #:cl-irregsexp)

(5am:def-suite bugs :in :cl-irregsexp)
(5am:in-suite bugs)

;;; For small bug testcase
(5am:test last-followed-by-quote
  (5am:is (eq t
	      (let (x)
		(cl-irregsexp:match-bind (or "a" (progn (last) '(setf x t)))
		    ""
		  x)))))