(in-package #:cl-irregsexp)

(5am:def-suite bind :in :cl-irregsexp)
(5am:in-suite bind)

(5am:test bind-char-range
  (match-bind (no-uppercase (or (last) (- #\A #\Z)))
      "this is a test. But so what."
    (5am:is (equalp no-uppercase "this is a test. "))))

(5am:test bind-http-request-line
  (flet ((r (line)
	   (match-bind (method (+ (space)) url (or (last) (+ (space)))
			       (:? "HTTP/" (version-major (integer) 1) "." (version-minor (integer) 0)))
	       line (list method url version-major version-minor))))
    (5am:is (equalp (r "GET /") (list "GET" "/" 1 0)))
    (5am:is (equalp (r "POST /?this HTTP/1.1") (list "POST" "/?this" 1 1)))))

(5am:test http-header
  (flet ((r (line)
	   (match-bind 
	       (header-name (progn (* (space)) ":") (* (space)) value)
	       line
	     (list header-name value))))
    (5am:is (equalp (r "Connection: close") (list "Connection" "close")))
    (5am:is (equalp (r "Transfer-encoding:     chunked") (list "Transfer-encoding" "chunked")))))
