(cl:defpackage #:cl-irregsexp.bytestrings
  (:export
   #:utf8-encode
   #:utf8-decode
   #:force-string
   #:force-simple-string
   #:force-byte-vector
   #:force-simple-byte-vector
   #:byte-vector-to-string
   #:make-byte-vector
   #:byte-vector
   #:simple-byte-vector))

(cl:defpackage #:cl-irregsexp.utils
  (:export
   #:alist-get
   #:defun-consistent
   #:declaim-defun-consistent-ftype
   #:defun-speedy
   #:defun-careful
   #:concat-sym
   #:concat-sym-from-sym-package
   #:read-only-load-time-value
   #:load-time-constantp))


(cl:defpackage #:cl-irregsexp
  (:use #:common-lisp)
  (:use #:cl-irregsexp.bytestrings #:cl-irregsexp.utils)
  (:import-from #:cl-utilities #:with-unique-names #:once-only)
  (:export 
   #:match-failed
   #:match-bind
   #:match-split
   #:if-match-bind
   #:match-replace-all
   #:match-replace-one))
