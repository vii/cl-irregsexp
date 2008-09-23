(cl:defpackage #:cl-irregsexp-bytestrings
  (:export
   #:force-string
   #:force-simple-string
   #:force-byte-vector
   #:force-simple-byte-vector
   #:byte-vector-to-string
   #:make-byte-vector
   #:byte-vector
   #:simple-byte-vector))

(cl:defpackage #:cl-irregsexp
  (:use #:common-lisp)
  (:use #:cl-irregsexp-bytestrings)
  (:import-from #:cl-utilities #:with-unique-names #:once-only)
  (:export 
   #:match-failed
   #:match-bind
   #:if-match-bind
   #:match-replace-all
   #:match-replace-one))
