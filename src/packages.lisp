(cl:defpackage #:cl-irregsexp
  (:use #:common-lisp)
  (:import-from #:cl-utilities #:with-unique-names #:once-only)
  (:export 
   #:match-bind
   #:if-match-bind
   #:match-replace-all
   #:match-replace-one))
