(cl:defpackage #:cl-irregsexp.system
  (:use #:cl))
(cl:in-package #:cl-irregsexp.system)

(asdf:defsystem #:cl-irregsexp
  :name "cl-irregsexp"
  :author "John Fremlin <john@fremlin.org>"
  :version "prerelease"
  :description "More powerful and prettier way of doing text matching, not using regular expressions"

  :components (
	       (:module :src
			:components (
				     (:file "packages")
				     (:file "macros" :depends-on ("packages"))
				     (:file "byte-vector" :depends-on ("packages"))
				     (:file "utf8" :depends-on ("macros" "byte-vector"))
				     (:file "force"  :depends-on ("utf8"))
				     (:file "harness" :depends-on ("force"))
				     (:file "helpers" :depends-on ("harness" "bm"))
				     (:file "bind" :depends-on ("helpers"))
				     (:file "bm" :depends-on ("harness"))
				     (:file "replace" :depends-on ("bind")))))
  :depends-on (
	       :iterate
	       :cl-utilities))


			
