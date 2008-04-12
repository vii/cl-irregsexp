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
				     (:file "harness" :depends-on ("packages"))
				     (:file "helpers" :depends-on ("harness"))
				     (:file "bind" :depends-on ("helpers"))
				     (:file "replace" :depends-on ("bind")))))

  :depends-on (
	       :cl-utilities
	       :iterate))
			
