(cl:defpackage #:cl-irregsexp-test.system
  (:use #:cl))
(cl:in-package #:cl-irregsexp-test.system)

(asdf:defsystem #:cl-irregsexp-test
  :name "cl-irregsexp-test"
  :author "John Fremlin <john@fremlin.org>"
  :version "prerelease"
  :description "Tests for cl-irregsexp"

  :components (
	       (:module :t
			:components (
				     (:file "suite")
				     (:file "helpers" :depends-on ("suite"))
				     (:file "bind" :depends-on ("suite"))
				     (:file "find" :depends-on ("suite"))
				     (:file "replace" :depends-on ("suite")))))
  :depends-on (
	       :fiveam
	       :cl-irregsexp))
