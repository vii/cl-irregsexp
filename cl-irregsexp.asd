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
				     (:file "type-specializations" :depends-on ("force"))
				     (:file "harness" :depends-on ("type-specializations" "simplify" "output-code"))
				     (:file "output-code" :depends-on ("simplify"))
				     (:file "ir" :depends-on ("packages"))
				     (:file "simplify" :depends-on ("type-specializations" "ir"))
				     (:file "helpers" :depends-on ("harness"))
				     (:file "bind" :depends-on ("helpers"))
				     (:file "replace" :depends-on ("bind")))))
  :depends-on (
	       :iterate
	       :cl-utilities))


			
