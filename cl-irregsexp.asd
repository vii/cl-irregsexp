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
				     (:file "opt" :depends-on ("packages"))
				     (:file "macros" :depends-on ("opt"))
				     (:file "byte-vector" :depends-on ("packages" "macros"))
				     (:file "utf8" :depends-on ("macros" "byte-vector"))
				     (:file "force"  :depends-on ("utf8"))
				     (:file "type-specializations" :depends-on ("force"))
				     (:file "harness" :depends-on ("type-specializations" "simplify" "force" "output-code"))
				     (:file "output-code" :depends-on ("simplify"))
				     (:file "ir" :depends-on ("packages"))
				     (:file "simplify" :depends-on ("type-specializations" "ir"))
				     (:file "helpers" :depends-on ("harness"))
				     (:file "bind" :depends-on ("helpers" "harness"))
				     (:file "replace" :depends-on ("bind" "byte-vector"))
				     (:file "split" :depends-on ("bind")))))
  :depends-on (
               :alexandria
               ))
