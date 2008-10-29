(in-package #:cl-irregsexp)

(defstruct const
  value)

(defstruct choice
  list)

(defstruct (seq (:copier copy-irregsexp-seq))
  list)

(defstruct simplifier
  name
  lambda)

(defstruct path
  (prefix nil)
  (after nil))

(defstruct decider
  paths
  end
  differing-point)

(defstruct match-end)
